# SPDX-License-Identifier: BSD-2-Clause
"""Combined bridge used by the Python pipeline to talk to Haskell refinement.

This module keeps the public `command` API that the existing generator expects
(`setupLanguage`, `collectPIds`, `refineSPINLine_main`, `test_body`), while
moving the actual refinement implementation into the Haskell executable.

Design intent:
1. Python orchestrates the workflow and owns high-level pipeline integration.
2. Haskell owns parsing/refinement/state transitions for SPIN lines.
3. The wire protocol between both sides stays line-oriented and explicit so
   failures are visible in terminal logs and easy to debug.
"""

import json
import logging
import os
import subprocess
import atexit
from pathlib import Path

logger = logging.getLogger(__name__)

# Backend selector retained for call-site compatibility.
# Current implementation always resolves to Haskell, but callers can still pass
# `backend` / `REFINE_BACKEND` without breaking older integration code.
DEFAULT_BACKEND = "haskell"


class _HaskellBackend:
    """Small client for the `refine_command.hs` interactive text protocol.

    Protocol shape:
    - Python -> Haskell:
      - `INIT JSON:{...}`
      - `COMMAND:setupLanguage`
      - `COMMAND:collectPIds:<pid>`
      - `COMMAND:refineSPINLine:<json-array>`
      - `COMMAND:testBody`
    - Haskell -> Python:
      - `RESPONSE:SUCCESS:...`
      - `RESPONSE:FAILED:...`
      - for test body: `BEGIN` / streamed body lines / `END`
    """

    def __init__(self, ref_dict, outputLOG, annoteComments, outputSWITCH):
        # `procIds` is still exposed to older Python code paths.
        # The source of truth for generated code is now on the Haskell side.
        self.procIds = set()
        self.haskell = None
        # Compatibility properties kept to avoid breaking legacy callers that
        # may inspect these fields; Haskell now returns a final combined body.
        self._defCode = []
        self._declCode = []
        self._testCodes = []

        base = Path(__file__).parent
        # Prefer sibling executables next to this bridge file.
        candidates = [base / "refine-haskell", base / "Main"]
        exe = None
        for path in candidates:
            if path.exists() and path.is_file() and path.stat().st_size > 0 and os.access(path, os.X_OK):
                exe = str(path)
                break
        if exe is None:
            # Preserve previous behavior: let subprocess resolve relative path.
            exe = "./refine-haskell"

        # `bufsize=1` + `text=True` keeps per-line command/response behavior.
        self.haskell = subprocess.Popen(
            [exe, "interactive"],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            text=True,
            bufsize=1
        )
        logger.info("Haskell bridge ready via %s", exe)

        init_data = {
            "ref_dict": ref_dict,
            "flags": {
                "outputLOG": outputLOG,
                "annoteComments": annoteComments,
                "outputSWITCH": outputSWITCH,
            },
        }
        # Initial configuration seed (language map + flags) mirrors what the
        # old pure-Python refiner received in its constructor.
        self._send(f"INIT JSON:{json.dumps(init_data)}")
        self._wait_for_response()
        # Ensure child process is cleaned up even if caller forgets `close()`.
        atexit.register(self.close)

    def _send(self, line):
        """Write one protocol line to the Haskell subprocess."""
        if self.haskell is None or self.haskell.stdin is None:
            raise RuntimeError("Haskell process is not available")
        self.haskell.stdin.write(line + "\n")
        self.haskell.stdin.flush()

    def _readline(self):
        """Read one line from Haskell and fail fast on unexpected EOF."""
        if self.haskell is None or self.haskell.stdout is None:
            raise RuntimeError("Haskell process is not available")
        line = self.haskell.stdout.readline()
        if line == "":
            raise RuntimeError("Haskell process exited unexpectedly")
        return line

    def _wait_for_response(self):
        """Consume output until a terminal SUCCESS/FAILED response line."""
        while True:
            line = self._readline()
            line = line.strip()
            if line.startswith("RESPONSE:SUCCESS:"):
                return line
            if line.startswith("RESPONSE:FAILED:"):
                raise RuntimeError(line)

    def setupLanguage(self):
        """Ask Haskell to apply language-specific token templates."""
        self._send("COMMAND:setupLanguage")
        self._wait_for_response()
        return True

    def collectPIds(self, line):
        """Track participating process IDs via Haskell command response."""
        if not line:
            return None
        pid = int(line[0])
        self._send(f"COMMAND:collectPIds:{pid}")
        response = self._wait_for_response()
        prefix = "RESPONSE:SUCCESS:collectPIds:"
        if response.startswith(prefix):
            payload = response[len(prefix):]
            if payload:
                self.procIds = {int(p) for p in payload.split(",") if p}
            else:
                self.procIds = set()
        return None

    def refineSPINLine_main(self, line):
        """Refine one tokenized SPIN line inside Haskell runtime state."""
        payload = json.dumps(line)
        self._send(f"COMMAND:refineSPINLine:{payload}")
        response = self._wait_for_response()
        if not response.startswith("RESPONSE:SUCCESS:refineSPINLine"):
            raise RuntimeError(response)
        if line:
            try:
                self.procIds.add(int(line[0]))
            except (ValueError, TypeError):
                pass
        return None

    def test_body(self):
        """Request the final emitted C fragments and return raw lines."""
        self._send("COMMAND:testBody")
        begin = self._readline().strip()
        if begin != "RESPONSE:SUCCESS:testBody:BEGIN":
            if begin.startswith("RESPONSE:FAILED:"):
                raise RuntimeError(begin)
            raise RuntimeError(f"Unexpected testBody response: {begin}")
        out = []
        while True:
            line = self._readline()
            if line.strip() == "RESPONSE:SUCCESS:testBody:END":
                break
            out.append(line)
        return out

    @property
    def defCode(self):
        """Compatibility shim: legacy field retained, now usually empty."""
        return self._defCode

    @property
    def declCode(self):
        """Compatibility shim: legacy field retained, now usually empty."""
        return self._declCode

    @property
    def testCodes(self):
        """Compatibility shim: legacy field retained, now usually empty."""
        return self._testCodes

    def __del__(self):
        # Best-effort cleanup for non-deterministic object teardown paths.
        self.close()

    def close(self):
        """Terminate child process without raising if it already exited."""
        proc = getattr(self, "haskell", None)
        if proc is not None:
            try:
                proc.terminate()
                proc.wait(timeout=1)
            except Exception:
                try:
                    proc.kill()
                except Exception:
                    pass
            self.haskell = None


class command:
    """Public façade matching the historical Python refiner class shape.

    Other parts of the generator instantiate `command` and call methods on it.
    Keeping this surface stable lets us swap implementations underneath.
    """

    def __init__(
        self,
        ref_dict,
        outputLOG=False,
        annoteComments=True,
        outputSWITCH=True,
        backend=None,
    ):
        self.ref_dict = ref_dict
        self.outputLOG = outputLOG
        self.annoteComments = annoteComments
        self.outputSWITCH = outputSWITCH

        # Only one active backend now; this field name is kept to minimize
        # downstream diffs in existing call sites.
        self.haskell_cmd = None

        requested = (backend or os.getenv("REFINE_BACKEND", DEFAULT_BACKEND)).lower()
        if requested not in {"auto", "haskell"}:
            raise ValueError("backend must be one of: auto, haskell")

        self.selected_mode = "haskell"

        try:
            # Construct and validate backend eagerly so pipeline errors happen
            # at startup rather than halfway through generation.
            self.haskell_cmd = _HaskellBackend(
                ref_dict,
                outputLOG=outputLOG,
                annoteComments=annoteComments,
                outputSWITCH=outputSWITCH,
            )
            logger.info("Haskell bridge available")
        except Exception as err:
            raise RuntimeError("Requested haskell backend, but startup failed") from err

        logger.info("Refine backend mode: %s", self.selected_mode)

    @property
    def procIds(self):
        """Expose PID set to callers that inspect scheduling participants."""
        return self.haskell_cmd.procIds

    @property
    def defCode(self):
        """Legacy compatibility field (actual code rendering is in Haskell)."""
        return self.haskell_cmd.defCode

    @property
    def declCode(self):
        """Legacy compatibility field (actual code rendering is in Haskell)."""
        return self.haskell_cmd.declCode

    @property
    def testCodes(self):
        """Legacy compatibility field (actual code rendering is in Haskell)."""
        return self.haskell_cmd.testCodes

    def setupLanguage(self):
        """Forward setup call to backend."""
        return self.haskell_cmd.setupLanguage()

    def collectPIds(self, ln):
        """Forward collectPIds call to backend."""
        return self.haskell_cmd.collectPIds(ln)

    def refineSPINLine_main(self, ln):
        """Forward per-line refinement call to backend."""
        return self.haskell_cmd.refineSPINLine_main(ln)

    def test_body(self):
        """Forward final body rendering call to backend."""
        return self.haskell_cmd.test_body()

    def close(self):
        """Explicitly close backend resources."""
        if self.haskell_cmd is not None:
            self.haskell_cmd.close()
