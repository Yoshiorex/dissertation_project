# SPDX-License-Identifier: BSD-2-Clause
"""Hybrid Python-Haskell refinement command with per-method routing."""

import json
import logging
import os
import subprocess
from pathlib import Path

from .retired_files import pure_python_refine_command as py_refine

logger = logging.getLogger(__name__)

# Manual toggle: "auto", "hybrid", "python", or "haskell"
# You can also override with environment variable REFINE_BACKEND.
DEFAULT_BACKEND = "haskell"

# Per-method routing in hybrid mode.
# Change these to move individual functions to Haskell as they reach parity.
HYBRID_METHOD_BACKEND = {
    "setupLanguage": "haskell",
    "collectPIds": "haskell",
    "refineSPINLine_main": "haskell",
    "test_body": "haskell",
    "state": "haskell",
}

class _HaskellBackend:
    """Bridge client for refine_command.hs with Python fallback for unported logic."""

    def __init__(self, ref_dict, outputLOG, annoteComments, outputSWITCH):
        self.procIds = set()
        self.haskell = None
        # Unported refinement logic currently runs here for parity.
        self.py_engine = py_refine.command(
            ref_dict,
            outputLOG=outputLOG,
            annoteComments=annoteComments,
            outputSWITCH=outputSWITCH,
        )

        base = Path(__file__).parent
        candidates = [base / "refine-haskell", base / "Main"]
        exe = None
        for path in candidates:
            if path.exists() and path.is_file() and path.stat().st_size > 0 and os.access(path, os.X_OK):
                exe = str(path)
                break
        if exe is None:
            exe = "./refine-haskell"

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
        self._send(f"INIT JSON:{json.dumps(init_data)}")
        self._wait_for_response()

    def _send(self, line):
        if self.haskell is None or self.haskell.stdin is None:
            raise RuntimeError("Haskell process is not available")
        self.haskell.stdin.write(line + "\n")
        self.haskell.stdin.flush()

    def _wait_for_response(self):
        if self.haskell is None or self.haskell.stdout is None:
            raise RuntimeError("Haskell process is not available")
        while True:
            line = self.haskell.stdout.readline()
            if line == "":
                raise RuntimeError("Haskell process exited unexpectedly")
            line = line.strip()
            if line.startswith("RESPONSE:SUCCESS:"):
                return line
            if line.startswith("RESPONSE:FAILED:"):
                raise RuntimeError(line)

    def setupLanguage(self):
        # Keep both engines configured identically.
        self.py_engine.setupLanguage()
        self._send("COMMAND:setupLanguage")
        self._wait_for_response()
        return True

    def collectPIds(self, line):
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
        self.py_engine.collectPIds(line)
        self.procIds = set(self.py_engine.procIds)
        return None

    def refineSPINLine_main(self, line):
        payload = json.dumps(line)
        self._send(f"COMMAND:refineSPINLine:{payload}")
        response = self._wait_for_response()
        if not response.startswith("RESPONSE:SUCCESS:refineSPINLine"):
            raise RuntimeError(response)
        # Keep Python-side state/test_body in sync for output parity.
        self.py_engine.refineSPINLine_main(line)
        self.procIds = set(self.py_engine.procIds)
        return None

    def test_body(self):
        return self.py_engine.test_body()

    @property
    def defCode(self):
        return self.py_engine.defCode

    @property
    def declCode(self):
        return self.py_engine.declCode

    @property
    def testCodes(self):
        return getattr(self.py_engine, "testCodes", [])

    def __del__(self):
        if getattr(self, "haskell", None) is not None:
            try:
                self.haskell.terminate()
            except Exception:
                pass


class command:
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

        self.python_cmd = py_refine.command(
            ref_dict,
            outputLOG=outputLOG,
            annoteComments=annoteComments,
            outputSWITCH=outputSWITCH,
        )
        self.haskell_cmd = None

        requested = (backend or os.getenv("REFINE_BACKEND", DEFAULT_BACKEND)).lower()
        if requested not in {"auto", "hybrid", "python", "haskell"}:
            raise ValueError("backend must be one of: auto, hybrid, python, haskell")

        # "auto" defaults to hybrid while Haskell parity is incomplete.
        self.selected_mode = "hybrid" if requested == "auto" else requested

        if self.selected_mode in {"haskell", "hybrid"}:
            try:
                self.haskell_cmd = _HaskellBackend(
                    ref_dict,
                    outputLOG=outputLOG,
                    annoteComments=annoteComments,
                    outputSWITCH=outputSWITCH,
                )
                logger.info("Haskell bridge available")
            except Exception as err:
                if self.selected_mode == "haskell":
                    raise RuntimeError("Requested haskell backend, but startup failed") from err
                logger.warning("Haskell unavailable, hybrid will use python-only: %s", err)
                self.haskell_cmd = None

        logger.info("Refine backend mode: %s", self.selected_mode)

    def _backend_for(self, method_name):
        if self.selected_mode == "python":
            return self.python_cmd
        if self.selected_mode == "haskell":
            return self.haskell_cmd if self.haskell_cmd is not None else self.python_cmd
        target = HYBRID_METHOD_BACKEND.get(method_name, "python")
        if target == "haskell" and self.haskell_cmd is not None:
            return self.haskell_cmd
        return self.python_cmd

    @property
    def procIds(self):
        return self._backend_for("state").procIds

    @property
    def defCode(self):
        return self._backend_for("state").defCode

    @property
    def declCode(self):
        return self._backend_for("state").declCode

    @property
    def testCodes(self):
        return getattr(self._backend_for("state"), "testCodes", [])

    def setupLanguage(self):
        return self._backend_for("setupLanguage").setupLanguage()

    def collectPIds(self, ln):
        return self._backend_for("collectPIds").collectPIds(ln)

    def refineSPINLine_main(self, ln):
        return self._backend_for("refineSPINLine_main").refineSPINLine_main(ln)

    def test_body(self):
        return self._backend_for("test_body").test_body()
