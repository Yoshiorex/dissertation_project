# SPDX-License-Identifier: BSD-2-Clause
"""Lean Python bridge that drives the Haskell refiner subprocess.

This file keeps the historical `command` API used by the generator, but all
refinement work happens inside the Haskell executable. The bridge just:
1. Starts the Haskell process in interactive mode.
2. Sends commands (INIT, refineSPINLine, testBody, ...).
3. Reads responses and exposes the same methods the Python pipeline expects.
"""

import json
import logging
import subprocess
from pathlib import Path

# Module-level logger used by upstream scripts (spin2test.py sets handlers).
logger = logging.getLogger(__name__)


class _HaskellBackend:
    """Client for the line-oriented protocol implemented in refine_command.hs.

    Protocol summary (request -> response):
    - INIT JSON:{...} -> RESPONSE:SUCCESS:...
    - COMMAND:setupLanguage -> RESPONSE:SUCCESS:...
    - COMMAND:collectPIds:<pid> -> RESPONSE:SUCCESS:collectPIds:<csv>
    - COMMAND:refineSPINLine:<json-array> -> RESPONSE:SUCCESS:refineSPINLine
    - COMMAND:testBody -> BEGIN + body lines + END
    """

    def __init__(self, ref_dict, outputLOG, annoteComments, outputSWITCH):
        # Tracks PIDs seen so far for logging/reporting in Python.
        self.procIds = set()

        # Prefer a sibling executable; fall back to current working directory.
        exe = Path(__file__).parent / "refine-haskell"
        if not exe.is_file():
            exe = Path("./refine-haskell")

        # Start the Haskell process in interactive mode with line buffering.
        self.proc = subprocess.Popen(
            [str(exe), "interactive"],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            text=True,
            bufsize=1,
        )

        # INIT config mirrors the old Python refiner constructor arguments.
        init_data = {
            "ref_dict": ref_dict,
            "flags": {
                "outputLOG": outputLOG,
                "annoteComments": annoteComments,
                "outputSWITCH": outputSWITCH,
            },
        }
        self._send(f"INIT JSON:{json.dumps(init_data)}")
        self._expect("RESPONSE:SUCCESS:")

    def _send(self, line):
        """Send one command line to the Haskell subprocess."""
        if self.proc is None or self.proc.stdin is None:
            raise RuntimeError("Haskell process is not available")
        self.proc.stdin.write(line + "\n")
        self.proc.stdin.flush()

    def _readline(self):
        """Read one line from the Haskell subprocess (fail on EOF)."""
        if self.proc is None or self.proc.stdout is None:
            raise RuntimeError("Haskell process is not available")
        line = self.proc.stdout.readline()
        if line == "":
            raise RuntimeError("Haskell process exited unexpectedly")
        return line

    def _expect(self, prefix):
        """Read one response line and ensure it starts with prefix."""
        line = self._readline().strip()
        if not line.startswith(prefix):
            raise RuntimeError(line)
        return line

    def setupLanguage(self):
        """Ask Haskell to apply language-specific templates."""
        self._send("COMMAND:setupLanguage")
        self._expect("RESPONSE:SUCCESS:")
        return True

    def collectPIds(self, line):
        """Register a PID and refresh the local PID set from Haskell."""
        if not line:
            return None
        pid = int(line[0])
        self._send(f"COMMAND:collectPIds:{pid}")
        response = self._expect("RESPONSE:SUCCESS:collectPIds:")
        payload = response[len("RESPONSE:SUCCESS:collectPIds:") :]
        if payload:
            self.procIds = {int(p) for p in payload.split(",") if p}
        else:
            self.procIds = set()
        return None

    def refineSPINLine_main(self, line):
        """Send one tokenized SPIN line to be refined inside Haskell."""
        payload = json.dumps(line)
        self._send(f"COMMAND:refineSPINLine:{payload}")
        self._expect("RESPONSE:SUCCESS:refineSPINLine")

        # Keep local PID set in sync even if collectPIds is not called explicitly.
        if line:
            try:
                self.procIds.add(int(line[0]))
            except (ValueError, TypeError):
                pass
        return None

    def test_body(self):
        """Fetch the full rendered test body (header + segments + footer)."""
        self._send("COMMAND:testBody")
        begin = self._readline().strip()
        if begin != "RESPONSE:SUCCESS:testBody:BEGIN":
            raise RuntimeError(begin)
        out = []
        while True:
            line = self._readline()
            if line.strip() == "RESPONSE:SUCCESS:testBody:END":
                break
            out.append(line)
        return out


class command:
    """Public API used by spin2test/testgen to drive refinement."""

    def __init__(self, ref_dict, outputLOG=False, annoteComments=True, outputSWITCH=True):
        # Only one backend now: Haskell.
        self.haskell_cmd = _HaskellBackend(
            ref_dict,
            outputLOG=outputLOG,
            annoteComments=annoteComments,
            outputSWITCH=outputSWITCH,
        )

    @property
    def procIds(self):
        """Expose PID set for logging and downstream reporting."""
        return self.haskell_cmd.procIds

    def setupLanguage(self):
        """Apply language templates before refinement."""
        return self.haskell_cmd.setupLanguage()

    def collectPIds(self, ln):
        """Forward PID collection to Haskell."""
        return self.haskell_cmd.collectPIds(ln)

    def refineSPINLine_main(self, ln):
        """Forward one tokenized SPIN line to Haskell refiner."""
        return self.haskell_cmd.refineSPINLine_main(ln)

    def test_body(self):
        """Return the final emitted test body lines."""
        return self.haskell_cmd.test_body()
