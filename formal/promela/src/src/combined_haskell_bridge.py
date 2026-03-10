# SPDX-License-Identifier: BSD-2-Clause

import json
import logging
import subprocess
from pathlib import Path

logger = logging.getLogger(__name__)


class _HaskellBackend:
    def __init__(self, ref_dict, outputLOG, annoteComments, outputSWITCH):
        self.procIds = set()
        exe = Path(__file__).parent / "refine-haskell"
        if not exe.is_file():
            exe = Path("./refine-haskell")
        self.proc = subprocess.Popen(
            [str(exe), "interactive"],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            text=True,
            bufsize=1,
        )
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
        if self.proc is None or self.proc.stdin is None:
            raise RuntimeError("Haskell process is not available")
        self.proc.stdin.write(line + "\n")
        self.proc.stdin.flush()

    def _readline(self):
        if self.proc is None or self.proc.stdout is None:
            raise RuntimeError("Haskell process is not available")
        line = self.proc.stdout.readline()
        if line == "":
            raise RuntimeError("Haskell process exited unexpectedly")
        return line

    def _expect(self, prefix):
        line = self._readline().strip()
        if not line.startswith(prefix):
            raise RuntimeError(line)
        return line

    def setupLanguage(self):
        self._send("COMMAND:setupLanguage")
        self._expect("RESPONSE:SUCCESS:")
        return True

    def collectPIds(self, line):
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
        payload = json.dumps(line)
        self._send(f"COMMAND:refineSPINLine:{payload}")
        self._expect("RESPONSE:SUCCESS:refineSPINLine")
        if line:
            try:
                self.procIds.add(int(line[0]))
            except (ValueError, TypeError):
                pass
        return None

    def test_body(self):
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
    def __init__(self, ref_dict, outputLOG=False, annoteComments=True, outputSWITCH=True):
        self.haskell_cmd = _HaskellBackend(
            ref_dict,
            outputLOG=outputLOG,
            annoteComments=annoteComments,
            outputSWITCH=outputSWITCH,
        )

    @property
    def procIds(self):
        return self.haskell_cmd.procIds

    def setupLanguage(self):
        return self.haskell_cmd.setupLanguage()

    def collectPIds(self, ln):
        return self.haskell_cmd.collectPIds(ln)

    def refineSPINLine_main(self, ln):
        return self.haskell_cmd.refineSPINLine_main(ln)

    def test_body(self):
        return self.haskell_cmd.test_body()
