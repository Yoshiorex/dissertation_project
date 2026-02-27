# SPDX-License-Identifier: BSD-2-Clause
"""Hybrid Python-Haskell refinement command"""

import subprocess
import tempfile
import os
import json
import logging
from pathlib import Path

logger = logging.getLogger (__name__)
# Try to import Haskell bridge, fall back to pure Python
try:
    from . import refine_haskell_bridge as haskell
    HAS_HASKELL = True
    print("Using Haskell refinement engine")
except ImportError:
    HAS_HASKELL = False
    from . import refine_command_pure_python as pure_python
    print("Using pure Python refinement engine (fallback)")

class command:
    def __init__(self, ref_dict, outputLOG=False, annoteComments=True, outputSWITCH=True):
        self.ref_dict = ref_dict
        self.outputLOG = outputLOG
        self.annoteComments = annoteComments 
        self.outputSWITCH = outputSWITCH
        
        if HAS_HASKELL:
            # Start with Haskell for simple functions
            self.haskell_cmd = haskell.HaskellCommand(ref_dict, outputLOG, annoteComments, outputSWITCH)
            self.python_cmd = None
            print("Haskell refinement engine initialized")
        else:
            # Fallback to pure Python
            self.python_cmd = pure_python.PythonCommand(ref_dict, outputLOG, annoteComments, outputSWITCH)
            self.haskell_cmd = None
            print("Python refinement engine initialized (fallback)")
    
    def setupLanguage(self):
        """Step 1: Replace this simple function first"""
        if self.haskell_cmd:
            return self.haskell_cmd.setupLanguage()
        else:
            return self.python_cmd.setupLanguage()
    
    def collectPIds(self, ln):
        """Step 2: Replace simple state management"""
        if self.haskell_cmd:
            return self.haskell_cmd.collectPIds(ln)
        else:
            return self.python_cmd.collectPIds(ln)
    
    def refineSPINLine_main(self, ln):
        """Step 3: Keep complex logic in Python initially, then break down"""
        if self.haskell_cmd:
            # Start with delegating the whole line processing
            return self.haskell_cmd.refineSPINLine_main(ln)
        else:
            return self.python_cmd.refineSPINLine_main(ln)
    
    def test_body(self):
        """Step 4: Replace output generation last"""
        if self.haskell_cmd:
            return self.haskell_cmd.test_body()
        else:
            return self.python_cmd.test_body()
    
    # Properties for compatibility
    @property
    def procIds(self):
        if self.haskell_cmd:
            return self.haskell_cmd.procIds
        else:
            return self.python_cmd.procIds
    
    @property 
    def defCode(self):
        if self.haskell_cmd:
            return self.haskell_cmd.defCode
        else:
            return self.python_cmd.defCode
    
    @property
    def declCode(self):
        if self.haskell_cmd:
            return self.haskell_cmd.declCode
        else:
            return self.python_cmd.declCode