from pathlib        	import Path
from xonsh.built_ins	import XSH
import subprocess

# Fix for Fedora :////
def get_detyped(self, key):
  return self.detype().get(key)

XSH.env.get_detyped = get_detyped.__get__(XSH.env)


ctx = XSH.ctx
rtx_init = subprocess.run(['rtx','activate','xonsh'],capture_output=True,encoding="UTF-8").stdout
XSH.builtins.execx(rtx_init,'exec',ctx,filename='rtx')
