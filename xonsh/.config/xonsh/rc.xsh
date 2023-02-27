$XONTRIBS_AUTOLOAD_DISABLED = {"homebrew", }
xontrib load homebrew

from shutil import which
from pathlib        	import Path
from xonsh.built_ins	import XSH
import subprocess

# Fix for Fedora :////
def get_detyped(self, key):
  return self.detype().get(key)

XSH.env.get_detyped = get_detyped.__get__(XSH.env)

if which('rtx') is not None:
	ctx = XSH.ctx
	rtx_init = subprocess.run(['rtx','activate','xonsh'],capture_output=True,encoding="UTF-8").stdout
	XSH.builtins.execx(rtx_init,'exec',ctx,filename='rtx')

if which('zoxide') is not None:
	execx($(zoxide init xonsh), 'exec', __xonsh__.ctx, filename='zoxide')

cargo_path = Path('~/.cargo/env').expanduser()

if cargo_path.exists():
	source-bash @(cargo_path)

if which('starship'):
	execx($(starship init xonsh))

if which('exa'):
	aliases['ls'] = 'exa'
