from shutil import which

if which('rtx') is not None:
	import rtx_config

$HISTCONTROL='ignoredups,erasedups'
$XONSH_HISTORY_BACKEND='sqlite'
