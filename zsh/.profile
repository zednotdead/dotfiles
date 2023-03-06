export $PATH = $PATH:$HOME/.local/bin:$HOME/.cargo:$HOME/.cargo/bin

if [[ -f "$HOME/.cargo/env" ]]; then
	source "$HOME/.cargo/env"
	. rtx e
fi
