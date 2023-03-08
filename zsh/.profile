export $PATH=$PATH:$HOME/.local/bin:$HOME/.cargo:$HOME/.cargo/bin

. "$HOME/.cargo/env"

if [[ -f "$HOME/.cargo/bin/rtx" ]]; then
    . <($HOME/.cargo/bin/rtx env)
fi
