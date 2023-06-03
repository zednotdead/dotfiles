export PATH=$PATH:$HOME/.local/bin:$HOME/.cargo:$HOME/.cargo/bin

. "$HOME/.cargo/env"

PYTHON_PATH=/Library/Frameworks/Python.framework/Versions/3.11/bin
if [[ -d $PYTHON_PATH ]]; then
	PATH=$PATH:$PYTHON_PATH
fi

if [[ -f "$HOME/.cargo/bin/rtx" ]]; then
    . <($HOME/.cargo/bin/rtx env)
fi


