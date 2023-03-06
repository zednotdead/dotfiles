PATH=$PATH:$HOME/.local/bin

if [[ -f "/home/linuxbrew/.linuxbrew/bin/brew" ]]; then
    eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"
fi

if [[ -f "$HOME/.cargo/env" ]]; then
    . $HOME/.cargo/env
fi

if [[ -f "$HOME/.cargo/bin/rtx" ]]; then
    . <(rtx env)
fi

#if [ "$XDG_SESSION_TYPE" = "wayland" ]; then
#    export MOZ_ENABLE_WAYLAND=1
#fi

if (( $+commands[podman] )) then
    export DOCKER_HOST=unix:///run/user/$UID/podman/podman.sock
fi

export PATH
