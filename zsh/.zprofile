. "$HOME/.profile"

if [[ -f "$HOME/.asdf/asdf.sh" ]]; then
    . $HOME/.asdf/asdf.sh
fi

if [[ -f "/opt/homebrew/opt/asdf/libexec/asdf.sh" ]]; then
    . /opt/homebrew/opt/asdf/libexec/asdf.sh
fi

export PNPM_HOME="/home/zed/.local/share/pnpm"
export PATH="$PNPM_HOME:$PATH"
export PATH="${KREW_ROOT:-$HOME/.krew}/bin:$PATH"
export EDITOR=$HOME/.local/bin/neovide
export XDG_DATA_DIRS="$XDG_DATA_DIRS:/var/lib/flatpak/exports/share:$HOME/.local/share/flatpak/exports/share"

if (( $+commands[flux] )) then
    . <(flux completion zsh)
fi

if [[ -f "/home/linuxbrew/.linuxbrew/bin/brew" ]]; then
    eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"
fi

if [[ -f "/opt/homebrew/bin/brew" ]]; then
    eval "$(/opt/homebrew/bin/brew shellenv)"
fi

if [[ -f "/usr/local/bin/talosctl" ]]; then
    . <(talosctl completion zsh)
fi
if [[ -f "/usr/local/bin/cilium" ]]; then
    . <(cilium completion zsh)
fi

if [[ -f "$(brew --prefix asdf)/libexec/asdf.sh" ]]; then
    . $(brew --prefix asdf)/libexec/asdf.sh
fi

# Setting PATH for Python 3.11
# The original version is saved in .zprofile.pysave
PATH="/Library/Frameworks/Python.framework/Versions/3.11/bin:$HOME/.deno/bin:${PATH}"
export PATH

if [ "$XDG_SESSION_TYPE" = "wayland" ]; then
    export MOZ_ENABLE_WAYLAND=1
fi

if (( $+commands[podman] )) then
  export DOCKER_HOST=unix:///run/user/$UID/podman/podman.sock
fi

# fnm
export PATH="/home/zed/.local/share/fnm:$PATH"
if (( $+commands[fnm] )) then
   eval "`fnm env`"
fi
