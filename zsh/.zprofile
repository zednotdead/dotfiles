. "$HOME/.profile"

if [[ -f "$HOME/.asdf/asdf.sh" ]]; then
  . $HOME/.asdf/asdf.sh
fi

export PNPM_HOME="/home/zed/.local/share/pnpm"
export PATH="$PNPM_HOME:$PATH"
export PATH="${KREW_ROOT:-$HOME/.krew}/bin:$PATH"
export EDITOR=$HOME/.local/bin/neovide

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
