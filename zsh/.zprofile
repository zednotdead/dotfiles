. "$HOME/.profile"
. "$HOME/.asdf/asdf.sh"
export PNPM_HOME="/home/zed/.local/share/pnpm"
export PATH="$PNPM_HOME:$PATH"
export PATH="${KREW_ROOT:-$HOME/.krew}/bin:$PATH"
export EDITOR=$HOME/.local/bin/neovide
. <(flux completion zsh)
eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"
