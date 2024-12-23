fish_add_path $HOME/.local/bin

set ARCHITECTURE $(uname -m)
set SOPS_AGE_KEY_FILE "$HOME/.config/sops/age/keys.txt"

if command -q keychain
  keychain -q --eval id_ed25519 | source
end

if test $ARCHITECTURE = "arm64"
  export PUPPETEER_SKIP_CHROMIUM_DOWNLOAD=true
  export PUPPETEER_EXECUTABLE_PATH=`which chromium`
end

if command -q starship
  starship init fish | source
end

if command -q mise
  mise activate fish | source
end

if command -q bob
  fish_add_path "$HOME/.local/share/bob/nvim-bin"
end

if command -q podman
  set DOCKER_HOST "unix://$(podman info --format '{{.Host.RemoteSocket.Path}}')"
end

if test -d "/opt/homebrew"
  fish_add_path "/opt/homebrew/opt/libpq/bin"
end

if test -f "/home/linuxbrew/.linuxbrew/bin/brew"
  /home/linuxbrew/.linuxbrew/bin/brew shellenv | source
end

if command -q zoxide
  zoxide init fish | source
end

ulimit -n 65536
ulimit -f unlimited

if command -q rye
  set -Ua fish_user_paths "$HOME/.rye/shims"
end

if string match 'polpc.*' $(hostname)
  source "$HOME/.config/fish/work.fish"
end
