fish_add_path $HOME/.local/bin
fish_add_path $HOME/.cargo/bin

if test -f "/home/linuxbrew/.linuxbrew/bin/brew"
  /home/linuxbrew/.linuxbrew/bin/brew shellenv | source
else if test -d /opt/homebrew
  /opt/homebrew/bin/brew shellenv | source
end

set -U fish_greeting ""
set -gx ARCHITECTURE $(uname -m)
set -gx SOPS_AGE_KEY_FILE "$HOME/.config/sops/age/keys.txt"
set -gx EDITOR "nvim"
set -gx GIT_EDITOR "$EDITOR"
set -gx VISUAL "$EDITOR"
set -gx PKG_CONFIG_PATH "/usr/lib/pkgconfig"
set -gx PKG_CONFIG "/usr/bin/pkg-config"

if command -q cargo
  fish_add_path "$HOME/.cargo/bin/"
end

if command -q keychain
  keychain -q --eval id_ed25519 | source
end

if test $ARCHITECTURE = "arm64"
  set -gx PUPPETEER_SKIP_CHROMIUM_DOWNLOAD true
  set -gx PUPPETEER_EXECUTABLE_PATH $(which chromium)
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
#
# if command -q podman
#   set DOCKER_HOST "unix://$(podman info --format '{{.Host.RemoteSocket.Path}}')"
# end

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

if command -q mcfly
  mcfly init fish | source
  if command -q mcfly-fzf
    mcfly-fzf init fish | source
  end
end

source "$HOME/.config/fish/alias.fish"
