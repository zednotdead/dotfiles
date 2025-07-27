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

eval (ssh-agent -c) > /dev/null

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

if command -q rye
  set -Ua fish_user_paths "$HOME/.rye/shims"
end

if string match 'polpc.*' $(hostname)
  source "$HOME/.config/fish/work.fish"
end

function fish_command_not_found
  if command -q command-not-found
    /usr/bin/command-not-found $argv
  end
end

source "$HOME/.config/fish/alias.fish"
