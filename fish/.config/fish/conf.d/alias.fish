alias zel="zellij"

if command -q paru
  alias yay="paru"
end

if command -q zellij
  alias zel="zellij"
end

if command -q evcxr
  alias irust="evcxr"
end

# if command -q go-task
#     alias task="go-task"
# end

if command -q docker
    alias dcup="docker-compose up"
end

if command -q npm
    alias nr="npm run"
end

# if command -q jaq
#     alias jq="jaq"
# end

if command -q kdash
    alias kd="kdash"
end

if command -q kubectl
    alias k="kubectl"
end

if command -q nvim
    alias vim="nvim"
end

if command -q kubie
    alias kubens="kubie ns"
    alias kubectx="kubectx"
end

if command -q listenbrainz-cli-tools
    alias lb="listenbrainz-cli-tools"
end

if command -q fastfetch
    alias ffetch="fastfetch"
end

if command -q eza
    alias ls="eza --icons"
end
