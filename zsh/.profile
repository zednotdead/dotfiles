export PATH="$HOME/.local/bin/:$PATH"
if [[ -f "$HOME/.cargo/env" ]]; then
    . "$HOME/.cargo/env"
fi

if [[ "/opt/homebrew/bin/fnm" ]]; then 
  . <(fnm env --use-on-cd)
fi

autoload -U +X bashcompinit && bashcompinit
autoload -Uz compinit && compinit
complete -o nospace -C /usr/local/bin/terraform terraform

complete -C /usr/bin/terraform terraform
