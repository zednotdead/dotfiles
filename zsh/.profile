if [[ -f "$HOME/.cargo/env" ]]; then
  . "$HOME/.cargo/env"
fi

autoload -U +X bashcompinit && bashcompinit
autoload -Uz compinit && compinit
complete -o nospace -C /usr/local/bin/terraform terraform
