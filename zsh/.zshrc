#!/usr/bin/env zsh

# Environment variables
export FLYCTL_INSTALL="$HOME/.fly"
export PATH="$FLYCTL_INSTALL/bin:$HOME/.local/bin:$HOME/.config/emacs/bin:$HOME/.deno/bin:$HOME/go/bin:$HOME/bin:$PATH"

export PATH="${KREW_ROOT:-$HOME/.krew}/bin:$PATH"
export PATH="$HOME/.evm/bin:$PATH"
export HISTORY_SUBSTRING_SEARCH_PREFIXED="true"
export SOPS_AGE_KEY_FILE=$HOME/.config/sops/age/keys.txt
export VAULT_ADDR="http://192.168.1.6:8200"

ARCHITECTURE=$(uname -m)

if [[ $ARCHITECTURE = "arm64" ]] then
    export PUPPETEER_SKIP_CHROMIUM_DOWNLOAD=true
    export PUPPETEER_EXECUTABLE_PATH=`which chromium`
fi

if (( $+commands[bob] )) then
    export PATH="$HOME/.local/share/bob/nvim-bin:$PATH"
fi

if (( $+commands[nvim] )) then
    export EDITOR=nvim
fi

if (( $+commands[podman] )) then
    export DOCKER_HOST="unix:///$HOME/.local/share/containers/podman/machine/qemu/podman.sock"
fi

if [ -d "/opt/homebrew" ]; then
    export PATH="/opt/homebrew/opt/libpq/bin:$PATH"
fi

if [ -f "/home/linuxbrew/.linuxbrew/bin/brew" ]; then
    eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"
fi

if [ -d "$HOME/.pulumi/bin" ]; then
    # add Pulumi to the PATH
    export PATH=$PATH:$HOME/.pulumi/bin
fi
# Config

HISTFILE=$HOME/.cache/zsh/.histfile
HISTSIZE=1000
SAVEHIST=10000
fpath=(~/.zfunc $fpath)

# no c-s/c-q output freezing
setopt noflowcontrol
# allow expansion in prompts
setopt prompt_subst # this is default, but set for share_history
setopt append_history
# save each command's beginning timestamp and the duration to the history file
setopt extended_history
# display PID when suspending processes as well
setopt longlistjobs
# try to avoid the 'zsh: no matches found...'
setopt nonomatch
# report the status of backgrounds jobs immediately
setopt notify
# whenever a command completion is attempted, make sure the entire command path
# is hashed first.
setopt hash_list_all
# not just at the end
setopt completeinword
# use zsh style word splitting
setopt noshwordsplit
# allow use of comments in interactive code
setopt interactivecomments
# disable beeping
unsetopt beep

bindkey -e

bindkey '^[[A' history-substring-search-up
bindkey '^[[B' history-substring-search-down
bindkey "\e[1;5C" forward-word
bindkey "\e[1;5D" backward-word
bindkey "^[[H" beginning-of-line
bindkey "^[[F" end-of-line
bindkey "^[[3~" delete-char
bindkey "^[[3;5~" kill-word
bindkey "^H" backward-kill-word

zstyle ':completion:*' completer _expand _complete _ignored _approximate
zstyle :compinstall filename '$HOME/.zshrc'
zstyle :omz:plugins:ssh-agent quiet yes

autoload -Uz compinit
compinit

# Completions

generate-completion() {
    if (( $+commands[$1] )) then
        if [[ ! -f "$HOME/.zfunc/_$1" ]]; then
            echo "$1 completion file not found, generating..."
            eval "$2" > "$HOME/.zfunc/_$1"
            echo "Completion generated! Reloading compinit..."
            compinit
        fi
    fi
}

get-task-completions() {
    curl "https://raw.githubusercontent.com/go-task/task/main/completion/zsh/_task"
}

generate-completion "rustup" "rustup completions zsh"
generate-completion "cargo" "rustup completions zsh cargo"
generate-completion "mise" "mise complete --shell zsh"
generate-completion "docker" "docker completion zsh"
generate-completion "sqlx" "sqlx completions zsh"
generate-completion "gh" "gh completion -s zsh"
generate-completion "op" "op completion zsh"
generate-completion "pulumi" "pulumi gen-completion zsh"
generate-completion "just" "just --completions zsh"
generate-completion "podman" "podman completion zsh"
generate-completion "kubectl" "kubectl completion zsh"
generate-completion "pnpm" "pnpm install-completion zsh"
generate-completion "vault" "vault -autocomplete-install"
generate-completion "flux" "flux completion zsh"
generate-completion "helm" "helm completion zsh"
generate-completion "jj" "jj util completion --zsh"
generate-completion "zellij" "zellij setup --generate-completion zsh"
generate-completion "grype" "grype completion zsh"
generate-completion "syft" "syft completion zsh"
generate-completion "velero" "velero completion zsh"
generate-completion "go-task" "get-task-completions"
generate-completion "bob" "bob complete zsh"
generate-completion "talosctl" "talosctl completion zsh"

# Hooks

if (( $+commands[zoxide] )) then
	eval "$(zoxide init zsh)"
fi

if (( $+commands[mise] )) then
	eval "$(mise activate zsh)"
fi

if (( $+commands[starship] )) then
	eval "$(starship init zsh)"
fi

if (( $+commands[direnv] )) then
	eval "$(direnv hook zsh)"
fi

# Fuzzy git checkout
# by: https://polothy.github.io/post/2019-08-19-fzf-git-checkout/

# Custom aliases

fzf-git-branch() {
    git rev-parse HEAD > /dev/null 2>&1 || return

    git branch --color=always --all --sort=-committerdate |
        grep -v HEAD |
        fzf --height 50% --ansi --no-multi --preview-window right:65% \
            --preview 'git log -n 50 --color=always --date=short --pretty="format:%C(auto)%cd %h%d %s" $(sed "s/.* //" <<< {})' |
        sed "s/.* //"
}

fzf-git-checkout() {
    git rev-parse HEAD > /dev/null 2>&1 || return

    local branch

    branch=$(fzf-git-branch)
    if [[ "$branch" = "" ]]; then
        echo "No branch selected."
        return
    fi

    # If branch name starts with 'remotes/' then it is a remote branch. By
    # using --track and a remote branch name, it is the same as:
    # git checkout -b branchName --track origin/branchName
    if [[ "$branch" = 'remotes/'* ]]; then
        git checkout --track $branch
    else
        git checkout $branch;
    fi
}

alias gch="fzf-git-checkout"

alias fzf-git-get-hash='git log --oneline | fzf | grep -oE "^.{10}"'

grev() {
	git revert $(fzf-git-get-hash)
}

gpuu() {
     git push -u origin $(git branch --show-current)
}

getticketnr() {
    git branch --show-current | perl -lane 'print m/([A-Z]{2,}-\d+)/'
}

clearnode_info() {
    echo "\e[34;49m[INFO]\e[0m $1"
}

clearnode() {
    if [ -d "node_modules" ]; then
        clearnode_info "node_modules exists. Deleting..."
        rm -rf "./node_modules"
    fi
    if [ -f "package-lock.json" ]; then
        clearnode_info "package-lock.json exists. Deleting..."
        rm -rf "./package-lock.json"
    fi
}

git-current-branch() {
    git rev-parse --abbrev-ref HEAD
}

if [[ `uname` = "Linux" ]]; then
    alias copy-to-clipboard="wl-copy"
elif [[ `uname` = "Darwin" ]]; then
    alias copy-to-clipboard="pbcopy"
fi

copy() {
    if [[ -f "$1" ]]; then
        copy-to-clipboard < $1
    else
        copy-to-clipboard $1
    fi
}

ghpr() {
    gh pr view $(git-current-branch) --json url | jq -r .url | copy
}

alias gpu="git push"
alias gput="git push && git push --tags"
alias gpl="git pull"
alias goops="git commit -a --amend --no-edit; git push --force-with-lease"
alias "1f"="onefetch --include-hidden"
alias "cls"="clear"

ulimit -n 65536 65536
ulimit -f unlimited

open() {
    setsid xdg-open $1 2> /dev/null
}

if (( $+commands[paru] )) then
    alias yay="paru"
fi

alias zel="zellij"

if (( $+commands[evcxr] )) then
    alias irust="evcxr"
fi

if (( $+commands[go-task] )) then
    alias task="go-task"
fi

if (( $+commands[docker] )) then
    alias dcup="docker-compose up"
fi

if (( $+commands[npm] )) then
    alias nr="npm run"
fi

if (( $+commands[jaq] )) then
    alias jq="jaq"
fi

if (( $+commands[kdash] )) then
    alias kd="kdash"
fi

if (( $+commands[go-task] )) then
    alias task="go-task"
fi

npm-update() {
    jq -r ".devDependencies | keys[]" package.json | fzf --multi | xargs -I _ npm i -D _@latest
}

sshfzf() {
    ssh $(grep -P '(Host )(?!\*)' ~/.ssh/config | cut -b 6- | fzf --query "$LBUFFER" --height=20)
}

# Loading Antidote

# source antidote
source ${ZDOTDIR:-~}/.antidote/antidote.zsh

# initialize plugins statically with ${ZDOTDIR:-~}/.zsh_plugins.txt
antidote load

# tabtab source for packages
# uninstall by removing these lines
[[ -f ~/.config/tabtab/zsh/__tabtab.zsh ]] && . ~/.config/tabtab/zsh/__tabtab.zsh || true

autoload -U +X bashcompinit && bashcompinit
complete -o nospace -C /usr/bin/vault vault

# pnpm
export PNPM_HOME="$HOME/.local/share/pnpm"
case ":$PATH:" in
  *":$PNPM_HOME:"*) ;;
  *) export PATH="$PNPM_HOME:$PATH" ;;
esac
# pnpm end

if (( $+commands[eza] )) then
    alias ls="eza --icons"
fi

if (( $+commands[mcfly] )) then
    eval "$(mcfly init zsh)"
    if (( $+commands[mcfly-fzf] )) then
        eval "$(mcfly-fzf init zsh)"
    fi
fi

