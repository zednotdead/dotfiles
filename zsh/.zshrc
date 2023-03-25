#!/usr/bin/env zsh

fpath=($HOME/.zfunc $fpath)

export PATH=$HOME/.local/bin:$HOME/.config/emacs/bin:$HOME/.deno/bin:$HOME/go/bin:$PATH
export PATH="${KREW_ROOT:-$HOME/.krew}/bin:$PATH"
export HISTORY_SUBSTRING_SEARCH_PREFIXED="true"
export SOPS_AGE_KEY_FILE=$HOME/.config/sops/age/keys.txt

zstyle ':completion:*' completer _expand _complete _ignored _approximate
zstyle ':completion:*' group-name ''
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' matcher-list '' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]}' '' 'l:|=* r:|=*'
zstyle ':completion:*' menu select=long
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' use-compctl false
zstyle ':completion:*' verbose true
# disable sort when completing `git checkout`
zstyle ':completion:*:git-checkout:*' sort false
# set descriptions format to enable group support
zstyle ':completion:*:descriptions' format '[%d]'
# set list-colors to enable filename colorizing
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle :compinstall filename '$HOME/.zshrc'

source ${ZDOTDIR:-~}/.antidote/antidote.zsh
antidote load

autoload -U +X bashcompinit && bashcompinit
autoload -Uz compinit && compinit
complete -o nospace -C /usr/bin/terraform terraform

if (( $+commands[flux] )) then
	. <(flux completion zsh)
fi
if (( $+commands[direnv] )) then
	. <(direnv hook zsh)
fi
if (( $+commands[kubectl] )) then
	source <(kubectl completion zsh)
fi
if (( $+commands[helm] )) then
	source <(helm completion zsh)
fi
if (( $+commands[zoxide] )) then
	eval "$(zoxide init zsh)"
fi

HISTFILE=~/.cache/zsh/.histfile
HISTSIZE=2000
SAVEHIST=100000
bindkey -e

# no c-s/c-q output freezing
setopt noflowcontrol
# allow expansion in prompts
setopt prompt_subst
# this is default, but set for share_history
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

# import new commands from the history file also in other zsh-session
setopt share_history
# If a new command line being added to the history list duplicates an older
# one, the older command is removed from the list
setopt histignorealldups
# remove command lines from the history list when the first character on the
# line is a space
setopt histignorespace

# avoid "beep"ing
setopt nobeep

bindkey '^[[A' history-substring-search-up
bindkey '^[[B' history-substring-search-down
bindkey "\e[1;5C" forward-word
bindkey "\e[1;5D" backward-word
bindkey "^[[H" beginning-of-line
bindkey "^[[F" end-of-line
bindkey "^[[3~" delete-char
bindkey "^[[3;5~" kill-word
bindkey "^H" backward-kill-word

if [[ -f "$HOME/.asdf/asdf.sh" ]]; then
	. $HOME/.asdf/asdf.sh
fi

if (( $+commands[flux] )) then
	if [[ -f "$(brew --prefix asdf)/libexec/asdf.sh" ]]; then
		. $(brew --prefix asdf)/libexec/asdf.sh
	fi
fi

if (( $+commands[flux] )) then
	. <(flux completion zsh)
fi

alias em="emacsclient -c"

if (( $+commands[fnm] )) then
	if [[ ! -f "$HOME/.zfunc/_fnm" ]] then
		echo "fnm completion file not found, generating..."
		fnm completions > $HOME/.zfunc/_fnm
	fi
fi

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/zed/Downloads/google-cloud-sdk/path.zsh.inc' ]; then . '/Users/zed/Downloads/google-cloud-sdk/path.zsh.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/Users/zed/Downloads/google-cloud-sdk/completion.zsh.inc' ]; then . '/Users/zed/Downloads/google-cloud-sdk/completion.zsh.inc'; fi

if (( $+commands[rtx] )) then
	eval "$(rtx activate zsh)"
	if [[ ! -f "$HOME/.zfunc/_rtx" ]] then
		echo "rtx completion file not found, generating..."
		rtx complete --shell zsh > $HOME/.zfunc/_rtx
	fi
fi

if (( $+commands[starship] )) then
	eval "$(starship init zsh)"
fi

if (( $+commands[flux] )) then
	. <(flux completion zsh)
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

if (( $+commands[cargo] )) then
	if [[ ! -f "$HOME/.zfunc/_cargo" ]]; then
		rustup completions zsh cargo > $HOME/.zfunc/_cargo
	fi
fi

if (( $+commands[rustup] )) then
	if [[ ! -f "$HOME/.zfunc/_rustup" ]]; then
		echo "rustup completion file not found, generating..."
		rustup completions zsh > $HOME/.zfunc/_rustup
	fi
fi


# Fuzzy git checkout
# by: https://polothy.github.io/post/2019-08-19-fzf-git-checkout/

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

alias ls="exa --icons"
alias gpu="git push"
