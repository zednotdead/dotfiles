# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
    source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

if (( $+commands[rustup] )) then
    if [[ ! -f "$HOME/.zfunc/_rustup" ]] then
        rustup completions zsh rustup > $HOME/.zfunc/_rustup
    fi
    if [[ ! -f "$HOME/.zfunc/_cargo" ]] then
        rustup completions zsh cargo > $HOME/.zfunc/_cargo
    fi
fi

fpath=($HOME/.asdf/completions $HOME/.zfunc $fpath)

export PATH=$HOME/.local/bin:$HOME/.config/emacs/bin:$HOME/.deno/bin:$HOME/go/bin:$PATH
export PATH="${KREW_ROOT:-$HOME/.krew}/bin:$PATH"
export HISTORY_SUBSTRING_SEARCH_PREFIXED="true"
export EDITOR=/usr/bin/nvim
export DIRENV_LOG_FORMAT=
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
zstyle :compinstall filename '$HOME/.zshrc'


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
if (( $+commands[flux] )) then
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
if [ ! -e "$HOME/.antidote/antidote.zsh" ]
then
    git clone --depth=1 https://github.com/mattmc3/antidote.git ${ZDOTDIR:-~}/.antidote
fi

source $HOME/.antidote/antidote.zsh

bindkey '^[[A' history-substring-search-up
bindkey '^[[B' history-substring-search-down
bindkey "\e[1;5C" forward-word
bindkey "\e[1;5D" backward-word
bindkey "^[[H" beginning-of-line
bindkey "^[[F" end-of-line
bindkey "^[[3~" delete-char
bindkey "^[[3;5~" kill-word
bindkey "^H" backward-kill-word

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

antidote load

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

[ -f "/home/zed/.ghcup/env" ] && source "/home/zed/.ghcup/env" # ghcup-env
typeset -g POWERLEVEL9K_INSTANT_PROMPT=quiet
alias em="emacsclient -c"

if (( $+commands[fnm] )) then
    if [[ ! -f "$HOME/.zfunc/_fnm" ]] then
        fnm completions > $HOME/.zfunc/_fnm
    fi
fi

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/zed/Downloads/google-cloud-sdk/path.zsh.inc' ]; then . '/Users/zed/Downloads/google-cloud-sdk/path.zsh.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/Users/zed/Downloads/google-cloud-sdk/completion.zsh.inc' ]; then . '/Users/zed/Downloads/google-cloud-sdk/completion.zsh.inc'; fi
eval "$(rtx activate zsh)"
