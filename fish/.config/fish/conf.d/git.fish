function gpuu
  git push -u origin $(git branch --show-current)
end

function getticketnr
  git branch --show-current | perl -lane 'print m/([A-Z]{2,}-\d+)/'
end

function git-current-branch
  git rev-parse --abbrev-ref HEAD
end

if test $(uname) = "Linux"
  alias copy-to-clipboard="wl-copy"
else if test $(uname) = "Darwin"
  alias copy-to-clipboard="pbcopy"
end

function copy
  if test -e $argv[$1] then
    copy-to-clipboard < $argv[$1]
  else
    copy-to-clipboard $argv[$1]
  end
end

function ghpr
  gh pr view $(git-current-branch) --json url | jq -r .url | copy
end

alias gpu="git push"
alias gput="git push && git push --tags"
alias gpl="git pull"
alias goops="git commit -a --amend --no-edit; git push --force-with-lease"
