function fzf-git-branch
  git rev-parse HEAD > /dev/null 2>&1 || return

  git branch --color=always --all --sort=-committerdate |
  grep -v HEAD |
  fzf --height 50% --ansi --no-multi --preview-window right:65% | sed "s/.* //"
end

function fzf-git-checkout
  git rev-parse HEAD > /dev/null 2>&1 || return

  set branch $(fzf-git-branch)

  if test $branch = ""
    echo "No branch selected."
    return
  end

  # If branch name starts with 'remotes/' then it is a remote branch. By
  # using --track and a remote branch name, it is the same as:
  # git checkout -b branchName --track origin/branchName
  if string match 'remotes/*' $branch
    git checkout --track $branch
  else
    git checkout $branch;
  end
end

alias gch="fzf-git-checkout"

alias fzf-git-get-hash='git log --oneline | fzf | grep -oE "^.{10}"'

function grev
  git revert $(fzf-git-get-hash)
end
