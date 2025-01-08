if set -q KREW_ROOT then
  fish_add_path $KREW_ROOT/.krew/bin
else
  fish_add_path $HOME/.krew/bin
end
