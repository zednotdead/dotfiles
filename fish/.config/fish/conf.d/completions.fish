function generate-completion
  if command -q $argv[1]
    if test ! -f "$HOME/.config/fish/completions/$argv[1].fish"
      echo "$argv[1] completion file not found, generating..."
      eval "$argv[2]" > "$HOME/.config/fish/completions/$argv[1].fish"
      echo "Completion generated!"
    end
  end
end

function get-task-completions
  curl "https://raw.githubusercontent.com/go-task/task/main/completion/fish/task.fish"
end

generate-completion "rustup" "rustup completions fish"
# [INFO]: Cargo doesn't support Fish completion yet, but Fish has built-in completion for it
# generate-completion "cargo" "rustup completions fish cargo"
generate-completion "mise" "mise complete --shell fish"
generate-completion "docker" "docker completion fish"
generate-completion "sqlx" "sqlx completions fish"
generate-completion "gh" "gh completion -s fish"
generate-completion "op" "op completion fish"
generate-completion "pulumi" "pulumi gen-completion fish"
generate-completion "just" "just --completions fish"
generate-completion "podman" "podman completion fish"
generate-completion "kubectl" "kubectl completion fish"
generate-completion "pnpm" "pnpm completion fish"
generate-completion "vault" "vault -autocomplete-install"
generate-completion "flux" "flux completion fish"
generate-completion "helm" "helm completion fish"
generate-completion "jj" "jj util completion --fish"
generate-completion "zellij" "zellij setup --generate-completion fish"
generate-completion "grype" "grype completion fish"
generate-completion "syft" "syft completion fish"
generate-completion "velero" "velero completion fish"
generate-completion "go-task" "get-task-completions"
generate-completion "task" "get-task-completions"
generate-completion "bob" "bob complete fish"
generate-completion "talosctl" "talosctl completion fish"
generate-completion "nyoom" "nyoom completions fish"
generate-completion "kind" "kind completion fish"
generate-completion "rye" "rye self completion -s fish"
