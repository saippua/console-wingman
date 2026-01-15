function cli-lookup-widget() {
  local new_command
  new_command=$(echo "$BUFFER" | cli-lookup --zsh)

  if [[ -n $new_command ]]; then
    BUFFER=$new_command
    CURSOR=${#BUFFER}
    zle reset-prompt
  fi
}

zle -N cli-lookup-widget

bindkey '^G' cli-lookup-widget
