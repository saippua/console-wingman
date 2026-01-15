cli-lookup-widget() {
  local new_command
  new_command=$(echo "$READLINE_LINE" | cli-lookup --bash)
  if [[ -n $new_command ]]; then
    READLINE_LINE=$new_command
    READLINE_POINT=${#READLINE_LINE}
  fi
}

bind -x '"\C-g": cli-lookup-widget'
