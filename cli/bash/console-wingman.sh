console-wingman-widget() {
    if ! command -v claude &> /dev/null; then
      echo -e "\033[31m[ERROR]\033[0m claude not found on path!" >&2
      return 1
    fi

    local prompt="You are a command line assistant that can help users with their tasks.
User wants assistance with the following command:

$READLINE_LINE

Respond with a command that can be used to achieve the desired result.
Command should be suitable for Linux/Unix OS with bash.
Output only the command, do not include any additional text.
Do not include any quotes or backticks in the output."

    local new_command
    new_command=$(claude -p --model haiku "$prompt" 2>&1)
    local exit_code=$?

    if [ $exit_code -ne 0 ] || [ -z "$new_command" ]; then
        echo -e "\033[31m[ERROR]\033[0m ${new_command:-Failed to get response from claude}" >&2
        return 1
    else
      READLINE_LINE=$new_command
      READLINE_POINT=${#READLINE_LINE}
    fi
}

bind -x '"\C-g": console-wingman-widget'

