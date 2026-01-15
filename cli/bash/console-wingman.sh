console-wingman-widget() {
    if ! command -v jq &> /dev/null; then
      echo -e "\033[31m[ERROR]\033[0m jq not found on path! try: sudo apt install jq" >&2
      return 1
    fi

    if ! command -v curl &> /dev/null; then
      echo -e "\033[31m[ERROR]\033[0m curl not found on path! try: sudo apt install curl" >&2
      return 1
    fi

    if [ ! -v GEMINI_API_KEY ]; then
      echo -e "\033[31m[ERROR]\033[0m GEMINI_API_KEY env var not set!" >&2
      return 1
    fi

    local prompt="You are a command line assistant that can help users with their tasks.
User wants assistance with the following command:

$READLINE_LINE

Respond with a command that can be used to achieve the desired result.
Command should be suitable for Linux/Unix OS with bash.
Output only the command, do not include any additional text.
Do not include any quotes or backticks in the output."

    local payload="{ \"contents\": [ { \"parts\": [ { \"text\": \"$prompt\" } ] } ] }"

    local response=$(curl -s \
        -X POST \
        "https://generativelanguage.googleapis.com/v1beta/models/gemini-3-flash-preview:generateContent" \
        -H "x-goog-api-key: $GEMINI_API_KEY" \
        -H "Content-Type: application/json" \
        -d "$payload")

    local new_command=$(echo "$response" | jq -r '.candidates[0].content.parts[0].text // empty' 2>/dev/null)

    if [ -z "$new_command" ]; then
        local error_msg=$(echo "$response" | jq -r '.error.message // "Unknown error"' 2>/dev/null)
        echo -e "\033[31m[ERROR]\033[0m $error_msg" >&2
        return 1
    else
      READLINE_LINE=$new_command
      READLINE_POINT=${#READLINE_LINE}
    fi
}

bind -x '"\C-g": console-wingman-widget'

