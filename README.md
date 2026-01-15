# CLI-Lookup

A command-line assistant that transforms natural language descriptions into shell commands using AI.

## Prerequisites

- set `GEMINI_API_KEY` environment variable (https://aistudio.google.com/app/api-keys)
- `bash` or `powershell`

## Installation

```bash
source cli/bash/cli-lookup.zsh
```

or

```powershell
./cli/pwsh/cli-lookup.ps1
```

Add these to your profile script(s) to make the tool available in every new shell instance

## Usage

1. Type a natural language description in your prompt
2. Press `Ctrl+G` to invoke the tool
3. The tool will replace your input with the appropriate command but does not run it automatically

### Examples

```bash
# Type this:
list all files including hidden ones

# Press Ctrl+G, get this:
ls -la

# Type this:
find all python files in current directory

# Press Ctrl+G, get this:
find . -name "*.py"

# Type this:
show disk usage of current directory

# Press Ctrl+G, get this:
du -sh .
```

