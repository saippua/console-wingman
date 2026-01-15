# Console-Wingman

A command-line assistant that transforms natural language descriptions into shell commands using AI.

## Prerequisites

- set `GEMINI_API_KEY` environment variable (https://aistudio.google.com/app/api-keys)

### Supported platforms

- `bash`
- `powershell`

## Installation

Source these in your profile script(s) to make the tool available in every new shell instance

```bash
# Linux (bash)
source cli/bash/console-wingman.sh
```

or

```powershell
# Windows (powershell)
.\cli\pwsh\console-wingman.ps1
```

### Standalone installation commands

Run these commands on a system to download the script and add it to the profile

```bash
# Linux (bash)
# Download the script
curl -fsSL "https://raw.githubusercontent.com/saippua/console-wingman/main/cli/bash/console-wingman.sh" -o "$HOME/.console-wingman.sh"
# Source it in the profile
grep -qxF 'source "$HOME/.console-wingman.sh"' "$HOME/.bashrc" || echo 'source "$HOME/.console-wingman.sh"' >> "$HOME/.bashrc"
```

or

```powershell
# Windows (powershell)
# Download the script
Invoke-WebRequest -Uri "https://raw.githubusercontent.com/saippua/console-wingman/main/cli/pwsh/console-wingman.ps1" -OutFile "$HOME\Documents\PowerShell\console-wingman.ps1"
# Source it in the profile
if (!(Test-Path $PROFILE)) { New-Item -Path $PROFILE -ItemType File -Force }; Add-Content -Path $PROFILE -Value "`n. `"$HOME\Documents\PowerShell\console-wingman.ps1`""
```

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

