Set-PSReadLineKeyHandler -Key 'Ctrl+g' -ScriptBlock {

    function Get-AI-Command {
      param([String]$Line)

      $prompt = "You are a command line assistant that can help users with their tasks.
User wants assistance with the following command:

$Line

Respond with a command that can be used to achieve the desired result.
Command should be suitable for Windows OS with powershell.
Output only the command, do not include any additional text.
Do not include any quotes or backticks in the output."

      try {
        $response = claude -p --model haiku $prompt 2>&1
        if ($LASTEXITCODE -ne 0) {
          Write-Host -Foreground Red "[ERROR] $response"
          return $null
        }
        return $response
      } catch {
        Write-Host -Foreground Red "[ERROR] $($_.Exception.Message)"
        return $null
      }
    }

    $line = $null
    $cursor = $null
    [Microsoft.PowerShell.PSConsoleReadLine]::GetBufferState([ref]$line, [ref]$cursor)
    [Microsoft.PowerShell.PSConsoleReadLine]::BackwardKillLine()

    $newCommand = Get-AI-Command -Line $line

    if ($newCommand) {
        [Microsoft.PowerShell.PSConsoleReadLine]::BackwardKillLine()
        [Microsoft.PowerShell.PSConsoleReadLine]::Insert($newCommand)
        [Microsoft.PowerShell.PSConsoleReadLine]::SetCursorPosition($newCommand.Length)
    } else {
        [Microsoft.PowerShell.PSConsoleReadLine]::BackwardKillLine()
    }
}

