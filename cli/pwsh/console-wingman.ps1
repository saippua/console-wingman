Set-PSReadLineKeyHandler -Key 'Ctrl+g' -ScriptBlock {

    function Get-AI-Command {
      param([String]$Line)

      $prompt = "You are a command line assistant that can help users with their tasks.
User wants assistance with the following command:

$Line

Respond with a command that can be used to achieve the desired result.\n\
Command should be suitable for Windows OS with powershell.
Output only the command, do not include any additional text.
Do not include any quotes or backticks in the output."

      $payload = @{ contents = @( @{ parts = @( @{ text = $prompt } ) } ) }
      $response = Invoke-RestMethod `
        -Uri "https://generativelanguage.googleapis.com/v1beta/models/gemini-3-flash-preview:generateContent" `
        -SkipHttpErrorCheck `
        -Method POST `
        -Headers @{
          "x-goog-api-key" = $env:GEMINI_API_KEY
          "Content-Type" = "application/json"
        } `
        -Body ($payload | ConvertTo-Json -Depth 5)

      try {
        return $response.candidates[0].content.parts[0].text
      } catch {
        Write-Host -Foreground Red -Message "[ERROR] " -NoNewLine
        Write-Host $response.error.message
      }
    }

    $line = $null
    $cursor = $null
    [Microsoft.PowerShell.PSConsoleReadLine]::GetBufferState([ref]$line, [ref]$cursor)
    [Microsoft.PowerShell.PSConsoleReadLine]::BackwardKillLine()

    # $newCommand = $line | cli-lookup --pwsh
    $newCommand = Get-AI-Command -Line $line
    
    if ($newCommand) {
        [Microsoft.PowerShell.PSConsoleReadLine]::BackwardKillLine()
        [Microsoft.PowerShell.PSConsoleReadLine]::Insert($newCommand)
        [Microsoft.PowerShell.PSConsoleReadLine]::SetCursorPosition($newCommand.Length)
    } else {
        [Microsoft.PowerShell.PSConsoleReadLine]::BackwardKillLine()
    }
}

