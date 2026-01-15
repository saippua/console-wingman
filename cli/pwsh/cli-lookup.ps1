Set-PSReadLineKeyHandler -Key 'Ctrl+g' -ScriptBlock {
    $line = $null
    $cursor = $null
    [Microsoft.PowerShell.PSConsoleReadLine]::GetBufferState([ref]$line, [ref]$cursor)
    
    $newCommand = $line | cli-lookup --pwsh
    
    if ($newCommand) {
        [Microsoft.PowerShell.PSConsoleReadLine]::Replace(0, $line.Length, $newCommand)
        [Microsoft.PowerShell.PSConsoleReadLine]::SetCursorPosition($newCommand.Length)
    }
}
