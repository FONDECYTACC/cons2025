# clean-git.ps1
# Script to remove all stray desktop.ini files from the .git folder
# PS C:\Windows\system32> cd "G:\My Drive\Alvacast\SISTRAT 2023\cons"
# PS G:\My Drive\Alvacast\SISTRAT 2023\cons> Get-ChildItem -Path .git -Filter desktop.ini -Recurse -Force | Remove-Item -Force
# PS G:\My Drive\Alvacast\SISTRAT 2023\cons>
# PS G:\My Drive\Alvacast\SISTRAT 2023\cons> Set-ExecutionPolicy -Scope Process -ExecutionPolicy Bypass

Write-Host "Cleaning desktop.ini files from .git..." -ForegroundColor Cyan

# Primary and fallback paths
$primaryPath = "G:\My Drive\Alvacast\SISTRAT 2023\cons\.git"
$fallbackPath = "C:\Users\$env:USERNAME\Documents\GitHub"

# Check if primary path exists; if not, use fallback
if (Test-Path $primaryPath) {
    $repoPath = $primaryPath
    Write-Host "Using primary path: $repoPath" -ForegroundColor Yellow
} else {
    $repoPath = $fallbackPath
    Write-Host "Primary path not found. Using fallback: $repoPath" -ForegroundColor Yellow
}

# Find and delete all desktop.ini files inside .git
Get-ChildItem -Path $repoPath -Filter desktop.ini -Recurse -Force |
    ForEach-Object {
        try {
            Remove-Item $_.FullName -Force
            Write-Host "Deleted:" $_.FullName -ForegroundColor Green
        } catch {
            Write-Host "Failed to delete:" $_.FullName -ForegroundColor Red
        }
    }

Write-Host "Cleanup complete. Run 'git gc --prune=now' and 'git fsck --full' next." -ForegroundColor Cyan
