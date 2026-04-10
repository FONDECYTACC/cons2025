@echo off
echo Current directory: %cd%
set /p "choice=Do you want to proceed with deleting desktop.ini files? (Y/N): "
if /i "%choice%"=="Y" (
    del desktop.ini /A:H /S
)
if /i "%choice%"=="N" (
    echo Execution aborted.
)
pause