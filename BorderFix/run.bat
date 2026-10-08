@echo off
:: Force the script's working directory to be the folder containing this batch file
cd /d "%~dp0"

:: MAKE SURE THIS PATH MATCHES THE PATH ON YOUR OWN COMPUTER
SET "ACAD_PATH=C:\Program Files\Autodesk\AutoCAD LT 2027\accoreconsole.exe"
:: Check if the Core Console exists before starting
if not exist "%ACAD_PATH%" (
    echo Error: accoreconsole.exe not found at %ACAD_PATH%
    echo Please edit this batch file with your correct AutoCAD LT version path.
    pause
    exit /b
)

:: DRAWING TYPE SELECTION
echo.
echo Select drawing type:
echo   [1] BMS Panel DWGs
echo   [2] BMS Shop DWGs
echo   [3] NA DWGs
echo.

set /p SCRIPT_CHOICE=Enter choice (1-3): 

if "%SCRIPT_CHOICE%"=="1" (
    set "START_SCRIPT=BMSPanelStartScript"
) else if "%SCRIPT_CHOICE%"=="2" (
    set "START_SCRIPT=BMSShopStartScript"
) else if "%SCRIPT_CHOICE%"=="3" (
    set "START_SCRIPT=NAStartScript"
) else (
    echo Invalid selection.
    pause
    exit /b
)

:: YOU MAY CHANGE THIS TO BE THE FOLDER PATH YOU WANT TO CHANGE ALL THE DRAWINGS IN
:: OTHERWISE YOU WILL BE PROMPTED TO ENTER A FILEPATH
SET "DWG_PATH="

if "%DWG_PATH%"=="" (
    set /p DWG_PATH=Enter path for your DWG files:
)

if not exist "%DWG_PATH%" (
    echo.
    echo ERROR: Path does not exist:
    echo %DWG_PATH%
    pause
    exit /b
)

:: Create the temporary script file dynamically
:: Note: Double backslashes are required for LISP paths inside an AutoCAD script
SET "LISP_PATH=%~dp0run.lsp"
SET "TEMP_SCR=%~dp0BMS_Panel_tempRunner.scr"
echo (load "%LISP_PATH:\=\\%") > "%TEMP_SCR%"
echo %START_SCRIPT% >> "%TEMP_SCR%"

echo Script running from: %CD%
echo --------------------------------------------------------

:: Loop through all .dwg files in DWG_PATH and its subfolders
for /r "%DWG_PATH%" %%F in (*.dwg) do (
    echo Processing: %%F

    "%ACAD_PATH%" /i "%%F" /s "%TEMP_SCR%"
)

:: Clean up the temporary script file
if exist "%TEMP_SCR%" del "%TEMP_SCR%"

echo --------------------------------------------------------
echo Batch processing complete!
pause