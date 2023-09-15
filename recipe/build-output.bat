:: TELEMAC home directory
set HOMETEL=%SRC_DIR%\telemac-mascaret
:: Configuration file
set SYSTELCFG=%HOMETEL%\configs\systel.cfg

:: Configure PATH and PYTHONPATH
set PATH=%HOMETEL%\scripts\python3;%PATH%
set PYTHONPATH=%HOMETEL%\scripts\python3;%PYTHONPATH%

:: Copy systel.cfg in configs directory
del /S /Q %HOMETEL%\configs\*
copy %SRC_DIR%\systel.cfg %HOMETEL%\configs\
:: Set TELEMAC version in systel.cfg
sed -i "/^modules:/a version:    %TELEMAC_VERSION%" %SYSTELCFG%

:: Compile all configs (currently: gnu.static gnu.static.debug)
python -m compile_telemac -j8
if errorlevel 1 exit 1

:: Copy builds
mkdir %LIBRARY_PREFIX%\telemac-mascaret\builds
xcopy %HOMETEL%\builds %LIBRARY_PREFIX%\telemac-mascaret\builds /E /H /C /I

:: Copy sources
mkdir %LIBRARY_PREFIX%\telemac-mascaret\sources
xcopy %HOMETEL%\sources %LIBRARY_PREFIX%\telemac-mascaret\sources /E /H /C /I

:: Copy configs
mkdir %LIBRARY_PREFIX%\telemac-mascaret\configs
copy  %SYSTELCFG% %LIBRARY_PREFIX%\telemac-mascaret\configs

:: Copy python scripts
mkdir %LIBRARY_PREFIX%\telemac-mascaret\scripts\python3
xcopy %HOMETEL%\scripts\python3 %LIBRARY_PREFIX%\telemac-mascaret\scripts\python3 /E /H /C /I

:: Copy TELEMAC command to enable/disable debug mode
copy %SRC_DIR%\telemac-debug.bat %SCRIPTS%\

setlocal EnableDelayedExpansion

:: Copy the [de]activate scripts to %PREFIX%\etc\conda\[de]activate.d.
:: This will allow them to be run on environment activation.
for %%F in (activate deactivate) DO (
    if not exist %PREFIX%\etc\conda\%%F.d mkdir %PREFIX%\etc\conda\%%F.d
    copy %RECIPE_DIR%\%%F.bat %PREFIX%\etc\conda\%%F.d\%PKG_NAME%_%%F.bat
)
