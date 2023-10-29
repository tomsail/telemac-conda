:: TELEMAC home directory
set HOMETEL=%SRC_DIR%\telemac-mascaret

:: Copy sources
mkdir %LIBRARY_PREFIX%\telemac-mascaret\documentation
xcopy %HOMETEL%\documentation\*.pdf %LIBRARY_PREFIX%\telemac-mascaret\documentation /E /H /C /I
