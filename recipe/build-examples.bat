:: TELEMAC home directory
set HOMETEL=%SRC_DIR%\telemac-mascaret

:: Copy sources
mkdir %LIBRARY_PREFIX%\telemac-mascaret\examples
mkdir %LIBRARY_PREFIX%\telemac-mascaret\notebooks
xcopy %HOMETEL%\examples %LIBRARY_PREFIX%\telemac-mascaret\examples /E /H /C /I
xcopy %HOMETEL%\notebooks %LIBRARY_PREFIX%\telemac-mascaret\notebooks /E /H /C /I
