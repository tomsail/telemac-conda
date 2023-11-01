:: Copy examples ad notebooks
mkdir %LIBRARY_PREFIX%\opentelemac\examples
mkdir %LIBRARY_PREFIX%\opentelemac\notebooks
xcopy %SRC_DIR%\opentelemac\examples %LIBRARY_PREFIX%\opentelemac\examples /E /H /C /I
xcopy %SRC_DIR%\opentelemac\notebooks %LIBRARY_PREFIX%\opentelemac\notebooks /E /H /C /I
