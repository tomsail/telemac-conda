:: TELEMAC home directory
set HOMETEL=%SRC_DIR%\telemac-mascaret

:: Copy sources
mkdir %LIBRARY_PREFIX%\telemac-mascaret\documentation
for /r %HOMETEL%\documentation %%f in (*.pdf) do @copy "%%f" %LIBRARY_PREFIX%\telemac-mascaret\documentation
