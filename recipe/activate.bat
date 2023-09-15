@echo off
:: Backup PATH, PYTHONPATH and LD_LIBRARY_PATH
set _OLD_PATH=%PATH%
set _OLD_PYTHONPATH=%PYTHONPATH%
set _OLD_LD_LIBRARY_PATH=%LD_LIBRARY_PATH%

:: Set TELEMAC specific environnement variables
set HOMETEL=%CONDA_PREFIX%\Library\telemac-mascaret
set SYSTELCFG=%HOMETEL%\configs\systel.cfg
set USETELCFG=gnu.static

:: Add TELEMAC binaries directory to PATH
set PATH=%HOMETEL%\builds\%USETELCFG%\lib;%PATH%
:: Add TELEMAC Python scripts to PATH and to Python environment
set PATH=%HOMETEL%\scripts\python3;%PATH%
set PYTHONPATH=%HOMETEL%\scripts\python3;%PYTHONPATH%
set PYTHONPATH=%HOMETEL%\builds\%USETELCFG%\wrap_api\lib;%PYTHONPATH%
set PYTHONUNBUFFERED="true"
