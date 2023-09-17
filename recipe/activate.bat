@echo off

:: Set TELEMAC specific environnement variables
set HOMETEL=%CONDA_PREFIX%\Library\telemac-mascaret
set SYSTELCFG=%HOMETEL%\configs\systel.cfg
set USETELCFG=gnu.dynamic

:: Add TELEMAC binaries directory to PATH
set PATH=%HOMETEL%\builds\%USETELCFG%\lib;%PATH%
:: Add TELEMAC Python scripts to PATH and to Python environment
set PATH=%HOMETEL%\scripts\python3;%PATH%
set PYTHONPATH=%HOMETEL%\scripts\python3;%PYTHONPATH%
set PYTHONPATH=%HOMETEL%\builds\%USETELCFG%\wrap_api\lib;%PYTHONPATH%
set PYTHONUNBUFFERED="true"
