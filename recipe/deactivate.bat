@echo off
if defined _OLD_PATH (
    set PATH=%_OLD_PATH%
    set _OLD_PATH
)
if defined _OLD_PYTHONPATH (
    set PYTHONPATH=%_OLD_PYTHONPATH%
    set _OLD_PYTHONPATH
)
if defined _OLD_LD_LIBRARY_PATH (
    set LD_LIBRARY_PATH=%_OLD_LD_LIBRARY_PATH%
    set _OLD_LD_LIBRARY_PATH
)
set HOMETEL=
set SYSTEL=
