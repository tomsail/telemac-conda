@echo off
if [%1]==[on] (
  set USETELCFG=gnu.static.debug
  echo Telemac debug mode ON
)
if [%1]==[off] (
  set USETELCFG=gnu.static
  echo Telemac debug mode OFF
)
