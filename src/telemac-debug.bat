@echo off
if [%1]==[on] (
  set USETELCFG=gnu.dynamic.debug
  echo Telemac debug mode ON
)
if [%1]==[off] (
  set USETELCFG=gnu.dynamic
  echo Telemac debug mode OFF
)
