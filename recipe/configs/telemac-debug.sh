#!/bin/bash
if [ "$1" = "on" ]; then
  USETELCFG="gnu.dynamic.debug"
  echo "Telemac debug mode ON"
fi
if [ "$1" = "off" ]; then
  USETELCFG="gnu.dynamic"
  echo "Telemac debug mode OFF"
fi