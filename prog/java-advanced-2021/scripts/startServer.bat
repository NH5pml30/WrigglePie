@echo off
call config.bat

set classpath=%OUT_DIR%\%SRC_PCKG%.jar

start rmiregistry -J--class-path=%classpath%
start java %SRC_PCKG%.bank.Server
