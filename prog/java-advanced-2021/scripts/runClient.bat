@echo off
call config.bat
java -cp %OUT_DIR%\%SRC_PCKG%.jar %SRC_PCKG%.bank.Client %*
