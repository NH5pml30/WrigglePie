call BankConfig.bat
                                                         	
java^
	-D"file.encoding=UTF-8"^
	-cp .^
	-p %OUT_DIR%\%SRC_PCKG%.jar;lib\;%COURSE_DIR%\lib\;%COURSE_DIR%\artifacts\^
	-m %SRC_PCKG%/%BANK_TESTER% %BANK_CUT%
@if %ERRORLEVEL% NEQ 0 exit /B 1 else exit /B 0
