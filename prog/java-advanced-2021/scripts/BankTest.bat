call BankConfig.bat
                                                         	
java^
	-D"file.encoding=UTF-8"^
	-D"cut=%BANK_CUT%"^
	-cp .^
	-p %OUT_DIR%\%SRC_PCKG%.jar;lib\;%COURSE_DIR%\lib\;%COURSE_DIR%\artifacts\^
	--add-modules %SRC_PCKG%^
	-m junit/org.junit.runner.JUnitCore %BANK_TEST%
@if %ERRORLEVEL% NEQ 0 exit /B 1 else exit /B 0
