call jar.bat

set VARIANT="%1"
if %VARIANT% == "" set VARIANT=jar-class

java^
	-D"file.encoding=UTF-8"^
	-cp .^
	-p %OUT_DIR%\%SRC_PCKG%.jar;%COURSE_DIR%\lib\;%COURSE_DIR%\artifacts\^
	--add-modules %SRC_PCKG%^
	-m %TEST_PCKG%^
	%VARIANT% %SRC_PCKG%.%TASK_NAME%.%MAIN_CLASS_NAME%