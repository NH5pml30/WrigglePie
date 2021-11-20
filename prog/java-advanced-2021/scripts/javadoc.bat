call config.bat

javadoc.exe^
	-private -notree -nonavbar -noindex^
	-link https://docs.oracle.com/en/java/javase/11/docs/api/^
	-d %JAVADOC_DIR%^
	-classpath %COURSE_DIR%\artifacts\*;%COURSE_DIR%\lib\*^
	--module-path %COURSE_DIR%\lib\^
	%IMPL_DIR%\%MAIN_CLASS_NAME%.java^
	%IMPL_DIR%\package-info.java^
	%TASK_DEPS%
