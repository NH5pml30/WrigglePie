call config.bat

mkdir %OUT_DIR%

powershell.exe -Command "& {Get-ChildItem -Path %SRC_DIR% -Recurse *.java | Resolve-Path -Relative}" > temp_sources.txt

javac.exe -encoding "UTF-8" -Werror -d %BUILD_DIR% --module-path %COURSE_DIR%\artifacts\;%COURSE_DIR%\lib\ @temp_sources.txt &&^
jar.exe -cvfm %OUT_DIR%\%SRC_PCKG%.jar %IMPL_DIR%\META-INF\MANIFEST.MF -C %BUILD_DIR% .