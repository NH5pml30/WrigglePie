cd ..\

set SRC_DIR=java-solutions
set BUILD_DIR=build
set OUT_DIR=artifacts
set COURSE_DIR=..\java-advanced-2021
set JAVADOC_DIR=javadoc

set COURSE_PCKG=info.kgeorgiy.java.advanced
set COURSE_PCKG_P=%COURSE_PCKG:.=\%
set SRC_PCKG=info.kgeorgiy.ja.holyavin
set SRC_PCKG_P=%SRC_PCKG:.=\%

set TASK_NAME=implementor
set MAIN_CLASS_NAME=Implementor

set IMPL_DIR=%SRC_DIR%\%SRC_PCKG_P%\%TASK_NAME%
set TEST_PCKG=%COURSE_PCKG%.%TASK_NAME%

set TASK_DEP_DIR=%COURSE_DIR%\modules\%TEST_PCKG%\%COURSE_PCKG_P%\%TASK_NAME%
set "TASK_DEPS=%TASK_DEP_DIR%\Impler.java %TASK_DEP_DIR%\JarImpler.java %TASK_DEP_DIR%\ImplerException.java"