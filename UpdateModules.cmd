@echo off
rem Get the submodule initially
@git submodule update --init
@git submodule update

rem Time passes, submodule upstream is updated
rem and you now want to update

echo JEDI include files
rem Change to the submodule directory
cd source\jedi
rem Checkout desired branch
git checkout master

rem Update
git pull

rem Get back to your project root
cd ..\..