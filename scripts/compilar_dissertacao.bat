@echo off
setlocal
powershell -ExecutionPolicy Bypass -File "%~dp0compilar_dissertacao.ps1" %*
exit /b %ERRORLEVEL%
