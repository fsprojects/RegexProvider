@echo off
cls

.paket\paket.bootstrapper.exe
if errorlevel 1 (
  exit /b %errorlevel%
)

.paket\paket.exe restore
if errorlevel 1 (
  exit /b %errorlevel%
)

IF NOT EXIST build.fsx (
  .paket\paket.exe update
  dotnet packages\fake-cli\tools\netcoreapp2.1\any\fake-cli.dll run init.fsx
)
dotnet packages\fake-cli\tools\netcoreapp2.1\any\fake-cli.dll run build.fsx %*
