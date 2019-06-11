@echo off
cls

IF NOT EXIST tools/fake.exe (
  dotnet tool install fake-cli --tool-path .tools
)
.tools\fake.exe run build.fsx %*
