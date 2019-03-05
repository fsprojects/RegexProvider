#!/bin/bash
if test "$OS" = "Windows_NT"
then
  # use .Net

  .paket/paket.bootstrapper.exe
  exit_code=$?
  if [ $exit_code -ne 0 ]; then
  	exit $exit_code
  fi

  .paket/paket.exe restore
  exit_code=$?
  if [ $exit_code -ne 0 ]; then
  	exit $exit_code
  fi
  
  [ ! -e build.fsx ] && .paket/paket.exe update
  [ ! -e build.fsx ] && dotnet packages/fake-cli/tools/netcoreapp2.1/any/fake-cli.dll run init.fsx
  dotnet packages/fake-cli/tools/netcoreapp2.1/any/fake-cli.dll run build.fsx
else
  # use mono
  mono .paket/paket.bootstrapper.exe
  exit_code=$?
  if [ $exit_code -ne 0 ]; then
  	exit $exit_code
  fi

  mono .paket/paket.exe restore
  exit_code=$?
  if [ $exit_code -ne 0 ]; then
  	exit $exit_code
  fi

  [ ! -e build.fsx ] && mono .paket/paket.exe update
  [ ! -e build.fsx ] && dotnet packages/fake-cli/tools/netcoreapp2.1/any/fake-cli.dll run init.fsx
  dotnet packages/fake-cli/tools/netcoreapp2.1/any/fake-cli.dll run build.fsx
fi
