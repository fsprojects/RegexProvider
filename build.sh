#!/bin/bash
  # use .Net

[ -f .tools/fake ] || dotnet tool install fake-cli --tool-path .tools
.tools/fake run build.fsx -t $@
