#!/bin/sh -e

mono --runtime=v4.0 .nuget/NuGet.exe install FAKE -OutputDirectory packages -ExcludeVersion
mono --runtime=v4.0 packages/FAKE/tools/FAKE.exe build.fsx $@
