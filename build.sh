#!/bin/sh -e

mono .nuget/NuGet.exe install FAKE -OutputDirectory packages -ExcludeVersion
mono packages/FAKE/tools/FAKE.exe build.fsx $@
