#!/bin/sh -e

mono --runtime=v4.0 .nuget/NuGet.exe install FAKE -OutputDirectory packages -ExcludeVersion

packages/FAKE/tools/Fake.exe build.fsx
