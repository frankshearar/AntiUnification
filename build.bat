@echo off
cls

if exist ".nuget\NuGet.exe" (
   ".nuget\NuGet.exe" "Install" "FAKE" "-OutputDirectory" "packages" "-ExcludeVersion"
) else (
  nuget "Install" "FAKE" "-OutputDirectory" "packages" "-ExcludeVersion"
)

"packages\FAKE\tools\Fake.exe" build.fsx