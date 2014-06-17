#I "packages/FAKE/tools"
#r "FakeLib.dll"
open Fake
open Fake.AssemblyInfoFile
open Fake.Git

RestorePackages()

let buildDir = "./build/"
let testDir = "./test/"

let version =
    match buildServer with
    | AppVeyor -> buildVersion
    | _ -> "0.0.0.1"

let commitHash = Information.getCurrentHash()

Target "Clean" (fun _ ->
    CleanDirs [buildDir; testDir]
)

Target "SetAssemblyInfo" (fun _ ->
    let assemblyPath = "./AntiUnification/Properties/AssemblyInfo.cs"
    System.IO.Directory.CreateDirectory(System.IO.Path.GetDirectoryName(assemblyPath)) |> ignore
    CreateCSharpAssemblyInfo assemblyPath
        [Attribute.Title "AntiUnification"
         Attribute.Description "Generalising from examples"
         Attribute.Guid "8D5773A2-7FC7-4270-9A6D-198DA3855B70"
         Attribute.Product "AntiUnification"
         Attribute.Metadata("githash", commitHash)
         Attribute.Version version
         Attribute.FileVersion version]
)

Target "BuildSolution" (fun _ ->
    MSBuildWithDefaults "Build" ["AntiUnification.sln"]
    |> Log "AppBuild-Output: "
)

Target "Test" (fun _ ->
    !! "./AntiUnificationTests/bin/Release/AntiUnificationTests.dll" // Don't like that "Release" there, but we need to limit ourselves to _one_ copy of the test dll
      |> NUnit (fun p ->
          {p with
             DisableShadowCopy = true;
//             Framework = "net-4.5";
             OutputFile = testDir + "TestResults.xml" })
)

// Default target
Target "Default" (fun _ ->
    trace "AntiUnification, built through the magic of FAKE"
)

"Clean"
    ==> "SetAssemblyInfo"
    ==> "BuildSolution"
    ==> "Test"
    ==> "Default"

// start build
RunTargetOrDefault "Default"
