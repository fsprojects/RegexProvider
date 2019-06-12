open Fake.SystemHelper
// --------------------------------------------------------------------------------------
// FAKE build script 
// --------------------------------------------------------------------------------------

// #I @"packages/FAKE/tools/"
// #r @"packages/FAKE/tools/FakeLib.dll"
#r "paket:
storage: none
source https://api.nuget.org/v3/index.json

nuget Fake.Core
nuget Fake.Core.ReleaseNotes
nuget Fake.Core.Target
nuget Fake.Core.Trace
nuget Fake.DotNet.AssemblyInfoFile
nuget Fake.Dotnet.Cli
nuget Fake.DotNet.Fsi
nuget Fake.Tools.Git
nuget Fake.DotNet.Paket //"

#load ".fake/build.fsx/intellisense.fsx"
#if !FAKE
#r "netstandard"
#endif

open Fake 
open Fake.Tools.Git
open Fake.Core
open Fake.Core.TargetOperators
open Fake.Tools.Git.Repository
open Fake.IO
open Fake.DotNet
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing.Operators
open Fake.DotNet.NuGet
open System
open System.IO

let projects = [|"RegexProvider"|]

let projectName = "FSharp.Text.RegexProvider"
let summary = "A type provider for regular expressions."
let description = "A type provider for regular expressions."

let solutionFile  = "RegexProvider"

let testAssemblies = "tests/**/bin/Release/*.Tests*.dll"
let gitHome = "https://github.com/fsprojects"
let gitName = "FSharp.Text.RegexProvider"
let cloneUrl = "git@github.com:fsprojects/FSharp.Text.RegexProvider.git"
let nugetDir = "./nuget/"

// Read additional information from the release notes document
Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
let release = ReleaseNotes.load "RELEASE_NOTES.md"

// Generate assembly info files with the right version & up-to-date information
Target.create "AssemblyInfo" (fun _ ->
  for project in projects do
    let fileName = "src/" + project + "/AssemblyInfo.fs"
    AssemblyInfoFile.createFSharp
        fileName
        [ AssemblyInfo.Title projectName
          AssemblyInfo.Product projectName
          AssemblyInfo.Description summary
          AssemblyInfo.Version release.AssemblyVersion
          AssemblyInfo.FileVersion release.AssemblyVersion ] 
)

// --------------------------------------------------------------------------------------
// Clean build results 

Target.create "Clean" (fun _ ->
    Shell.cleanDirs ["bin"; "temp"; nugetDir]
)

Target.create "CleanDocs" (fun _ ->
    Shell.cleanDirs ["docs/output"]
)

// --------------------------------------------------------------------------------------
// Build library & test project

Target.create "Build" (fun _ ->
    DotNet.build (fun p -> 
        { p with 
            Configuration = DotNet.BuildConfiguration.Release
         } )
        "src/RegexProvider/"

    DotNet.publish (fun p -> 
        { p with 
            Configuration = DotNet.BuildConfiguration.Release
            Framework = Some "netstandard2.0"
            OutputPath = Some "../../bin/netstandard2.0"
         } )
        "src/RegexProvider/"
    DotNet.publish (fun p -> 
        { p with 
            Configuration = DotNet.BuildConfiguration.Release
            Framework = Some "net461"
            OutputPath = Some "../../bin/net461" } )
        "src/RegexProvider/"

    DotNet.build (fun p -> 
        { p with 
            Configuration = DotNet.BuildConfiguration.Release
         } )
        "tests/RegexProvider.Tests/"
)

// --------------------------------------------------------------------------------------
// Run the unit tests using test runner & kill test runner when complete


Target.create "RunTests" (fun _ ->
    DotNet.test (fun p -> {
        p with
            Configuration = DotNet.BuildConfiguration.Release
        }) "tests/RegexProvider.Tests"
)

// --------------------------------------------------------------------------------------
// Build a NuGet package

Target.create "NuGet" (fun _ ->
    let toolPath =
        if File.Exists ".paket/paket" then
            ".paket/paket"
        else
            ".paket/paket.exe"

    Paket.pack (fun p ->
        { p with 
            OutputPath = "bin/nuget"
            ReleaseNotes = release.Notes |> String.toLines
            Version = release.NugetVersion
            ToolPath = toolPath
            BuildConfig = "release" })

    if Environment.hasEnvironVar "nugetkey" then
        !! ("bin/nuget/*.nupkg")
        |> Paket.pushFiles (fun p ->
            { p with ApiKey = Environment.environVar "nugetkey" 
                     ToolPath = toolPath } )
    
)

// --------------------------------------------------------------------------------------
// Generate the documentation

let generateHelp' fail debug =
    let ret, errors = Fake.DotNet.Fsi.exec (fun p -> 
        let fsiPath = __SOURCE_DIRECTORY__ + "/packages/docs/FSharp.Compiler.Tools/tools/fsi.exe"
        { p with WorkingDirectory = "." 
                 ToolPath = Fsi.FsiTool.External fsiPath} ) "docs/tools/generate.fsx" []
    if ret = 0 then
        Trace.traceImportant "Help generated"
    else
        if fail then
            failwith "generating help documentation failed"
        else
            Trace.traceImportant "generating help documentation failed"
    ()

let generateHelp fail =
    generateHelp' fail false

Target.create "GenerateHelp" (fun _ ->
    Shell.rm "docs/content/release-notes.md"
    Shell.copyFile "docs/content/" "RELEASE_NOTES.md"
    Shell.rename "docs/content/release-notes.md" "docs/content/RELEASE_NOTES.md"

    Shell.rm "docs/content/license.md"
    Shell.copyFile "docs/content/" "LICENSE.txt"
    Shell.rename "docs/content/license.md" "docs/content/LICENSE.txt"

    generateHelp true
)

Target.create "GenerateHelpDebug" (fun _ ->
    Shell.rm "docs/content/release-notes.md"
    Shell.copyFile "docs/content/" "RELEASE_NOTES.md"
    Shell.rename "docs/content/release-notes.md" "docs/content/RELEASE_NOTES.md"

    Shell.rm "docs/content/license.md"
    Shell.copyFile "docs/content/" "LICENSE.txt"
    Shell.rename "docs/content/license.md" "docs/content/LICENSE.txt"

    generateHelp' true true
)

Target.create "KeepRunning" (fun _ ->    
    use watcher = new FileSystemWatcher(DirectoryInfo("docs/content").FullName,"*.*")
    watcher.EnableRaisingEvents <- true
    watcher.Changed.Add(fun e -> generateHelp false)
    watcher.Created.Add(fun e -> generateHelp false)
    watcher.Renamed.Add(fun e -> generateHelp false)
    watcher.Deleted.Add(fun e -> generateHelp false)

    Trace.traceImportant "Waiting for help edits. Press any key to stop."

    System.Console.ReadKey() |> ignore

    watcher.EnableRaisingEvents <- false
    watcher.Dispose()
)

Target.create "GenerateDocs" ignore

// --------------------------------------------------------------------------------------
// Release Scripts

Target.create "ReleaseDocs" (fun _ ->
    let tempDocsDir = "temp/gh-pages"
    Shell.cleanDir tempDocsDir
    Repository.cloneSingleBranch "" cloneUrl "gh-pages" tempDocsDir

    fullclean tempDocsDir
    Shell.copyRecursive "docs/output" tempDocsDir true |> Trace.tracefn "%A"
    Staging.stageAll tempDocsDir
    Commit.exec tempDocsDir (sprintf "Update generated documentation for version %s" release.NugetVersion)
    Branches.push tempDocsDir
)

Target.create "Release" ignore

// --------------------------------------------------------------------------------------
// Run all targets by default. Invoke 'build <Target>' to override

Target.create "All" ignore

"Clean"
  ==> "AssemblyInfo"
  ==> "Build"
  ==> "RunTests"
  =?> ("GenerateDocs", BuildServer.isLocalBuild)
  ==> "All"
  =?> ("ReleaseDocs", BuildServer.isLocalBuild)

"All"
  ==> "NuGet"

"CleanDocs"
  ==> "GenerateHelp"
  ==> "GenerateDocs"

"CleanDocs"
  ==> "GenerateHelpDebug"

"GenerateHelp"
  ==> "KeepRunning"
    
"ReleaseDocs"
  ==> "Release"

"Nuget"
  ==> "Release"

Target.runOrDefault "All"
