// --------------------------------------------------------------------------------------
// Builds the documentation from `.fsx` and `.md` files in the 'docs/content' directory
// (the generated documentation is stored in the 'docs/output' directory)
// --------------------------------------------------------------------------------------

// Binaries that have XML documentation (in a corresponding generated XML file)
let referenceBinaries = [ "FSharp.Text.RegexProvider.dll" ]
// Web site location for the generated documentation
let website = "/FSharp.Text.RegexProvider"

let githubLink = "http://github.com/fsprojects/FSharp.Text.RegexProvider"

// Specify more information about your project
let info =
  [ "project-name", "FSharp.Text.RegexProvider"
    "project-author", "Steffen Forkmann"
    "project-summary", "A type provider for regular expressions."
    "project-github", "http://github.com/fsprojects/FSharp.Text.RegexProvider"
    "project-nuget", "https://www.nuget.org/packages/FSharp.Text.RegexProvider" ]

// --------------------------------------------------------------------------------------
// For typical project, no changes are needed below
// --------------------------------------------------------------------------------------

#I "../../packages/Fake.Core.Environment/lib/netstandard2.0"
#I "../../packages/Fake.Core.Context/lib/netstandard2.0"
#I "../../packages/Fake.Core.FakeVar/lib/netstandard2.0"
#I "../../packages/Fake.Core.Trace/lib/netstandard2.0"
#I "../../packages/Fake.IO.FileSystem/lib/netstandard2.0"
#load "../../packages/FSharp.Formatting/FSharp.Formatting.fsx"
#r "Fake.Core.Environment.dll"
#r "Fake.Core.Trace.dll"
#r "Fake.IO.FileSystem.dll"
open System.IO
open Fake
open Fake.Core
open Fake.IO
open Fake.IO.FileSystemOperators
open FSharp.Literate
open FSharp.MetadataFormat

// When called from 'build.fsx', use the public project URL as <root>
// otherwise, use the current 'output' directory.
#if RELEASE
let root = website
#else
let root = "file://" + (__SOURCE_DIRECTORY__ @@ "../output")
#endif

// Paths with template/source/output locations
let bin        = __SOURCE_DIRECTORY__ @@ "../../bin"
let content    = __SOURCE_DIRECTORY__ @@ "../content"
let output     = __SOURCE_DIRECTORY__ @@ "../output"
let files      = __SOURCE_DIRECTORY__ @@ "../files"
let templates  = __SOURCE_DIRECTORY__ @@ "templates"
let formatting = __SOURCE_DIRECTORY__ @@ "../../packages/FSharp.Formatting/"
let docTemplate = formatting @@ "templates/docpage.cshtml"

// Where to look for *.csproj templates (in this order)
let layoutRootsAll = new System.Collections.Generic.Dictionary<string, string list>()
layoutRootsAll.Add("en",[ templates; formatting @@ "templates"
                          formatting @@ "templates/reference" ])
DirectoryInfo.getSubDirectories (DirectoryInfo templates)
|> Seq.iter (fun d ->
                let name = d.Name
                if name.Length = 2 || name.Length = 3 then
                    layoutRootsAll.Add(
                            name, [templates @@ name
                                   formatting @@ "templates"
                                   formatting @@ "templates/reference" ]))

// Copy static files and CSS + JS from F# Formatting
let copyFiles () =
  Shell.copyRecursive files output true |> Seq.iter (Trace.logfn "Copying file: %s")
  Directory.create (output @@ "content")
  Shell.copyRecursive (formatting @@ "styles") (output @@ "content") true 
    |> Seq.iter (Trace.logfn "Copying styles and scripts: %s")

let references =
  if Environment.isMono then
    // Workaround compiler errors in Razor-ViewEngine
    let d = RazorEngine.Compilation.ReferenceResolver.UseCurrentAssembliesReferenceResolver()
    let loadedList = d.GetReferences () |> Seq.map (fun r -> r.GetFile()) |> Seq.cache
    // We replace the list and add required items manually as mcs doesn't like duplicates...
    let getItem name = loadedList |> Seq.find (fun l -> l.Contains name)
    [ (getItem "FSharp.Core").Replace("4.3.0.0", "4.3.1.0")
      Path.GetFullPath "./../../packages/FSharp.Compiler.Service/lib/net40/FSharp.Compiler.Service.dll"
      Path.GetFullPath "./../../packages/FSharp.Formatting/lib/net40/System.Web.Razor.dll"
      Path.GetFullPath "./../../packages/FSharp.Formatting/lib/net40/RazorEngine.dll"
      Path.GetFullPath "./../../packages/FSharp.Formatting/lib/net40/FSharp.Literate.dll"
      Path.GetFullPath "./../../packages/FSharp.Formatting/lib/net40/FSharp.CodeFormat.dll"
      Path.GetFullPath "./../../packages/FSharp.Formatting/lib/net40/FSharp.MetadataFormat.dll" ]
    |> Some
  else None

// Build API reference from XML comments
let buildReference () =
  Shell.cleanDir (output @@ "reference")
  let binaries =
    referenceBinaries
    |> List.map (fun lib-> bin @@ lib)
  MetadataFormat.Generate
    ( binaries, output @@ "reference", layoutRootsAll.["en"],
      parameters = ("root", root)::info,
      sourceRepo = githubLink @@ "tree/master",
      sourceFolder = __SOURCE_DIRECTORY__ @@ ".." @@ "..",
      publicOnly = true, libDirs = [bin],
      ?assemblyReferences = references )

// Build documentation from `fsx` and `md` files in `docs/content`
let buildDocumentation () =
  let fsiEval = FsiEvaluator ()
  let subdirs = Directory.EnumerateDirectories(content, "*", SearchOption.AllDirectories)
  for dir in Seq.append [content] subdirs do
    let sub = if dir.Length > content.Length then dir.Substring(content.Length + 1) else "."
    let langSpecificPath(lang, path:string) =
        path.Split([|'/'; '\\'|], System.StringSplitOptions.RemoveEmptyEntries)
        |> Array.exists(fun i -> i = lang)
    let layoutRoots =
        let key = layoutRootsAll.Keys |> Seq.tryFind (fun i -> langSpecificPath(i, dir))
        match key with
        | Some lang -> layoutRootsAll.[lang]
        | None -> layoutRootsAll.["en"] // "en" is the default language
    Literate.ProcessDirectory
      ( dir, docTemplate, output @@ sub, replacements = ("root", root)::info,
        layoutRoots = layoutRoots,
        ?assemblyReferences = references,
        generateAnchors = true,
        fsiEvaluator = fsiEval)

// Generate
copyFiles()
#if HELP
buildDocumentation()
#endif
#if REFERENCE
buildReference()
#endif