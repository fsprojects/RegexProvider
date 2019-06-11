//#load "../../.paket/load/netstandard2.0/docs/docs.group.fsx"
#load "../../.paket/load/netstandard2.0/docs/FSharp.Compiler.Service.fsx"
#load "../../.paket/load/netstandard2.0/docs/FSharp.Literate.fsx"
#load "../../.paket/load/netstandard2.0/docs/Fable.React.fsx"
#if !FAKE
#r "netstandard"
#endif

open System
open Fable.React
open Fable.React.Props
open FSharp.Literate

let (</>) x y = IO.Path.Combine(x,y)
module Path =
    let root = __SOURCE_DIRECTORY__ </> ".."
    let content = root </> "content"
    let output = root </> "output"
    let files = root </> "files"

    let dir p = IO.Path.GetDirectoryName(p: string)
    let filename p = IO.Path.GetFileName(p: string)
    let changeExt ext p = IO.Path.ChangeExtension(p, ext)

module Directory =
    let ensure dir =
        if not (IO.Directory.Exists dir) then
            IO.Directory.CreateDirectory dir |> ignore

    let copyRecursive (path: string) dest =
        let path =
            if not (path.EndsWith(string IO.Path.DirectorySeparatorChar)) then
                path + string IO.Path.DirectorySeparatorChar
            else
                path
        let trim (p: string) =
            if p.StartsWith(path) then
                p.Substring(path.Length)
            else
                failwithf "Cannot find path root"
        IO.Directory.EnumerateFiles(path, "*", IO.SearchOption.AllDirectories)
        |> Seq.iter (fun p ->
            let target = dest </> trim p
            ensure(Path.dir target)
            IO.File.Copy(p, target, true))


    

type Template = 
    { Name: string
      Description: string
      Body: string
      Author: string
      GitHub: string
      NuGet: string
      Root: string }

let properties =
    { Name = "FSharp.Text.RegexProvider"
      Description = "A type provider for regular expressions."
      Author = "Steffen Forkmann"
      GitHub = "http://github.com/fsprojects/FSharp.Text.RegexProvider"
      NuGet = "https://www.nuget.org/packages/FSharp.Text.RegexProvider" 
      Body = ""
      Root = "."}

let href t link =
    Href (t.Root + link)
let src t link =
    Src (t.Root + link)

let template t =
    fragment [] [
        RawText "<!DOCTYPE html>"
        RawText "\n"
        html [ Lang "en" ]
            [ head [ ]
                [ meta [ CharSet "utf-8" ]
                  title [ ] [ str t.Name ]
                  meta [ Name "viewport"
                         HTMLAttr.Content "width=device-width, initial-scale=1.0" ]
                  meta [ Name "description"
                         HTMLAttr.Content t.Description ]
                  meta [ Name "author"
                         HTMLAttr.Content t.Author ]
                  script [ Src "http://code.jquery.com/jquery-1.8.0.js" ] [ ]
                  script [ Src "http://code.jquery.com/ui/1.8.23/jquery-ui.js" ] [ ]
                  script [ Src "http://netdna.bootstrapcdn.com/twitter-bootstrap/2.2.1/js/bootstrap.min.js" ] [ ]
                  link [ Href "http://netdna.bootstrapcdn.com/twitter-bootstrap/2.2.1/css/bootstrap-combined.min.css"
                         Rel "stylesheet" ]
                  link [ Type "text/css"
                         Rel "stylesheet"
                         href t "/content/style.css" ]
                  script [ Type "text/javascript"
                           src t "/content/tips.js" ]
                    [ ] ]
              body [ ]
                [ div [ Class "container" ]
                    [ div [ Class "masthead" ]
                        [ ul [ Class "nav nav-pills pull-right" ]
                            [ li [ ]
                                [ a [ Href "http://fsharp.org" ]
                                    [ str "fsharp.org" ] ]
                              li [ ]
                                [ a [ Href t.GitHub ]
                                    [ str "github page" ] ] ]
                          h3 [ Class "muted" ]
                            [ a [ href t "/index.html" ]
                                [ str t.Name ] ] ]
                      hr [ ]
                      div [ Class "row" ]
                        [ div [ Class "span9"
                                Id "main" ]
                            [ RawText t.Body ]
                          div [ Class "span3" ]
                            [ ul [ Class "nav nav-list"
                                   Id "menu" ]
                                [ li [ Class "nav-header" ]
                                    [ str t.Name ]
                                  li [ ]
                                    [ a [ href t "/index.html" ]
                                        [ str "Home page" ] ]
                                  li [ Class "divider" ]
                                    [ ]
                                  li [ ]
                                    [ a [ Href t.NuGet ]
                                        [ str "Get Library via NuGet" ] ]
                                  li [ ]
                                    [ a [ Href t.GitHub ]
                                        [ str "Source Code on GitHub" ] ]
                                  li [ ]
                                    [ a [ href t "/license.html" ]
                                        [ str "License" ] ]
                                  li [ ]
                                    [ a [ href t "/release-notes.html" ]
                                        [ str "Release Notes" ] ] ] ] ] ]
                  a [ Href t.GitHub ]
                    [ img [ Style [ Position PositionOptions.Absolute
                                    Top "0"
                                    Right "0"
                                    Border "0" ]
                            Src "https://s3.amazonaws.com/github/ribbons/forkme_right_gray_6d6d6d.png"
                            Alt "Fork me on GitHub" ] ] ] ] ]

let write path html =
    use writer = System.IO.File.CreateText(path)
    Fable.ReactServer.Raw.writeTo  writer (Fable.ReactServer.castHTMLNode html)

let docPackagePath  path =
    __SOURCE_DIRECTORY__ + @"/../../packages/docs/" + path
let includeDir path =
    "-I:" + docPackagePath path
let reference path =
    "-r:" + docPackagePath path
let evaluationOptions = 
    [| 
         includeDir "FSharp.Core/lib/netstandard1.6/"
         includeDir "FSharp.Literate/lib/netstandard2.0/" 
         includeDir "FSharp.Compiler.Service/lib/netstandard2.0/" 
         reference "FSharp.Compiler.Service/lib/netstandard2.0/FSharp.Compiler.Service.dll" |] 

let compilerOptions = 
    String.concat " " ( 
         "-r:System.Runtime"
         :: Array.toList evaluationOptions)

let parseFsx source =
    let doc = 
      
      Literate.ParseScriptString(
                  source, 
                  compilerOptions = compilerOptions,
                  fsiEvaluator = FSharp.Literate.FsiEvaluator(evaluationOptions))
    FSharp.Literate.Literate.FormatLiterateNodes(doc, OutputKind.Html, "", true, true)

let parseMd source =
    let doc = 
      Literate.ParseMarkdownString(
                  source, 
                  compilerOptions = compilerOptions,
                  fsiEvaluator = FSharp.Literate.FsiEvaluator(evaluationOptions))
    FSharp.Literate.Literate.FormatLiterateNodes(doc, OutputKind.Html, "", true, true)

let format (doc: LiterateDocument) =
    Formatting.format doc.MarkdownDocument true OutputKind.Html

let processFile outdir path  =
    let outfile = 
        let name = path |> Path.filename |> Path.changeExt ".html"
        outdir </> name

    let parse = 
        match IO.Path.GetExtension(path) with
        | ".fsx" -> parseFsx
        | ".md" -> parseMd
        | ext -> failwithf "Unable to process doc for %s files" ext
    let t =
        { properties with
            Body =
                IO.File.ReadAllText(path)
                |> parse
                |> format }
    t 
    |> template
    |> write outfile


Directory.copyRecursive Path.files Path.output

IO.Directory.EnumerateFiles Path.content
|> Seq.iter (processFile Path.output)
