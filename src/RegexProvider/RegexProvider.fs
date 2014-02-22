module RegexProvider.RegexProvider

open Samples.FSharp.ProvidedTypes
open RegexProvider.Helper
open System
open System.IO
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.CompilerServices

let internal typedRegex() = 
    let regexType = erasedType<Regex> thisAssembly rootNamespace "Regex"
    regexType.DefineStaticParameters(
        parameters = [ProvidedStaticParameter("pattern", typeof<string>)], 
        instantiationFunction = (fun typeName parameterValues ->
            match parameterValues with 
            | [| :? string as pattern |] -> 
                let matchType = runtimeType<Match> "MatchType"
                matchType.HideObjectMethods <- true

                for group in Regex(pattern).GetGroupNames() do
                    let property = 
                        ProvidedProperty(
                            propertyName = (if group <> "0" then group else "CompleteMatch"),
                            propertyType = typeof<Group>,
                            GetterCode = (fun args -> <@@ (%%args.[0]:Match).Groups.[group] @@>))
                    property.AddXmlDoc(sprintf @"Gets the ""%s"" group from this match" group)
                    matchType.AddMember property
                

                let regexType = erasedType<Regex> thisAssembly rootNamespace typeName
                regexType.HideObjectMethods <- true
                regexType.AddXmlDoc "A strongly typed interface to the regular expression '%s'"

                regexType.AddMember matchType

                let isMatchMethod =
                    ProvidedMethod(
                        methodName = "IsMatch",
                        parameters = [ProvidedParameter("input", typeof<string>)],
                        returnType = typeof<bool>,
                        InvokeCode = (fun args -> <@@ Regex.IsMatch(%%args.[0], pattern) @@>),
                        IsStaticMethod = true)
                isMatchMethod.AddXmlDoc "Indicates whether the regular expression finds a match in the specified input string"

                regexType.AddMember isMatchMethod

                let matchMethod =
                    ProvidedMethod(
                        methodName = "Match",
                        parameters = [ProvidedParameter("input", typeof<string>)],
                        returnType = matchType,
                        InvokeCode = (fun args -> <@@ (%%args.[0]:Regex).Match(%%args.[1]) @@>))
                matchMethod.AddXmlDoc "Searches the specified input string for the first occurrence of this regular expression"

                regexType.AddMember matchMethod

                let matchesMethod =
                    ProvidedMethod(
                        methodName = "Matches",
                        parameters = [ProvidedParameter("input", typeof<string>)],
                        returnType = seqType matchType,
                        InvokeCode = (fun args -> <@@ (%%args.[0]:Regex).Matches(%%args.[1]) |> Seq.cast<Match> @@>))
                matchesMethod.AddXmlDoc "Searches the specified input string for all occurrences of this regular expression"

                regexType.AddMember matchesMethod

                let ctor = 
                    ProvidedConstructor(
                        parameters = [], 
                        InvokeCode = (fun args -> <@@ Regex(pattern) @@>))

                ctor.AddXmlDoc "Initializes a regular expression instance"
                regexType.AddMember ctor

                regexType
            | _ -> failwith "unexpected parameter values"))
    regexType

[<TypeProvider>]
type public RegexProvider(cfg:TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces()

    do this.AddNamespace(rootNamespace, [typedRegex()])

[<TypeProviderAssembly>]
do ()
