namespace FSharp.Text.RegexProvider

open ProviderImplementation.ProvidedTypes
open FSharp.Text.RegexProvider.Helper
open System
open System.IO
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.CompilerServices

module internal TypedRegex =
    let typedRegex() = 
        let regexType = erasedType<Regex> thisAssembly rootNamespace "Regex" false
        regexType.DefineStaticParameters(
            parameters =
                [
                    ProvidedStaticParameter("pattern", typeof<string>)
                    ProvidedStaticParameter("noMethodPrefix", typeof<bool>, false)
                ], 
            instantiationFunction = (fun typeName parameterValues ->
                match parameterValues with 
                | [| :? string as pattern; :? bool as noMethodPrefix |] ->

                    let getMethodName baseName =
                        if noMethodPrefix then baseName
                        else sprintf "Typed%s" baseName

                    let matchType = runtimeType<Match>"MatchType" true

                    for group in Regex(pattern).GetGroupNames() do
                        let property = 
                            ProvidedProperty(
                                propertyName = (if group <> "0" then group else "CompleteMatch"),
                                propertyType = typeof<Group>,
                                getterCode = (fun args -> <@@ (%%args.[0]:Match).Groups.[group] @@>))
                        property.AddXmlDoc(sprintf @"Gets the ""%s"" group from this match" group)
                        matchType.AddMember property

                    let matchMethod =
                        ProvidedMethod(
                            methodName = getMethodName "NextMatch",
                            parameters = [],
                            returnType = matchType,
                            invokeCode = (fun args -> <@@ (%%args.[0]:Match).NextMatch() @@>))
                    matchMethod.AddXmlDoc "Searches the specified input string for the next occurrence of this regular expression."

                    matchType.AddMember matchMethod
                    

                    let regexType = erasedType<Regex> thisAssembly rootNamespace typeName true
                    regexType.AddXmlDoc "A strongly typed interface to the regular expression '%s'"

                    regexType.AddMember matchType

                    let isMatchMethod =
                        ProvidedMethod(
                            methodName = getMethodName "IsMatch",
                            parameters = [ProvidedParameter("input", typeof<string>)],
                            returnType = typeof<bool>,
                            invokeCode = (fun args -> <@@ Regex.IsMatch(%%args.[0], pattern) @@>),
                            isStatic = true)
                    isMatchMethod.AddXmlDoc "Indicates whether the regular expression finds a match in the specified input string"

                    regexType.AddMember isMatchMethod

                    let matchMethod =
                        ProvidedMethod(
                            methodName = getMethodName "Match",
                            parameters = [ProvidedParameter("input", typeof<string>)],
                            returnType = matchType,
                            invokeCode = (fun args -> <@@ (%%args.[0]:Regex).Match(%%args.[1]) @@>))
                    matchMethod.AddXmlDoc "Searches the specified input string for the first occurrence of this regular expression"

                    regexType.AddMember matchMethod

                    let matchesMethod =
                        ProvidedMethod(
                            methodName = getMethodName "Matches",
                            parameters = [ProvidedParameter("input", typeof<string>)],
                            returnType = seqType matchType,
                            invokeCode = (fun args -> <@@ (%%args.[0]:Regex).Matches(%%args.[1]) |> Seq.cast<Match> @@>))
                    matchesMethod.AddXmlDoc "Searches the specified input string for all occurrences of this regular expression"

                    regexType.AddMember matchesMethod

                    let ctor = 
                        ProvidedConstructor(
                            parameters = [], 
                            invokeCode = (fun args -> <@@ Regex(pattern) @@>))

                    ctor.AddXmlDoc "Initializes a regular expression instance"
                    regexType.AddMember ctor

                    let ctor =
                        ProvidedConstructor(
                            parameters = [ProvidedParameter("options", typeof<RegexOptions>)],
                            invokeCode = (fun args -> <@@ Regex(pattern, %%args.[0]) @@>))
                    ctor.AddXmlDoc "Initializes a regular expression instance, with options that modify the pattern."                
                    regexType.AddMember ctor

                    regexType
                | _ -> failwith "unexpected parameter values"))
        regexType

[<TypeProvider>]
type public RegexProvider(cfg:TypeProviderConfig) =
    inherit TypeProviderForNamespaces(cfg, rootNamespace, [TypedRegex.typedRegex()])

[<TypeProviderAssembly>]
do ()
