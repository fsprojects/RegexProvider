namespace FSharp.Text.RegexProvider

open Samples.FSharp.ProvidedTypes
open FSharp.Text.RegexProvider.Helper
open System
open System.IO
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.CompilerServices

module internal TypedRegex =
    let typedRegex() = 
        let regexType = erasedType<Regex> thisAssembly rootNamespace "Regex"
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

                    let matchType = runtimeType<Match> "MatchType"
                    matchType.HideObjectMethods <- true

                    let safeMatchType = runtimeType<Match> "SafeMatchType"
                    safeMatchType.HideObjectMethods <- true

                    for group in Regex(pattern).GetGroupNames() do
                        let getMatchProperty = 
                            ProvidedProperty(
                                propertyName = (if group <> "0" then group else "CompleteMatch"),
                                propertyType = typeof<Group>,
                                GetterCode = (fun args -> <@@ (%%args.[0]:Match).Groups.[group] @@>))
                        getMatchProperty.AddXmlDoc(sprintf @"Gets the ""%s"" group from this match" group)
                        matchType.AddMember getMatchProperty

                        let getValueSafeProperty = 
                            ProvidedProperty(
                                propertyName = "value_" + (if group <> "0" then group else "CompleteMatch"),
                                propertyType = typeof<string>,
                                GetterCode = (fun args -> <@@ (%%args.[0]:Match).Groups.[group].Value @@>))
                        getValueSafeProperty.AddXmlDoc(sprintf @"Gets the value of the ""%s"" group from this match" group)
                        safeMatchType.AddMember getValueSafeProperty
                        safeMatchType.AddMember getMatchProperty

                    let matchMethod =
                        ProvidedMethod(
                            methodName = getMethodName "NextMatch",
                            parameters = [],
                            returnType = matchType,
                            InvokeCode = (fun args -> <@@ (%%args.[0]:Match).NextMatch() @@>))
                    matchMethod.AddXmlDoc "Searches the specified input string for the next occurrence of this regular expression."

                    matchType.AddMember matchMethod
                    
                    let regexType = erasedType<Regex> thisAssembly rootNamespace typeName
                    regexType.HideObjectMethods <- true
                    regexType.AddXmlDoc "A strongly typed interface to the regular expression '%s'"

                    regexType.AddMember matchType
                    regexType.AddMember safeMatchType

                    let isMatchMethod =
                        ProvidedMethod(
                            methodName = getMethodName "IsMatch",
                            parameters = [ProvidedParameter("input", typeof<string>)],
                            returnType = typeof<bool>,
                            InvokeCode = (fun args -> <@@ Regex.IsMatch(%%args.[0], pattern) @@>),
                            IsStaticMethod = true)
                    isMatchMethod.AddXmlDoc "Indicates whether the regular expression finds a match in the specified input string"

                    regexType.AddMember isMatchMethod

                    let matchMethod =
                        ProvidedMethod(
                            methodName = getMethodName "Match",
                            parameters = [ProvidedParameter("input", typeof<string>)],
                            returnType = matchType,
                            InvokeCode = (fun args -> <@@ (%%args.[0]:Regex).Match(%%args.[1]) @@>))
                    matchMethod.AddXmlDoc "Searches the specified input string for the first occurrence of this regular expression"

                    regexType.AddMember matchMethod
                    
                    let tryMatchMethod =
                        ProvidedMethod(
                            methodName = "Try" + getMethodName "Match",
                            parameters = [ProvidedParameter("input", typeof<string>)],
                            returnType = optionType safeMatchType,
                            InvokeCode = (fun args -> 
                                <@@ let res = (%%args.[0]:Regex).Match(%%args.[1]) 
                                    if res.Success then Some res else None
                                @@>))
                    tryMatchMethod.AddXmlDoc "Searches the specified input string for the first occurrence of this regular expression"

                    regexType.AddMember tryMatchMethod

                    let matchesMethod =
                        ProvidedMethod(
                            methodName = getMethodName "Matches",
                            parameters = [ProvidedParameter("input", typeof<string>)],
                            returnType = seqType matchType,
                            InvokeCode = (fun args -> <@@ (%%args.[0]:Regex).Matches(%%args.[1]) |> Seq.cast<Match> @@>))
                    matchesMethod.AddXmlDoc "Searches the specified input string for all occurrences of this regular expression"

                    regexType.AddMember matchesMethod

                    let tryMatchesMethod =
                        ProvidedMethod(
                            methodName = "Try" + getMethodName "Matches",
                            parameters = [ProvidedParameter("input", typeof<string>)],
                            returnType = (seqType safeMatchType |> optionType),
                            InvokeCode = (fun args -> <@@ (%%args.[0]:Regex).Matches(%%args.[1]) |> Seq.cast<Match> |> (fun s -> if Seq.isEmpty s then None else Some s) @@> ))
                    tryMatchesMethod.AddXmlDoc "Searches the specified input string for all occurrences of this regular expression"
                    
                    regexType.AddMember tryMatchesMethod

                    let ctor = 
                        ProvidedConstructor(
                            parameters = [], 
                            InvokeCode = (fun args -> <@@ Regex(pattern) @@>))

                    ctor.AddXmlDoc "Initializes a regular expression instance"
                    regexType.AddMember ctor

                    let ctor =
                        ProvidedConstructor(
                            parameters = [ProvidedParameter("options", typeof<RegexOptions>)],
                            InvokeCode = (fun args -> <@@ Regex(pattern, %%args.[0]) @@>))
                    ctor.AddXmlDoc "Initializes a regular expression instance, with options that modify the pattern."                
                    regexType.AddMember ctor

                    regexType
                | _ -> failwith "unexpected parameter values"))
        regexType

[<TypeProvider>]
type public RegexProvider(cfg:TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces()
    do this.AddNamespace(rootNamespace, [TypedRegex.typedRegex()])

[<TypeProviderAssembly>]
do ()
