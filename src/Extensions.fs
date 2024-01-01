module Feliz.Generator.Extensions


open Feliz.Generator
open System.Net.Http
open System.IO

[<RequireQualifiedAccess>]
module Seq =

    let trimEmptyLines list =
        list
        |> Seq.skipWhile ((=) "")
        |> Seq.rev
        |> Seq.skipWhile ((=) "")
        |> Seq.rev

[<RequireQualifiedAccess>]
module String =
    open System.Text.RegularExpressions

    let newline = "\n"

    let normalizeWhiteSpace input = System.Text.RegularExpressions.Regex.Replace(input, @"\s+", " ")

    let replace (oldValue: string) (newValue: string) (s: string) = s.Replace(oldValue, newValue)
    let replaceNewlineWithSpace = replace newline " "

    let removeControlChars (input: string) =
        input
        |> Seq.filter (fun c -> c <> '\u001b' && not (System.Char.IsControl c))
        |> System.String.Concat
        |> fun s -> System.Text.RegularExpressions.Regex.Replace(s,"", "")

    let removeControlCharacters =
        // https://stackoverflow.com/a/40568888/2978652
        let r = Regex("\p{Cc}", RegexOptions.Compiled)
        fun (s: string) -> r.Replace(s, "")

    let lowerFirst input = if input = "" then input else input.Substring(0, 1).ToLower() + input.Substring 1
    let capitalizeFirst input =
        if System.String.IsNullOrEmpty(input) then input
        else input.[0..0].ToUpper() + input.[1..]


    let spaceCaseTokebabCase input = replace " " "-" input
    let spaceCaseToPascalCase (input: string) = input.Split(' ') |> Array.map capitalizeFirst |> String.concat ""
    let spaceCaseToCamelCase (input: string) = input.Split(' ') |> Array.map capitalizeFirst |> String.concat "" |> lowerFirst

    let kebabCaseToCamelCase (input: string) =
        let pieces = input.Split('-')

        if pieces.Length > 1 then
            pieces
            |> Array.iteri (fun i piece ->
                if i > 0 then
                    pieces.[i] <-
                        piece.Substring(0, 1).ToUpper()
                        + piece.Substring(1))

            pieces |> String.concat ""
        else
            input

    let camelCaseToPascalCase (input: string) =
        if System.String.IsNullOrEmpty(input) then input
        else input.Substring(0, 1).ToUpper() + input.Substring(1)

    let snakeCaseToCamelCase (input: string) =
        let pieces =
            input.Split('_')
            |> Seq.trimEmptyLines
            |> Array.ofSeq
        if pieces.Length > 1 then
            pieces
            |> Array.iteri (fun i piece ->
                if i > 0 then pieces.[i] <- piece.Substring(0, 1).ToUpper() + piece.Substring(1))
            pieces |> String.concat ""
        else
            input


    let appendApostropheToReservedKeywords (name: string) =
        let reserved = // F# reserved keywords
            [
                "checked"; "static"; "fixed"; "inline"; "default"; "component";
                "inherit"; "open"; "type"; "true"; "false"; "in"; "end"; "global";
                "list"; "as"; "base"; "begin"; "class"; "bool"; "int"; "member"; "lazy";
                "virtual"
            ]
            |> Set.ofList

        if reserved.Contains name then sprintf "%s'" name else name


/// Converts a method name to a valid F# name by doing the following:
/// - Removing control characters
/// - Removing control chars
/// - Replacing newlines with spaces
/// - Normalizing whitespace
/// - Converting space case to camel case
/// - Converting snake case to camel case
/// - Converting kebab case to camel case
/// - Removing question marks
/// - Appending an apostrophe to reserved keywords
let mkValidFelizName =
    String.removeControlCharacters
    >> String.removeControlChars
    >> String.replaceNewlineWithSpace
    >> String.normalizeWhiteSpace
    >> String.spaceCaseToCamelCase
    >> String.snakeCaseToCamelCase
    >> String.kebabCaseToCamelCase
    >> String.replace "?" ""
    >> String.appendApostropheToReservedKeywords


[<RequireQualifiedAccess>]
module RegularPropOverload =

    /// Creates an inline prop overload with the specified code for params and
    /// value expression, implemented as a regular (non-extension) member.
    /// Wraps the signature and value in parentheses.
    let create (paramsCode: string) (valueCode: string) =
        let inParentheses input = "(" + input + ")"
        RegularPropOverload.create (inParentheses paramsCode) (inParentheses valueCode)

[<RequireQualifiedAccess>]
module EnumPropOverload =

    /// Creates an inline enum prop value/overload with the specified method name
    /// and code for value expression and no docs or params.
    /// Wraps the signature and value in quotes if the value is not a number and converts the method name to a valid F# name.
    let create methodName valueCode =
        let inQuotes input = "\"" + input + "\""
        System.Int32.TryParse valueCode |> function
        | true, i -> EnumPropOverload.create (mkValidFelizName methodName) ($"{i}")
        | false, _ -> EnumPropOverload.create (mkValidFelizName methodName) (inQuotes valueCode)

[<RequireQualifiedAccess>]
module Prop =

    /// Creates a prop with the specified native API name and method name and no
    /// docs or overloads.
    /// Converts the method name valid F# name.
    let create realPropName methodName =
        Prop.create realPropName (mkValidFelizName methodName)

[<RequireQualifiedAccess>]
module Component =

    /// Creates a component with the specified method name and import
    /// path/selector, no documentation, props, or prop inheritance, and the
    /// default component overload. Converts the selector name to a valid F# name.
    let createImportSelector methodName importSelector importPath =
        Component.createImportSelector (mkValidFelizName methodName) importSelector importPath


type ComponentUrlPath = { Name: string; Url: string }


let downloadAndSaveHtml baseUri cacheFolder componentUrl =
    async {
        use httpClient = new HttpClient()
        let! html = httpClient.GetStringAsync(baseUri + componentUrl.Url) |> Async.AwaitTask

        Directory.CreateDirectory(cacheFolder) |> ignore

        let fileName = Path.Combine(cacheFolder, componentUrl.Name + ".html")

        return File.WriteAllText(fileName, html)
    }


let refresh baseUrl cacheFolder componentUrls =
    componentUrls
    |> List.map (downloadAndSaveHtml baseUrl cacheFolder)
    |> Async.Parallel
    |> Async.Ignore
