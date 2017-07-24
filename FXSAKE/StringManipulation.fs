namespace FxSake

module StringManipulation = 
    
    // Wrap "Stringy" things up in quotes or double quotes, and allow some types to remain as they are (still as a string)
    let wrapString char stringy = sprintf "%s%O%s" char stringy char

    let wrapThing char thing = 
        match box thing with         
        | :? int as _int -> _int.ToString() // TODO - ExcelDNA always casts numbers to floats, so this is missed always. Does it matter for SQL (i.e. are "bit" fields ok with a string '1' etc?)
        | :? bool as _boolean -> _boolean.ToString() 
        | _ -> wrapString char thing 
    
    // Apply the quote method desred
    let singleQuotes s = wrapThing "'" s

    let doubleQuotes s = 
        let wrappingString = char(34).ToString()
        wrapThing wrappingString s


    // Reduce lists into delimited strings (whitespace should be included in the delimiter if you want it)
    let useDelimiter delimeter a b = sprintf "%s%s%s" a delimeter b
    let commaDelimeter = useDelimiter ", "

    //TESTS
    let meInSingleQuotes = singleQuotes "calvin"
    let oneInSingleQuotes = singleQuotes 1
    let tupleInSingleQuotes = singleQuotes ("calvin", "stewart")
    let booleanInSingleQuotes = singleQuotes true

    let meInDoubleQuotes = doubleQuotes "calvin"
    let booleanInDoubleQuotes = doubleQuotes false

    let listOfSqlArguments = ["MyModel"; "IntheBin"; "apples"]

    let sqlStringExample = listOfSqlArguments |> List.map singleQuotes |> List.reduce (commaDelimeter) 