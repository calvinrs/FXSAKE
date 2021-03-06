﻿module ExcelHelpers

// Add references to Excel interop:
#if INTERACTIVE
#r "Microsoft.Office.Interop.Excel"
#endif

open System
open Microsoft.Office.Interop.Excel
open ExcelDna.Integration 

/// Helper function to represent string or floating point cell content as a string.
let cellContent (range : Range) = 
    match range.Value2 with
    | :? string as _string -> sprintf "string: %s" _string
    | :? double as _double -> sprintf "double: %f" _double
    | _ -> "(unknown type)"

/// Helper function to return cell content as float if possible, if not as 0.0.
let cellDouble (range : Range) = 
    match range.Value2 with
    | :? double as _double -> _double
    | _ -> 0.0
     
/// Returns the specified worksheet range as a sequence of indvidual cell ranges.
let toSeq (range : Range) =
    seq {
            for r in 1 .. range.Rows.Count do
                for c in 1 .. range.Columns.Count do
                    let cell = range.Item(r, c) :?> Range
                    yield cell
    }

/// Returns the specified worksheet range as a sequence of indvidual cell ranges, together with a 0-based
/// row-index and column-index for each cell.
let toSeqrc (range : Range) =
    seq {
            for r in 1 .. range.Rows.Count do
                for c in 1 .. range.Columns.Count do
                    let cell = range.Item(r, c) :?> Range
                    yield r, c, cell
    }

/// Takes a sequence of individual cell-ranges and returns an Excel range representation of the cells 
/// (using Excel 'union' representation - eg. "R1C1, R2C1, R5C4").
let toRange (workSheet : Worksheet) (rangeSeq : seq<Range>) =
    let csvSeq sequence =
        let result = 
            sequence
            |> Seq.fold (fun acc x -> acc + x + ",") ""
        result.Remove(result.Length-1)
    let rangeName = 
        rangeSeq
        |> Seq.map (fun cell -> cell.Address())
        |> csvSeq
    workSheet.Range(rangeName)

/// Takes a function and an Excel range, and returns the results of applying the function to each individual cell.
let map (f : Range -> 'T) (range : Range) =
    range
    |> toSeq
    |> Seq.map f

/// Takes a function and an Excel range, and returns the results of applying the function to each individual cell,
/// providing 0-based row-index and column-index for each cell as arguments to the function.
let maprc (f : int -> int -> Range -> 'T) (range : Range) =
    range
    |> toSeqrc
    |> Seq.map (fun item -> match item with
                            | (r, c, cell) -> f r c cell)

/// Takes a function and an Excel range, and applies the function to each individual cell.
let iter (f : Range -> unit) (range : Range) =
    range
    |> toSeq
    |> Seq.iter (fun cell -> f cell)

/// Takes a function and an Excel range, and applies the function to each individual cell,
/// providing 0-based row-index and column-index for each cell as arguments to the function.
let iterrc (f : int -> int -> Range -> unit) (range : Range) =
    range
    |> toSeqrc
    |> Seq.iter (fun item -> match item with
                                | (r, c, cell) -> f r c cell)

/// Takes a function and an Excel range, and returns a sequence of individual cell ranges where the result
/// of applying the function to the cell is true.
let filter (f : Range -> bool) (range : Range) =
    range
    |> toSeq
    |> Seq.filter (fun cell -> f cell)



/// My helper functions here
let isTallRange (caller: ExcelReference) =         
        let isTall = (caller.RowLast - caller.RowFirst) >= 0 && (caller.ColumnLast - caller.ColumnFirst = 0)           
        isTall

// When data is a 1D array, this will help to resize it for use in array functions horizonally (which it does natively) or veritally (with some shuffling)
let arrayDirectionHelper (caller: obj) rawOutput = 
        let isTall = caller :?> ExcelReference |> isTallRange
        if isTall then Array2D.init (Array.length rawOutput) 1 (fun i j -> match j with |0 -> rawOutput.[i] |_ -> 0.0)
        else Array2D.init 1 (Array.length rawOutput) (fun i j -> match i with |0 -> rawOutput.[j] |_ -> 0.0) 

///// Examples /////

// Start Excel.
let excel = ApplicationClass(Visible = true)

// Open a workbook:
let workbookDir = @"C:\Users\kit\Documents\Visual Studio 2010\Projects\ExcelTest\Spreadsheets"
let workbook = excel.Workbooks.Open(workbookDir + @"\Example1.xlsx")

// Get a reference to the workbook:
let exampleSheet = workbook.Sheets.["ExampleSheet"] :?> Worksheet

// Get a reference to a named range:
let exampleRange = exampleSheet.Range("MyRange")

// toSeq example:
let cellCount =
    exampleRange
    |> toSeq 
    |> Seq.length
// 4

// toSeqrc example:
let listCellRC =
    exampleRange
    |> toSeqrc 
    |> Seq.iter (fun item -> match item with
                             | (r, c, cell) -> printfn "row:%i col:%i cell:%s" r c (cellContent cell))
// row:1 col:1 cell:string: A
// row:2 col:1 cell:double: 1.000000
// row:3 col:1 cell:double: 2.000000
// row:4 col:1 cell:double: 3.000000
// ...
// row:4 col:3 cell:double: 9.000000

// toRange example:
let rangeAddress =
    let range = 
        exampleRange
        |> toSeq
        |> toRange exampleRange.Worksheet
    printfn "Range: %s" (range.Address())
// Range: $A$1,$B$1,$C$1,$A$2,$B$2,$C$2,$A$3,$B$3,$C$3,$A$4,$B$4,$C$4

// map example:
let floatTotal =
    exampleRange
    |> map (fun cell -> cellDouble cell)
    |> Seq.sum
// 42.0

// maprc example:
let evenTotal =
    exampleRange
    |> maprc (fun r _ cell -> if r % 2 = 0 then
                                  cellDouble cell
                              else 
                                  0.0)
    |> Seq.sum
// 28.0

// iter example
let highlightRange =
    exampleRange
    |> iter (fun cell -> cell.Interior.Color <- 65535) // Yellow
// Entire range is yellow

// iterrc example
let chequerRange =
    exampleRange
    |> iterrc (fun r c cell -> if    (r % 2 = 0) && (c % 2 <> 0) 
                                    || (r % 2 <> 0) && (c % 2 = 0) then      
                                    cell.Interior.Color <- 65535 // Yellow
                                else
                                    cell.Interior.Color <- 255) // Red
// Range is fetchingly chequered in red and yellow

// filter and toRange example:
let colourOddInts =
    let oddIntRange = 
        exampleRange 
        |> filter (fun cell -> let cellVal = cellDouble cell
                               (cellVal = float(int(cellVal)))
                               && (int(cellVal)) % 2 <> 0)
        |> toRange exampleSheet
    oddIntRange.Interior.Color <- 255 // Red
// Cells containing odd integers are coloured red; other colours are unchanged