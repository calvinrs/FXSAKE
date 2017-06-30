﻿namespace FxSake

module MyFunctions = 

    open ExcelDna.Integration    
    open ExcelHelpers

    [<ExcelFunction(Description="My first .NET function")>]
    let HelloDna name = 
        "Hello " + name

    [<ExcelFunction(Description="My second .NET function")>]
    let GoodByeDna name = 
        "Goodbye " + name
    
    

    [<ExcelFunction(Description="Return an index in years for an array of numbers, given the frequency in steps per year")>]
    let nuallZCB stepsPerYear (myZCB: float[]) =         
        let isTall = XlCall.Excel(XlCall.xlfCaller) :?> ExcelReference |> isTallRange
        let myIndex = [| 1..(Array.length myZCB) |]
        let timeIndex = myIndex |> Array.map (fun i -> float(i) * 1.0 / stepsPerYear)
        if isTall then Array2D.init (Array.length timeIndex) 1 (fun i j -> match j with |0 -> timeIndex.[i] |_ -> 0.0)
        else Array2D.init 1 (Array.length timeIndex) (fun i j -> match i with |0 -> timeIndex.[j] |_ -> 0.0) 
        
    

    let ans = nuallZCB 12.0 [| 1.0..10.0 |]


    [<ExcelFunction(Description="Convert a ZCB price to an n-uall spot rate. If stepsPerYear is not set, the output rate is continuously compounded")>]
    let ZCBtoSpot time price (stepsPerYear: obj) = 
        match stepsPerYear with
        | :? ExcelMissing -> -log(price)/time
        | _ ->  unbox stepsPerYear * ( (1.0 / price) ** (1.0 / time * (1.0 / unbox stepsPerYear)) - 1.0)     
 