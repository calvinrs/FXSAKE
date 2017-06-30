﻿namespace FxSake

module MyFunctions = 

    open ExcelDna.Integration    
    open ExcelHelpers    

    [<ExcelFunction(Description="My first .NET function")>]
    let helloDna name = 
        "Hello " + name

    [<ExcelFunction(Description="My second .NET function")>]
    let GoodByeDna name = 
        "Goodbye " + name    

    [<ExcelFunction(Description="Return an index in years for an array of numbers, given the frequency in steps per year")>]
    let nuallZCB stepsPerYear (myZCB: float[]) = 
        let myIndex = [| 1..(Array.length myZCB) |]
        let timeIndex = myIndex |> Array.map (fun i -> float(i) * 1.0 / stepsPerYear)
        timeIndex |> arrayDirectionHelper (XlCall.Excel(XlCall.xlfCaller))
    
     
    [<ExcelFunction(Description="Convert a ZCB price to an n-uall spot rate. If stepsPerYear is not set, the output rate is continuously compounded")>]
    let zcbToSpot time price (stepsPerYear: obj) = 
        match stepsPerYear with
        | :? ExcelMissing -> -log(price)/time
        | _ ->  unbox stepsPerYear * ( (1.0 / price) ** (1.0 / time * (1.0 / unbox stepsPerYear)) - 1.0)     
 
    // MATRIX ALGEBRA
    [<ExcelFunction(Description="Determine if a correlation matrix is PSD - i.e. all eigenvalues are positive.")>]
    let isMatrixPSD (myMatrix: float[,]) = 
        FXSEMatrixAlgebra.xlsMatrixIsPSD myMatrix

    [<ExcelFunction(Description="Return the eigenvalues of a square matrix.")>]
    let getEigenValues(myMatrix: float[,]) = 
        let evArray = FXSEMatrixAlgebra.xlsEigenValues myMatrix
        evArray |> arrayDirectionHelper (XlCall.Excel(XlCall.xlfCaller))