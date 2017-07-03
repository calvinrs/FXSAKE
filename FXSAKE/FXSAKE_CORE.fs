namespace FxSake

module MyFunctions = 

    open ExcelDna.Integration    
    open ExcelHelpers
    open YieldCurves  

    // TEST FUNCTIONS
    [<ExcelFunction(Description="My first .NET function")>]
    let helloDna name = 
        "Hello " + name

    [<ExcelFunction(Description="My second .NET function")>]
    let GoodByeDna name = 
        "Goodbye " + name    
    
    // YIELD CURVE OPERATIONS
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
    
    [<ExcelFunction(Description="Convert a ZCB curve to an n-uall spot rate. If stepsPerYear is not set, the output rate is continuously compounded")>]         
    let zcbToSpotRates (time: float[]) (price: float[]) (stepsPerYear: obj) = 
        let zcbCurve = createZCBCurve time price        
        let compoundType =  match stepsPerYear with
                            | :? ExcelMissing | :? ExcelEmpty  -> 0.0 |> getCompounding                              
                            | _ ->  stepsPerYear |> unbox |> getCompounding 
        let spotCurve = getSpotCurveFromZCBCurve compoundType zcbCurve
        let spotRates = spotCurve |> unpackSpotCurve |> extractCurveValue
        spotRates |> arrayDirectionHelper (XlCall.Excel(XlCall.xlfCaller))

    [<ExcelFunction(Description="Convert a spot rate to a ZCB curve, given a compounding frequency for the spot rates. If stepsPerYear is not set, the output rate is continuously compounded")>] 
    let spotCurveToZCBPrices (time: float[]) (spot: float[]) (stepsPerYear: obj) = 
        let spotCurve = createSpotCurve time spot 
        let compoundType =  match stepsPerYear with
                            | :? ExcelMissing | :? ExcelEmpty  -> 0.0 |> getCompounding                              
                            | _ ->  stepsPerYear |> unbox |> getCompounding 
        let zcbCurve = getZCBCurveFromSpotCurve compoundType spotCurve
        let zcbPrices = zcbCurve |> unpackZCBCurve |> extractCurveValue
        zcbPrices |> arrayDirectionHelper (XlCall.Excel(XlCall.xlfCaller))

    [<ExcelFunction(Description="Convert a ZCB curve to simply compounded forward rate. The forward curve will start at t=0, so if the t=0 ZCB price is excluded from the input, this will be offset by one place.")>]         
    let zcbToForwardRates (time: float[]) (price: float[]) (stepsPerYear: obj) = 
        let zcbCurve = createZCBCurve time price    
        let fwdCurve = zcbCurve |> simplyCompoundedForwardCurve
        let fwdRates = fwdCurve |> unpackForwardCurve |> extractCurveValue
        fwdRates |> arrayDirectionHelper (XlCall.Excel(XlCall.xlfCaller))

    [<ExcelFunction(Description="Interpolate over an annual ZCB curve and return a monthly ZCB curve - this includes the terms, so return this over 2 columns.")>]         
    let zcbAnnCurveMonthlyInterpolation (time: float[]) (price: float[]) =
        let annZcbCurve = createZCBCurve time price  
        let monZcbCurveArray = annZcbCurve |> interpolateAnnZCBCurveAsMonthly |> curveTo2DArray        
        monZcbCurveArray

    // MATRIX ALGEBRA
    [<ExcelFunction(Description="Determine if a correlation matrix is PSD - i.e. all eigenvalues are positive.")>]
    let isMatrixPSD (myMatrix: float[,]) = 
        FXSEMatrixAlgebra.xlsMatrixIsPSD myMatrix

    [<ExcelFunction(Description="Return the eigenvalues of a square matrix.")>]
    let getEigenValues(myMatrix: float[,]) = 
        let evArray = FXSEMatrixAlgebra.xlsEigenValues myMatrix
        evArray |> arrayDirectionHelper (XlCall.Excel(XlCall.xlfCaller))