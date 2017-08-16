namespace FxSake

module MyFunctions = 
    
    open System
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

    // String Manipulation

    [<ExcelFunction(Description="Wrap an array of values as as single string for entry into a SQL INSERT VALUES statement.")>]
    let sqlValuesList (things: obj[]) = 
        things |> Array.map StringManipulation.singleQuotes |> Array.reduce (StringManipulation.commaDelimeter) |> sprintf ",(%s)"
    
    [<ExcelFunction(Description="Convert a date to a SQL-compatable YYYYMMDD format.")>]
    let sqlDateFormat (date:DateTime) = 
        StringManipulation.sqlDate () date

    // SVJD Term Structure

    open SVJD_TermStructure

    [<ExcelFunction(Description="Return a SVJD term structure, given the SVJD Model and its end-points.")>]
    let svjdTermStructure (startVol: float) (uncVol: float) (reversionSpeed: float) (volOfVariance: float) (correlation: float) (jumpIntensity: float) (jumpMean: float) (jumpVol: float) (monthlyTerms: float[]) =        
        let roundedMonthlyTerms = monthlyTerms |> Array.map int
        let svjdParameters =   {reversionLevel = uncVol * uncVol; 
                                reversionSpeed = reversionSpeed;
                                startVal = startVol * startVol; 
                                volOfVariance = volOfVariance; 
                                Correlation = correlation; 
                                jumpIntensity = jumpIntensity; 
                                jumpMean = jumpMean;
                                jumpVol = jumpVol }  
        let timescaleSVJD = 1.0 / 12.0
        let extrapolatedSVJDTermStructure = getSVJDInterpolatedVols svjdParameters timescaleSVJD roundedMonthlyTerms
        extrapolatedSVJDTermStructure |> Array.map snd |> arrayDirectionHelper (XlCall.Excel(XlCall.xlfCaller))    

    // RWTS Functions

    open RWTS

    [<ExcelFunction(Description="Take a series of returns, and calculate the log change of the entire series.")>]
    let logReturnsQuarterly (dateasFloatArray: float[]) (valueArray: float[]) =
        let dateArray = dateasFloatArray |> Array.map (fun d -> DateTime.FromOADate(d))
        let returns = timeSeriesFromArrays dateArray valueArray |> logReturnQuarterly
        let resultArray = evaluateTimeSeriesOverDates returns dateArray
        resultArray |> arrayDirectionHelper (XlCall.Excel(XlCall.xlfCaller))    

    [<ExcelFunction(Description="Take a time series, and calculate the EWMA weighted average of the at each date.")>]
    let ewmaSeriesAverage (dateasFloatArray: float[]) (valueArray: float[]) (lambda: float) (initValue: float) =
        let settings = {lambda = lambda; initValue = initValue}
        let dateArray = dateasFloatArray |> Array.map (fun d -> DateTime.FromOADate(d))
        let averages = timeSeriesFromArrays dateArray valueArray |> ewmaAverage settings
        let resultArray = evaluateTimeSeriesOverDates averages dateArray
        resultArray |> arrayDirectionHelper (XlCall.Excel(XlCall.xlfCaller)) 

    [<ExcelFunction(Description="Take a series of (excess) returns, and calculate the EWMA Volatility of the series at the final date.")>]
    let ewmaReturnVolatility (dateasFloatArray: float[]) (returnsArray: float[]) (lambda: float) (initValue: float) (seriesPeriodInMonths: float) =
        let settings = {lambda = lambda; initValue = initValue}
        let dateArray = dateasFloatArray |> Array.map (fun d -> DateTime.FromOADate(d))
        let returns = timeSeriesFromArrays dateArray returnsArray |> resampleMonthlySeries (int seriesPeriodInMonths) // note we are assuming a monthly series, and resampling
        let volatilitySeries = ewmaVolatility settings (int seriesPeriodInMonths) returns
        let lastDate = volatilitySeries.Keys |> Seq.toList |> List.rev |> List.head
        dateLookupInTimeSeries volatilitySeries lastDate