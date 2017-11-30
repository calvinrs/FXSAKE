namespace FxSake

module RWTS = 
    
    open System
    open System.Net
    open Deedle
    open MathNet.Numerics.Statistics
    open MathNet.Numerics.LinearAlgebra
    open FSharp.Data

    // TYPES

    type TimeSeries = Series<DateTime,float>

    type EWMASettings = { lambda: float ; initValue: float }
   
    // Helpers

    let timeSeriesFromArrays dateArray valueArray =   
        let dateValArray = valueArray |> Array.toSeq |>  Seq.zip (dateArray |> Array.toSeq)        
        let dateValTimeSeries:TimeSeries =  dateValArray |>  Series.ofObservations
        dateValTimeSeries

    let dateLookupInTimeSeries (timeSeries: TimeSeries) (date: DateTime) =         
        timeSeries.Get(date)

    let evaluateTimeSeriesOverDates (timeSeries: TimeSeries) (dateArray: DateTime[]) = 
        let keys = dateArray |> Array.toSeq
        timeSeries |> Series.lookupAll keys Lookup.Exact |> Series.values |> Seq.toArray
       
    let cartesianProduct xs ys = 
        xs |> List.collect (fun x -> ys |> List.map (fun y -> x, y))    
    
    // End-of-month date calculations
    let eoMonth (date: System.DateTime) = 
        let year = date.Year
        let month = date.Month
        let day = System.DateTime.DaysInMonth(year, month)
        let eoMonth = System.DateTime(year, month, day, 23, 59, 59, 999)
        eoMonth

    let eoMonthString date = 
        let parsedDate = System.DateTime.Parse(date)
        eoMonth parsedDate

    let monthEndsBetween (firstDate: System.DateTime) (lastDate: System.DateTime) = 
        let firstEoMonth = eoMonth firstDate
        Seq.unfold (fun d -> if d <= lastDate then Some(d, eoMonth(d.AddMonths(1))) else None) firstEoMonth
        |> Seq.toList

    // TimeSeries API calls

    type TimeSeriesPoint = { Date: DateTime; Value: float }

    let dateToUniversalString (date: DateTime) = 
        date.ToString("u") |> String.map (fun c -> if c = ' ' then 'T' else c)

    let urlQueryDateRange  (startDate: DateTime)  (endDate: DateTime) = 
        [("minValueDateUtc",dateToUniversalString startDate);("maxValueDateUtc",dateToUniversalString endDate)]

    let tsFrameFromAPI url query = 
        // Helper function to extract the response body
        let httpText responseBody = 
            match responseBody with
            | Text text -> text
            | Binary bytes -> sprintf "Got %d bytes of binary content" bytes.Length  
        let response = 
          Http.Request(url, 
              query, 
              customizeHttpRequest = fun r ->
              r.Credentials <- CredentialCache.DefaultNetworkCredentials
              r) 
        let timeSeriesData = JsonValue.Parse(httpText response.Body)
        let jsonArray = timeSeriesData.Properties() |> Array.map (fun (k, v) ->  { Date =  System.DateTime.Parse(k); Value = v.AsFloat() } )
        let tsFrame = Frame.ofRecords jsonArray |> Frame.indexRowsDate "Date" |> Frame.sortRowsByKey
        tsFrame

    let fullTsFrameFromAPI url = tsFrameFromAPI url (urlQueryDateRange (DateTime(1850,1,1)) DateTime.Now) 

    // Makes mode sense for TimeSeries to be a Deedle "Series", no?
    let timeSeriesFromAPI url query =  
        let frame = tsFrameFromAPI url query
        frame?Value

    let fullTimeSeriesFromAPI url = timeSeriesFromAPI url (urlQueryDateRange (DateTime(1850,1,1)) DateTime.Now) 


    // The final "join" function?
    let combineMonthlySeries (ongoingSeries: TimeSeries) (historicSeries: TimeSeries) lastHistoricDate =     
        let startDate = historicSeries |> Series.mapKeys (eoMonth) |> Series.firstKey
        let maxDate =  ongoingSeries |> Series.mapKeys (eoMonth) |> Series.lastKey
        let expectedMonthEnds = monthEndsBetween startDate maxDate
        let eoMonthHistSeries = historicSeries |> Series.mapKeys (eoMonth) |> Series.filter (fun k v -> k <= lastHistoricDate ) |> Series.lookupAll expectedMonthEnds Lookup.Exact
        let eoMonthOngoingSeries = ongoingSeries |> Series.mapKeys (eoMonth) |> Series.filter (fun k v -> k > lastHistoricDate ) |> Series.lookupAll expectedMonthEnds Lookup.Exact
        let joinHistToOngoing = [ 
            "Historic" => eoMonthHistSeries ;  
            "Ongoing" => eoMonthOngoingSeries ] |> Frame.ofColumns
        let fullReturns = joinHistToOngoing?Ongoing |> Series.fillMissingUsing (fun k -> joinHistToOngoing?Historic.Get(k))
        fullReturns
    
    // Clean up the datetimes of a series so that they are end-of-month, and rejoin to the expected months in the series to ensure the data is complete on a monthly frequency
    let eoMonthSeries series = 
        let startDate = series |> Series.mapKeys (eoMonth) |> Series.firstKey
        let endDate =  series |> Series.mapKeys (eoMonth) |> Series.lastKey
        let expectedMonthEnds = monthEndsBetween startDate endDate
        series |> Series.mapKeys (eoMonth) |> Series.lookupAll expectedMonthEnds Lookup.Exact

    // TS Integrity checks
    let testTSGaps timeSeries (maxDays:TimeSpan) (minDays:TimeSpan) = 
        let gaps = timeSeries |> Series.keys |> Seq.pairwise |> Seq.map (fun dates -> snd dates - fst dates )
        let min = Seq.min gaps
        let max = Seq.max gaps
        let withinRange = min >= minDays && max <= maxDays
        withinRange

    let seriesIsValidMonthly (timeSeries: TimeSeries) = 
        testTSGaps timeSeries (TimeSpan(31,0,0,0)) (TimeSpan(28,0,0,0))

    // Excess Returns

    let lambdaFromMeanAge obsPerAnnum meanAge = 1.0 - (1.0 / obsPerAnnum) / meanAge

    let logReturnOverMonths months (returns: TimeSeries) = 
        log(returns) - log(returns.Shift(months)) |> Series.dropMissing
    
    let logReturnMonthly (returns: TimeSeries) = logReturnOverMonths 1 returns
    let logReturnQuarterly (returns: TimeSeries) = logReturnOverMonths 3 returns
    let logReturnAnnual (returns: TimeSeries) = logReturnOverMonths 12 returns

    let contCompounded3MRates (rates: TimeSeries) = 
        0.25 * log(1.0 + rates.Shift(3))

    let contCompoundeThreeMToOneMReturns (rates: TimeSeries) = log(1.0 + rates.Shift(1) / 1200.0) // For conversion of 3M rates for 1m vol calculations

    let excessReturn (returns: TimeSeries) (rates: TimeSeries) = 
        let initDate = returns.GetKeyAt(0)
        let truncRate = rates |> Series.filter (fun k v ->  k >= initDate)
        let joinTRIToRates = [ 
          "Returns" => returns;  
          "Rates" => truncRate ] |> Frame.ofColumns
        let fullXSReturns = joinTRIToRates?Returns - joinTRIToRates?Rates |> Series.fillMissingUsing (fun k -> joinTRIToRates?Returns.Get(k)) // TODO: This fills dates without rates with the TRI only - is this the right thing to do?
        fullXSReturns

    let resampleMonthlySeries seriesPeriodPerAnnum (series: TimeSeries) =
        let seriesPeriodInMonths = (12  / seriesPeriodPerAnnum)
        series |> Series.filter (fun k v -> k.Month % seriesPeriodInMonths = 0)

    // UNIVARIATE  
    // EWMA Calculations

    let ewmaVariance (settings: EWMASettings) (series: TimeSeries) =
        let {lambda = lambda; initValue = initialVal} = settings
        let initDate = series.GetKeyAt(0)
        let initPoint = Series([initDate], [initialVal])
        let variance = 
            series   
            |> Series.filter (fun k v -> k.Equals(initDate) = false)  
            |> Series.scanValues (fun prevVariance currentReturn -> lambda * prevVariance + (1.0 - lambda) * currentReturn ** 2.0 ) initialVal
            |> Series.merge initPoint        
        variance

    let ewmaVolatility (settings: EWMASettings) seriesPeriodPerAnnum (series: TimeSeries) =       
        let variance = ewmaVariance settings series            
        //Finally, we convert the variance to an annualised volatility. 
        let seriesPeriodInMonths = float (12  / seriesPeriodPerAnnum)
        let volatility = (12.0 / seriesPeriodInMonths) ** 0.5 * variance ** 0.5
        volatility  

    let ewmaAverage (settings: EWMASettings) (series: TimeSeries) =
        let {lambda = lambda; initValue = initialVal} = settings
        let initDate = series.GetKeyAt(0)
        let initPoint = Series([initDate], [initialVal])
        let ave = 
            series   
            |> Series.filter (fun k v -> k.Equals(initDate) = false)  // TODO: is this step required - the average example does not start at init, but at the first calculation
            |> Series.scanValues (fun prevValue curValue -> lambda * prevValue + (1.0 - lambda) * curValue ) initialVal
            |> Series.merge initPoint        
        ave

    // MULTIVARIATE

    let tenYearCorrelation (calibrationDate: DateTime) (assetLHSeries: TimeSeries) (assetRHSeries: TimeSeries) = 
        
        let combinedStartDate = if assetLHSeries.FirstKey() > assetRHSeries.FirstKey() then assetLHSeries.FirstKey() else assetRHSeries.FirstKey()
        let tenYearsAgo = calibrationDate.AddYears(-10)  
        let exampleReturnDataFrame = Frame.ofColumns [ assetLHSeries => assetLHSeries; assetRHSeries => assetRHSeries ]  
        let exampleHist10YReturns = exampleReturnDataFrame |> Frame.filterRows (fun k v -> k >= combinedStartDate && k > tenYearsAgo && k <= calibrationDate)

        let myX = exampleHist10YReturns.GetColumn<float>(assetLHSeries).ValuesAll
        let myY = exampleHist10YReturns.GetColumn<float>(assetRHSeries).ValuesAll

        let myCorrelXY = Correlation.Pearson(myX, myY)
        myCorrelXY

    let getUncCorrelFromPair lambda uncVarInit covarScale (calibrationDate: DateTime) (assetLHSeries: TimeSeries) (assetRHSeries: TimeSeries)  =      
   
        let seriesLH = assetLHSeries.EndAt(calibrationDate)
        let seriesRH = assetRHSeries.EndAt(calibrationDate)

        // we will need to take the mean value for each series. This is over the entire lifespan of each series.   
        let seriesMeanLHS = seriesLH.Mean()
        let seriesMeanRHS = seriesRH.Mean()

        // For the EWMA part of this calculation, we need to start at the first point in time where both series are avaliable
        let combinedStartDate = if seriesLH.FirstKey() > seriesRH.FirstKey() then seriesLH.FirstKey() else seriesRH.FirstKey()

        // we can now truncate the data series, and from now on will be working with series of equal length
        let ewmaReturnsLH = seriesLH.StartAt(combinedStartDate)
        let ewmaReturnsRH = seriesRH.StartAt(combinedStartDate)

        // Use the Series mu (over all points) rather than the mean of the ewma period, to calculate the covariance
        let seriesLHMinusMu = ewmaReturnsLH - seriesMeanLHS
        let seriesRHMinusMu = ewmaReturnsRH - seriesMeanRHS
        let ewmaCovar = seriesLHMinusMu * seriesRHMinusMu

        let uncCovarInit = uncVarInit * covarScale

        // EWMA of series
        let ewmaWeightedCovar = 
            ewmaCovar      
            |> Series.scanValues (fun lastweightedCovar thisCovar -> lambda * lastweightedCovar + (1.0 - lambda) * thisCovar ) uncCovarInit
    
        let ewmaVarLH = 
            seriesLHMinusMu      
            |> Series.scanValues (fun lastweightedVar thisVar -> lambda * lastweightedVar + (1.0 - lambda) * thisVar ** 2.0 ) uncVarInit

        let ewmaVarRH = 
            seriesRHMinusMu      
            |> Series.scanValues (fun lastweightedVar thisVar -> lambda * lastweightedVar + (1.0 - lambda) * thisVar ** 2.0 ) uncVarInit

        let ewmaCorrel = ewmaWeightedCovar / (ewmaVarLH * ewmaVarRH) ** 0.5
        // finally, the contribution to the current unconditional correlation matrix for this pair is the ewmaCorrel value at the calibration date
        ewmaCorrel.Get(calibrationDate)