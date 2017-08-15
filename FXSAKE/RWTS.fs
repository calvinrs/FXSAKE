namespace FxSake

module RWTS = 
    
    open System
    open Deedle

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
       


    // Excess Returns

    let lambdaFromMeanAge obsPerAnnum meanAge = 1.0 - (1.0 / obsPerAnnum) / meanAge

    let logReturnOverMonths months (returns: TimeSeries) = 
        log(returns) - log(returns.Shift(months)) |> Series.dropMissing

    let logReturnQuarterly (returns: TimeSeries) = logReturnOverMonths 3 returns
    let logReturnAnnual (returns: TimeSeries) = logReturnOverMonths 12 returns

    let contCompounded3MRates (rates: TimeSeries) = 
        0.25 * log(1.0 + rates.Shift(3))

    let excessReturn (returns: TimeSeries) (rates: TimeSeries) = 
        let initDate = returns.GetKeyAt(0)
        let truncRate = rates |> Series.filter (fun k v ->  k >= initDate)
        let joinTRIToRates = [ 
          "Returns" => returns;  
          "Rates" => truncRate ] |> Frame.ofColumns
        let fullXSReturns = joinTRIToRates?Returns - joinTRIToRates?Rates |> Series.fillMissingUsing (fun k -> joinTRIToRates?Returns.Get(k)) // TODO: This fills dates without rates with the TRI only - is this the right thing to do?
        fullXSReturns

    let resampleSeries seriesPeriodPerAnnum (series: TimeSeries) =
        let seriesPeriodInMonths = (12  / seriesPeriodPerAnnum)
        series |> Series.filter (fun k v -> k.Month % seriesPeriodInMonths = 0)

    // UNIVARIATE  
    // EWMA Calculations

    let ewmaVariance (settings: EWMASettings) (xsReturns: TimeSeries) =
        let {lambda = lambda; initValue = initialVal} = settings
        let initDate = xsReturns.GetKeyAt(0)
        let initPoint = Series([initDate], [initialVal])
        let variance = 
            xsReturns   
            |> Series.filter (fun k v -> k.Equals(initDate) = false)  
            |> Series.scanValues (fun prevVariance currentReturn -> lambda * prevVariance + (1.0 - lambda) * currentReturn ** 2.0 ) initialVal
            |> Series.merge initPoint        
        variance

    let ewmaVolatility (settings: EWMASettings) seriesPeriodPerAnnum (xsReturns: TimeSeries) =       
        let variance = ewmaVariance settings xsReturns            
        //Finally, we convert the variance to an annualised volatility. 
        let seriesPeriodInMonths = float (12  / seriesPeriodPerAnnum)
        let volatility = (12.0 / seriesPeriodInMonths) ** 0.5 * variance ** 0.5
        volatility  

    let ewmaAverage (settings: EWMASettings) (series: TimeSeries) =
        let {lambda = lambda; initValue = initialVal} = settings
        let initDate = series.GetKeyAt(0)
        let initPoint = Series([initDate], [initialVal])
        let average = 
            series   
            |> Series.filter (fun k v -> k.Equals(initDate) = false)  // TODO: is this step required - the average example does not start at init, but at the first calculation
            |> Series.scanValues (fun prevValue curValue -> lambda * prevValue + (1.0 - lambda) * curValue ) initialVal
            |> Series.merge initPoint        
        average