#load @"..\packages\Deedle.1.2.5\Deedle.fsx"
//#r "packages/Deedle.1.2.5/lib/net40/Deedle.dll"
#r @"..\packages\MathNet.Numerics.3.20.0\lib\net40\MathNet.Numerics.dll"
#r @"..\packages\MathNet.Numerics.FSharp.3.20.0\lib\net40\MathNet.Numerics.FSharp.dll"
#r @"..\packages\FSharp.Data.2.3.3\lib\net40\FSharp.Data.dll"
#load "RWTS.fs"

open System
open Deedle
open MathNet.Numerics.Statistics
open MathNet.Numerics.LinearAlgebra


//Mockup of time series service - here these are just grabbed from a CSV file
let root = "C:/Users/stewarca/Documents/Visual Studio 2015/Projects/FsLab Journal AnalVisDemo/FsLab Journal AnalVisDemo/data/TSExampleData/"
let TRIfromCSV = 
    Frame.ReadCsv<DateTime>(root + "TotalReturnIndices_EndDec2016_InColumns.csv", indexCol="Index", missingValues=[|"null"|])
    |> Frame.sortRowsByKey

let ThreeMRatesfromCSV = 
    Frame.ReadCsv<DateTime>(root + "ThreeMonthInterestRates_EndDec2016_InColumns.csv", indexCol="Index", missingValues=[|"null"|])
    |> Frame.sortRowsByKey

// Same deal for the prepackaged XS returns
let XSReturnsfromCSV = 
    Frame.ReadCsv<DateTime>(root + "HistoricReturnsQuarterly_EndDec2016_InColumns.csv", indexCol="Index", missingValues=[|"null"|])
    |> Frame.sortRowsByKey

// I can create a function to return all <float> values for each Asset
let ReturnSeries (allSeries: Frame<DateTime,string>) asset = 
    allSeries.GetColumn<string>(asset) |> Series.filter (fun k v -> v <> "null") |> Series.map (fun k v -> float v)

// And I can "bake in" the series to have a 1-argument function to return the required series
let returnTRISeries = ReturnSeries TRIfromCSV
let return3MSeries = ReturnSeries ThreeMRatesfromCSV
let returnXSReturnSeries = ReturnSeries XSReturnsfromCSV

let TRI = returnTRISeries "E_CNY"
let Rates3M = return3MSeries "E_CNY"
let testXSRetSeries = returnXSReturnSeries "E_CNY"

// Now use the RWTS module 

open FxSake.RWTS

// Unrelated to RWTS - try to get data from our model API

//open FxSake.RWTS
//open FSharp.Data
//open System

let fullDateRangeQuery  = urlQueryDateRange (DateTime(1850,1,1)) DateTime.Now

let testRWTSSeries = tsFrameFromAPI """http://lhr-wbsdiweb501/Api/timeseriespoints/Series.MarketData.Equity.TotalReturnIndex/RWTS/RW/NONE/NONE/EURSTOXX50/NONE""" fullDateRangeQuery

let testFullRWTSSeries = fullTimeSeriesFromAPI """http://lhr-wbsdiweb501/Api/timeseriespoints/Series.MarketData.Equity.TotalReturnIndex/RWTS/RW/NONE/NONE/E_NTH_M/NONE"""
let testFullDDLSeries = fullTimeSeriesFromAPI """http://lhr-wbsdiweb501/Api/timeseriespoints/Series.MarketData.Equity.TotalReturnIndex/DDL/NONE/NONE/NONE/AMSTERDAM_MIDKAP/NONE"""

// Is this the place to set dates to EOMonth?

// Start Date is the first key in the historic series
// End date will be set manually - this is where we decide we want to switch over (take at EOmonth to ensure it includes the last value)
let endDate = System.DateTime.Parse("2017-06-30") |> eoMonth

let cleanedSeries = combineMonthlySeries testFullDDLSeries testFullRWTSSeries endDate



// let's keep going - take in the 3M series too
let testFullRWTS3MRates = fullTimeSeriesFromAPI """http://lhr-wbsdiweb501/Api/timeseriespoints/Series.MarketData.SpotRate.3m/RWTS/RW/NONE/NTH/NONE/NONE"""
let testFullDDL3MRatess = fullTimeSeriesFromAPI """http://lhr-wbsdiweb501/Api/timeseriespoints/Series.MarketData.SpotRate.3m/DDL/NONE/NONE/EUR/NONE/NONE"""

let cleaned3M = combineMonthlySeries testFullDDL3MRatess testFullRWTS3MRates endDate

// Can we now do Timeseries functions on that data?
let cleanLogAnnReturns = logReturnMonthly cleanedSeries

let threeMToOneMReturns = log(1.0 + cleaned3M.Shift(1) / 1200.0)

let excessReturns = excessReturn cleanLogAnnReturns threeMToOneMReturns

let test3MSettings = {lambda = 0.75; initValue = 0.003}

ewmaVolatility test3MSettings 12 excessReturns

// Should we test that this data is valid monthly data?

let gaps = cleanedSeries |> Series.keys |> Seq.pairwise |> Seq.map (fun dates -> snd dates - fst dates )
let min = Seq.min gaps
let max = Seq.max gaps
let withinRange = min >= (TimeSpan(28,0,0,0)) && max <= (TimeSpan(31,0,0,0))

let testTSGaps timeSeries (maxDays:TimeSpan) (minDays:TimeSpan) = 
    let gaps = timeSeries |> Series.keys |> Seq.pairwise |> Seq.map (fun dates -> snd dates - fst dates )
    let min = Seq.min gaps
    let max = Seq.max gaps
    let withinRange = min >= minDays && max <= maxDays
    withinRange

let seriesIsValidMonthly (timeSeries: TimeSeries) = 
    testTSGaps timeSeries (TimeSpan(31,0,0,0)) (TimeSpan(28,0,0,0))

seriesIsValidMonthly cleanedSeries

let returnsWithMissing = cleanedSeries |> Series.filter (fun k v -> k.Equals(eoMonth(endDate)) = false)

seriesIsValidMonthly returnsWithMissing

// Repeating the CSV data, this time replaced by data from the (historic) time series service

let TRI' = fullTimeSeriesFromAPI """http://lhr-wbsdiweb501/Api/timeseriespoints/Series.MarketData.Equity.TotalReturnIndex/RWTS/RW/NONE/NONE/E_CNY/NONE"""
let Rates3M' = fullTimeSeriesFromAPI """http://lhr-wbsdiweb501/Api/timeseriespoints/Series.MarketData.SpotRate.3m/RWTS/RW/NONE/CNY/NONE/NONE"""




// Let's compare the data from the 2 methods - clean up the Date keys by making everything end-of month)

let joinTRICSVtoAPI = [ 
        "CSV" => (TRI |> eoMonthSeries) ;  
        "API" => (TRI' |> eoMonthSeries) ] |> Frame.ofColumns

// Watch out for the difference in scale
let join3MCSVtoAPI = [ 
        "CSV" => (Rates3M |> eoMonthSeries) ;  
        "API" => (Rates3M' |> eoMonthSeries |> Series.mapValues (fun v -> v / 100.0)) ] |> Frame.ofColumns



let testXSRetSeries' = returnXSReturnSeries "E_CNY"

// Test with CSV data


let dateArray = [| DateTime(2014,12,31); DateTime(2015,12,31); DateTime(2016,12,31) |]
let valArray = [| 100.0; 110.0; 120.0 |]

timeSeriesFromArrays dateArray valArray


let quarterlyTRI = logReturnQuarterly TRI
let comp3MRates = contCompounded3MRates Rates3M

dateLookupInTimeSeries quarterlyTRI (DateTime(2015,12,31))
dateLookupInTimeSeries comp3MRates (DateTime(2015,12,31))

// Once more, with API data
let quarterlyTRI' = logReturnQuarterly TRI'
let comp3MRates' =  Rates3M' |> eoMonthSeries |> Series.mapValues (fun v -> v / 100.0) |> contCompounded3MRates

dateLookupInTimeSeries quarterlyTRI' (DateTime(2015,12,31))

// Datetime lookup is a bit weird, if we want to find on a particular day, we need the time as well
let dateLookupInTimeSeries' (timeSeries: TimeSeries) (date: DateTime) =  
        let endOfDay = DateTime(date.Year,date.Month, date.Day, 23, 59, 59, 999)       
        timeSeries.Get(endOfDay)

dateLookupInTimeSeries' comp3MRates' (DateTime(2015,12,31))

// Ecess returns

let xsReturns = excessReturn quarterlyTRI comp3MRates

let quarterlyXSReturns = resampleMonthlySeries 4 xsReturns

let settingLambda = 0.98
let settingInitialVal = 0.031102706532478
let seriesPeriod = 4

let testSettings = {lambda = settingLambda; initValue = settingInitialVal}

ewmaVariance testSettings quarterlyXSReturns

ewmaVolatility testSettings seriesPeriod quarterlyXSReturns

// Correlation Tests

let calibrationDate = DateTime(2016, 12, 31)

let seriesLH = returnXSReturnSeries "ASX_200_A_REIT"
let seriesRH = returnXSReturnSeries "ASX_200_BANKS" 

tenYearCorrelation calibrationDate (returnXSReturnSeries "ASX_200_A_REIT") (returnXSReturnSeries "ASX_200_BANKS" ) // = 0.6329625501
// pair with a missing value 
tenYearCorrelation calibrationDate (returnXSReturnSeries "EURSTOXX50") (returnXSReturnSeries "EUR_BankLoans" ) // = 0.5962678681

// Unconditional correlation
getUncCorrelFromPair 0.99 0.005625 0.5 (DateTime(2009, 12, 31)) (returnXSReturnSeries "E_AUD") (returnXSReturnSeries "E_HKD") //  = 0.53407839

getUncCorrelFromPair 0.99 0.005625 0.5 (DateTime(2009, 12, 31)) (returnXSReturnSeries "E_HKD") (returnXSReturnSeries "E_AUD") //  = 0.53407839, still

