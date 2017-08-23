#load @"..\packages\Deedle.1.2.5\Deedle.fsx"
//#r "packages/Deedle.1.2.5/lib/net40/Deedle.dll"
#r @"..\packages\MathNet.Numerics.3.20.0\lib\net40\MathNet.Numerics.dll"
#r @"..\packages\MathNet.Numerics.FSharp.3.20.0\lib\net40\MathNet.Numerics.FSharp.dll"
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


let dateArray = [| DateTime(2014,12,31); DateTime(2015,12,31); DateTime(2016,12,31) |]
let valArray = [| 100.0; 110.0; 120.0 |]

timeSeriesFromArrays dateArray valArray


let quarterlyTRI = logReturnQuarterly TRI
let comp3MRates = contCompounded3MRates Rates3M

dateLookupInTimeSeries quarterlyTRI (DateTime(2015,12,31))

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