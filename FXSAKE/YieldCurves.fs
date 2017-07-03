namespace FxSake

module YieldCurves = 

    type ZCBPrices = ZCBPrices of float array
    type ZCBIndexed = ZCBIndexed of (int * float) array
    type ZCBCurve = ZCBCurve of (float * float) array

    type Compounding = Continuous | Monthly | SemiAnnual | Annual

    type SpotRates = SpotRates of float array
    type SpotCurve = SpotCurve of (float * float) array

    type ForwardCurve = ForwardCurve of (float * float) array

    // HELPER FUNCTIONS
    let createZCBCurve (time: float[]) (price: float[]) = 
        let myZCBCurve =  Array.zip time price
        ZCBCurve myZCBCurve
    
    let createSpotCurve (time: float[]) (spot: float[]) = 
        let mySpotCurve =  Array.zip time spot
        SpotCurve mySpotCurve

    let unpackSpotCurve (SpotCurve mySpot) = mySpot
    let unpackZCBCurve (ZCBCurve myZCB) = myZCB
    let unpackForwardCurve (ForwardCurve myFwd) = myFwd

    let extractCurveValue (curve: (float * float) array) =        
        curve |> Array.map (snd)
    

    // ZCB AND SPOTS

    let getSpotCurveFromZCBCurve compMethod (ZCBCurve timedZCB) = 
        // Spot rate methods as helper functions
        let spotRates stepsPerYear timedZCB = SpotCurve ( timedZCB |> Array.map (fun (t, z) -> (t, stepsPerYear * ( (1.0 / z) ** (1.0 / t * (1.0 / stepsPerYear)) - 1.0))))
        let contCompRates timedZCB = SpotCurve (timedZCB |> Array.map (fun (t, z) -> (t, -log(z)/t)))
        // Match on the method to determine which method to use
        match compMethod with
        | Continuous -> timedZCB |> contCompRates
        | Monthly -> timedZCB |> spotRates 12.0
        | SemiAnnual  -> timedZCB |> spotRates 2.0
        | Annual -> timedZCB |> spotRates 1.0

    let getCompounding stepsPerYear = match stepsPerYear with
        | 12.0 -> Monthly
        | 2.0 -> SemiAnnual
        | 1.0 -> Annual
        | _ -> Continuous
    
    let withZeroZCB (ZCBCurve aZCBCurve) = 
        let outCurve = match fst aZCBCurve.[0] with
                        | 0.0 -> aZCBCurve
                        | _ -> Array.append [|(0.0, 1.0)|] aZCBCurve
        ZCBCurve outCurve

    let getZCBCurveFromSpotCurve compMethod (SpotCurve timedSpots) = 
        let contSpotToZCB spotCurve = ZCBCurve (spotCurve |> Array.map (fun (t, s) -> (t, exp (-s * t))))
        let nuallSpotToZCB frequency spotCurve = ZCBCurve (spotCurve |> Array.map (fun (t, s)-> (t, 1.0 / (1.0 + (s / frequency)) ** (t * frequency))))
        match compMethod with
        | Continuous -> timedSpots |> contSpotToZCB
        | Monthly -> timedSpots |> nuallSpotToZCB 12.0
        | SemiAnnual  -> timedSpots |> nuallSpotToZCB 2.0
        | Annual -> timedSpots |> nuallSpotToZCB 1.0

    // FORWARD RATES

    let implyScaleFactor (ZCBCurve aZCBCurve) =     
        (fst aZCBCurve.[1]) -  (fst aZCBCurve.[0])
    
    let simplyCompoundedForwardCurve (aZCBCurve) = 
        let (ZCBCurve fullZCBCurve) = withZeroZCB aZCBCurve
        let pairZCBs = fullZCBCurve |> Array.toSeq |> Seq.pairwise //Array.pairwise avaliable from F# 4.0 - convert this to a Seq and back to get this effect
        let scaleFactor = (ZCBCurve fullZCBCurve) |> implyScaleFactor    
        let forwardRates = pairZCBs |> Seq.map (fun ((t1,z1),(t2,z2)) -> (t1, ((1.0 / scaleFactor) * (z1 / z2) - 1.0)) ) |> Seq.toArray
        ForwardCurve forwardRates

    // INTERPOLATION - using MathNet.numerics

    open MathNet.Numerics
    open MathNet.Numerics.Interpolation

    let interpolateAnnZCBCurveAsMonthly (ZCBCurve annZCBCurve) =         
        let fullAnnZCBCurve = (ZCBCurve annZCBCurve |> withZeroZCB)
        // Use the Interpolate method to seed the cubic spline method
        let curveTerms = fullAnnZCBCurve |> unpackZCBCurve |> Array.map(fst) |> Array.toSeq
        let curvePrices = fullAnnZCBCurve |> unpackZCBCurve |> Array.map(snd) |> Array.toSeq
        let cubicInterp = Interpolate.CubicSpline(curveTerms, curvePrices)
        // Then evaluate over the increased frequency         
        let maxAnnTerm = Seq.max curveTerms      
        let monthlyTerms = [|0.0..maxAnnTerm * 12.0|] |> Array.map (fun t -> t / 12.0)
        let monthlyValues = monthlyTerms |> Array.map(cubicInterp.Interpolate)
        let monthlyCurve = ZCBCurve (Array.zip monthlyTerms monthlyValues)
        monthlyCurve

    // Helpers

    // convert a curve (2-tuple array) to a 2D array for output to spreadsheets
    let curveTo2DArray (ZCBCurve aZCBCurve) =
        Array2D.init (Array.length aZCBCurve) 2 (fun i j -> match j with |0 -> fst aZCBCurve.[i] |_ -> snd aZCBCurve.[i])
        

    /// TESTS
    let testZCBArray = [|   0.9946570790646718;
                            0.987700509264612;
                            0.9792121374870155;
                            0.9692753745078829;
                            0.9579705066376444;
                            0.9453917221779694; |]

    let testAnnualTimes = [|1.0;
                            2.0;
                            3.0;
                            4.0;
                            5.0;
                            6.0; |]

    let testAnnZCBCurve = createZCBCurve testAnnualTimes testZCBArray

    let withZeroTestZCB = testAnnZCBCurve |> withZeroZCB
    let stillWithZeroTestZCB = withZeroTestZCB |> withZeroZCB

    let guessTheScaleFactor = testAnnZCBCurve |> implyScaleFactor

    let testAnnSpotFromAnnCurve = getSpotCurveFromZCBCurve Annual testAnnZCBCurve

    let testContSpotFromAnnCurve = getSpotCurveFromZCBCurve (getCompounding 0.0) testAnnZCBCurve

    let extractContSpotRates = testContSpotFromAnnCurve |> unpackSpotCurve |> extractCurveValue

    let testSimpleFwdCurve = testAnnZCBCurve |> simplyCompoundedForwardCurve

    let testInterpMonCurve = testAnnZCBCurve |>  interpolateAnnZCBCurveAsMonthly

    let testRawInterp = testInterpMonCurve |> curveTo2DArray

    