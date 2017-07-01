namespace FxSake

module YieldCurves = 

    type ZCBPrices = ZCBPrices of float array
    type ZCBIndexed = ZCBIndexed of (int * float) array
    type ZCBCurve = ZCBCurve of (float * float) array

    type Compounding = Continuous | Monthly | SemiAnnual | Annual

    type SpotRates = SpotRates of float array
    type SpotCurve = SpotCurve of (float * float) array

    type ForwardCurve = ForwardCurve of (float * float) array

    let createZCBCurve (time: float[]) (price: float[]) = 
        let myZCBCurve =  Array.zip time price
        ZCBCurve myZCBCurve

    let unpackSpotCurve (SpotCurve mySpot) = mySpot
    let unpackZCBCurve (ZCBCurve myZCB) = myZCB
    let unpackForwardCurve (ForwardCurve myFwd) = myFwd

    let extractCurveValue (curve: (float * float) array) =        
        curve |> Array.map (snd)
    
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

    let implyScaleFactor (ZCBCurve aZCBCurve) =     
        (fst aZCBCurve.[1]) -  (fst aZCBCurve.[0])
    
    let simplyCompoundedForwardCurve (aZCBCurve) = 
        let (ZCBCurve fullZCBCurve) = withZeroZCB aZCBCurve
        let pairZCBs = fullZCBCurve |> Array.toSeq |> Seq.pairwise //Array.pairwise avaliable from F# 4.0 - convert this to a Seq and back to get this effect
        let scaleFactor = (ZCBCurve fullZCBCurve) |> implyScaleFactor    
        let forwardRates = pairZCBs |> Seq.map (fun ((t1,z1),(t2,z2)) -> (t1, ((1.0 / scaleFactor) * (z1 / z2) - 1.0)) ) |> Seq.toArray
        ForwardCurve forwardRates

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