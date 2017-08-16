namespace FxSake

module SVJD_TermStructure = 
    
    open System // For Math.Exp

    type SVJDParameters = {reversionLevel: float; reversionSpeed: float; startVal: float; volOfVariance: float; Correlation: float; jumpIntensity: float; jumpMean: float; jumpVol: float }
    

    let getSVJDInterpolatedVols parameters timescale monthlist = 
        
        // heston model (TODO: Should these be avaliable as outer functions, and not just in this method here?
        let hestonVariance revLevel revSpeed currVol time = revLevel * (1.0 - Math.Exp(-revSpeed * time)) + currVol * Math.Exp(-revSpeed * time)

        let hestonVarianceOfVariance revLevel revSpeed currVol volVar time = 
            revLevel * volVar ** 2.0 * (1.0 - Math.Exp(-2.0 * revSpeed * time)) / (2.0 * revSpeed) + currVol * volVar ** 2.0 * (1.0 - revLevel / currVol) * Math.Exp(-revSpeed * time) * (1.0 - Math.Exp(-revSpeed * time)) / revSpeed
    
        let jumpVariance intensity mean vol = intensity * (mean * mean + vol * vol)

        let hestonCovariance revLevel revSpeed currVol volVar correl startTime endTime = 
            correl * ((hestonVariance revLevel revSpeed currVol endTime) * (hestonVarianceOfVariance revLevel revSpeed currVol volVar startTime)) ** 0.5

        let intervalVol parameters startTime endTime timescale = 
            hestonVariance parameters.reversionLevel parameters.reversionSpeed parameters.startVal startTime
            + 0.25 * timescale * hestonVarianceOfVariance parameters.reversionLevel parameters.reversionSpeed parameters.startVal parameters.volOfVariance startTime
            + jumpVariance parameters.jumpMean parameters.jumpMean parameters.jumpVol
            - hestonCovariance parameters.reversionLevel parameters.reversionSpeed parameters.startVal parameters.volOfVariance parameters.Correlation startTime endTime
        
        // Evaluate this model at the defined points
        let sortedOutputMonths = monthlist |> Array.sort
        let maxTermInMonths = monthlist |> Array.max |> float

        let termsSVJD = [| 0.0..maxTermInMonths |] |> Array.map (fun t -> t * timescale)
    
        let intervalsSVJD = termsSVJD |> Array.pairwise
        let intervalVols = intervalsSVJD |> Array.map (fun (t1, t2) -> intervalVol parameters t1 t2 timescale)
    
        let calcIntervalTotalVol subInt = (Array.sum subInt / float subInt.Length) ** 0.5
        let takeSubInterval months = intervalVols |> Array.take months
        let subIntTotalVol months = takeSubInterval months |> calcIntervalTotalVol

        monthlist |> Array.map (fun m -> (m, subIntTotalVol m)) 

