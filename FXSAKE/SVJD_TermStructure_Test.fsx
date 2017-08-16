#load "SVJD_TermStructure.fs"

open FxSake.SVJD_TermStructure

let defaultSVJDParameters = {reversionLevel = 0.25; reversionSpeed = 4.10636051632234; startVal = 0.2; volOfVariance = 0.468645736687236; Correlation = -0.527442089116611; jumpIntensity = 0.0; jumpMean = 0.0; jumpVol = 0.0}

// we will need to define the 1m and unconditional targets for the given asset
let this1mVol = 0.14551565323787
let thisUncVol = 0.282190830041867

let timescaleSVJD = 1.0 / 12.0
let returnMonths = [| 1;3;6;9;12;24;36;48;60;84;120;360 |]

// We use the square of these as the reversionLevel and volOfVariance SVJD parameters
let thisSVJDParamters = { defaultSVJDParameters with reversionLevel = thisUncVol * thisUncVol; startVal = this1mVol * this1mVol;}

// We can now call our function to return the interpolated term structure
let extrapolatedSVJDTermStructure = getSVJDInterpolatedVols thisSVJDParamters (1.0 / 12.0) returnMonths