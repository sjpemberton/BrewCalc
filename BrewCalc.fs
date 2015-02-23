open LanguagePrimitives
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

(* Define custom units of measure *)

//General
[<Measure>]type percentage

//Volumes
[<Measure>] type L //Litres - m^3/1000
[<Measure>] type imperialGallon
[<Measure>] type usGallon

//Gravity UoM
[<Measure>] type sg = kg/L //Specific Gravity - Ratio of density compared to water measured in Kg/L
[<Measure>] type gp //Gravity Points - A breing simplification of specific gravity

let litresPerImperialGallon = 4.54609<L/imperialGallon>
let litresPerMetricGallon = 3.78541<L/usGallon>

//weights - KG is standard
[<Measure>] type gram
[<Measure>] type lb
[<Measure>] type ounce

//Temp
[<Measure>] type degC
[<Measure>] type degF

let degreesFperC =  1.8<degF/degC>

//Alcohol
[<Measure>] type abv 

let convertDegCToF c = c * degreesFperC + 32.0<degF>
let convertDegFToC f = (f - 32.0<degF>) / degreesFperC 

let convertToGravPoints (grav:float<sg>) =
    (float grav - 1.0) * 1000.0<gp>

let convertToGravity (gp:float<gp>) =
    ((float gp / 1000.0) + 1.0) * 1.0<sg>

let pointsByVolume (gp:float<gp>) (vol : float<'u>) =  gp * vol

let calculateGravPoints<[<Measure>] 'u> og (vol : float<'u>) =
     pointsByVolume (convertToGravPoints og) vol

//Yeast attenuation
let attenuation (og:float<sg>) (fg:float<sg>) =
    ((og - fg) / (og - 1.0<sg>)) * 100.0<percentage>

//Estimated FG
let expectedFinalGravity (og:float<sg>) (attenuation:float<percentage>) =
    let originalGravPoints = convertToGravPoints og
    (originalGravPoints - (originalGravPoints * (float attenuation / 100.0))) 
    |> convertToGravity

//Estimated ABV
let ABV (og:float<sg>) (fg:float<sg>) : float<abv> = 
    FloatWithMeasure ((1.05 / 0.79) * ((og - fg) / fg)) * 100.0

let ABVsimple (og:float<sg>) (fg:float<sg>) : float<abv> = 
    FloatWithMeasure 131.0 * (og - fg)

//IBU

//Extract Potential - Basic
//Can be used iteratively on a grain bill to produce a full max PPG value
let calculateExtractPotential (potentialPoints:float<gp>) (grain:float<'u>) (vol:float<'v>) =
    (potentialPoints * grain) / vol

//Estimated OG based on potential and efficiency
let estimatedOriginalGravity (potential:float<gp>) (efficiency:float<percentage>) =
    (float potential * efficiency)

//Mash Efficiency from points
let calculateEfficiency (potential:float<gp>) (actual:float<gp>) = 
    (actual / potential) * 1.0<percentage>

//Mash Efficiency - alternate - using a 'standard' 37.0 ppg. based on lager malt maximum
let efficiency (og:float<sg>) (vol : float<'u>) (grain:float<'v>) :float<percentage> =
    let ppg = calculateGravPoints og vol
    ((float ppg / float grain) / 37.0) * 100.0<percentage>

//Efficiency taking into account losses during process
let calculateBrewHouseEfficiency (ppv: float<'u gp>) (apv: float<'u gp>) =
    (apv / ppv) * 1.0<percentage>