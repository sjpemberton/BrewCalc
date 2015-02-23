open LanguagePrimitives
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

(* Define custom units of measure *)

///Percentage
[<Measure>]type percentage

//Temp
///Degrees Celsius
[<Measure>] type degC
///Degrees Fahrenheit
[<Measure>] type degF

//Volumes
///Litres - m^3/1000
[<Measure>] type L 

///Imperial Gallon
[<Measure>] type impGal

///US Gallon
[<Measure>] type usGal

//weights
///Grams
[<Measure>] type g

///Pound
[<Measure>] type lb

///Ounces
[<Measure>] type oz

//Gravity
///Specific Gravity - Ratio of density compared to water measured in Kg/L
[<Measure>] type sg = kg/L

///Hot Water Extract
[<Measure>] type hwe = L * degC / kg

///Point/Pound/Gallon(US)
[<Measure>] type ppg = usGal * degF / lb

///Gravity Points - A brewing simplification of specific gravity
type GravityPoint =
    | PPG of float<ppg>
    | HWE of float<hwe>


//Alcohol
/// Alcohol By Volume
[<Measure>] type abv 

let litresPerimpGal = 4.54609<L/impGal>
let litresPerMetricGallon = 3.78541<L/usGal>
let degreesFperC = 1.8<degF/degC>
let ppgInHwe = 8.3454<ppg/hwe>
let poundPerKg = 2.20<lb/kg>

let toFahrenheit c = c * degreesFperC + 32.0<degF>

let toCelsius f = (f - 32.0<degF>) / degreesFperC 

let toPPG (hwe:float<hwe>) = hwe * ppgInHwe

let toGravityPoints<[<Measure>] 'u> (g:float<sg>) :float<'u> = 
    FloatWithMeasure (float g - 1.0) * 1000.0

let toGravity (gp:float<'u>) =
    ((float gp / 1000.0) + 1.0) * 1.0<sg>

let pointsByVolume (gp:float<'v>) (vol : float<'u>) =  gp * vol

//TODO - Refactor the to GP calc funcs
let calculateGravPointsPPG<[<Measure>] 'u> og (vol : float<usGal>) =
     pointsByVolume (og |> toGravityPoints<ppg>) vol

let calculateGravPointsHWE<[<Measure>] 'u> og (vol : float<L>) =
     pointsByVolume (og |> toGravityPoints<hwe>) vol

//Yeast attenuation
let attenuation (og:float<sg>) (fg:float<sg>) =
    ((og - fg) / (og - 1.0<sg>)) * 100.0<percentage>

//Estimated FG
let expectedFinalGravity (og:float<sg>) (attenuation:float<percentage>) =
    let originalGravPoints = og |> toGravityPoints
    (originalGravPoints - (originalGravPoints * (float attenuation / 100.0))) 
    |> toGravity

//Estimated ABV
let ABV (og:float<sg>) (fg:float<sg>) : float<abv> = 
    FloatWithMeasure ((1.05 / 0.79) * ((og - fg) / fg)) * 100.0

let ABVsimple (og:float<sg>) (fg:float<sg>) : float<abv> = 
    FloatWithMeasure 131.0 * (og - fg)

//Extract Potential - Basic
//Can be used iteratively on a grain bill to produce a full max PPG value
let calculatePotential (grainPotential:float<ppg>) (grain:float<'u>) (vol:float<'v>) =
    (grainPotential * grain) / vol

//Estimated OG based on potential and efficiency
let estimatedOriginalGravity (potential:float<ppg>) (efficiency:float<percentage>) =
    (float potential / 100.0) * efficiency

//Mash Efficiency from points
let calculateEfficiency (potential:float<ppg>) (actual:float<ppg>) = 
    (actual / potential) * 1.0<percentage>

//Mash Efficiency - alternate - using a 'standard' 37.0 ppg. based on lager malt maximum. For use when individual grain potential is not known
let alternativeEfficiency (og:float<sg>) (vol : float<'u>) (grain:float<'v>) :float<percentage> =
    let ppg = calculateGravPointsPPG og vol
    ((float ppg / float grain) / 37.0) * 100.0<percentage>

//Efficiency taking into account losses during process
let calculateBrewHouseEfficiency (ppv: float<'u ppg>) (apv: float<'u ppg>) =
    (apv / ppv) * 1.0<percentage>

//IBU