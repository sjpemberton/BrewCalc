open LanguagePrimitives
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

(* Define custom units of measure *)

//General
[<Measure>]type percentage

//Gravity UoM
[<Measure>] type gravity //grams of sugar in 100g of liquid (Kg/L)
[<Measure>] type gravityPoint

//Volumes
[<Measure>] type litre
[<Measure>] type imperialGallon
[<Measure>] type usGallon

let litresPerImperialGallon = 4.54609<litre/imperialGallon>
let litresPerMetricGallon = 3.78541<litre/usGallon>

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

let convertToGravPoints (grav:float<gravity>) : float<gravityPoint> =
    FloatWithMeasure (float grav - 1.0) * 1000.0

let convertToGravity (gp:float<gravityPoint>) : float<gravity> =
    FloatWithMeasure ((float gp / 1000.0) + 1.0)

let calculateGravPoints<[<Measure>] 'u> og (vol : float<'u>) =
     convertToGravPoints og * vol

//Yeast attenuation
let attenuation (og:float<gravity>) (fg:float<gravity>) =
    ((float og - float fg) / (float og - 1.0)) * 100.0<percentage>

//Estimated FG
let expectedFinalGravity (og:float<gravity>) (attenuation:float<percentage>) =
    let originalGravPoints = convertToGravPoints og
    (originalGravPoints - (originalGravPoints * (float attenuation / 100.0))) 
    |> convertToGravity

//Estimated ABV
let ABV (og:float<gravity>) (fg:float<gravity>) : float<abv> = 
    FloatWithMeasure ((1.05 / 0.79) * ((og - fg) / fg)) * 100.0

let ABVsimple (og:float<gravity>) (fg:float<gravity>) : float<abv> = 
    FloatWithMeasure 131.0 * (og - fg)

//IBU


//Efficiency
let efficiency (og:float<gravity>) (vol : float<'u>) (grain:float<kg>) =
    let extract = (vol * og * 0.14)
    (extract / grain) * 100.0<percentage>
