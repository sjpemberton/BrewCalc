namespace BrewCalculations

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

///Module containing Units of Measure for use with Brewing Calculations
module Units =

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
    [<Measure>] type Gal

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
    [<Measure>] type sg = kg / L  //gp / usGal

    ///Gravity Point - A Simplified brewing unit for amount of sugar dissolved in solution
    [<Measure>] type gp

    ///The number of gravity points per Gallon(US
    [<Measure>] type ppg = gp / usGal

    ///Potential Gravity Points The number of Gravity points in a lb of malt
    [<Measure>] type pgp = gp / lb

    ///Hot Water Extract. Points per Litre per Kilo
    [<Measure>] type hwe = gp / L

     ///The number of gravity points per Litre
    [<Measure>] type ppl = gp / L

    ///Gravity Points - A brewing simplification of specific gravity
    type GravityPoint =
        | PPG of float<ppg>
        | HWE of float<hwe>


    //Alcohol
    /// Alcohol By Volume
    [<Measure>] type abv 

///Simple Conversion constants and functions 
module Conversions =

    open Units

    let sucroseBasePoints = 46.0<ppg>
    let litresPerGal = 4.54609<L/Gal>
    let litresPerUsGallon = 3.78541<L/usGal>
    let degreesFperC = 1.8<degF/degC>
    let hweInPpg = 8.3454<hwe/ppg>
    let ouncesPerPound = 16<oz/lb>
    let poundPerKg = 2.20462<lb/kg>

    let toFahrenheit degreesC = degreesC * degreesFperC + 32.0<degF>
    let toCelsius degreesF = (degreesF - 32.0<degF>) / degreesFperC 
    let hweToPPG (hwe:float<hwe>) = hwe / hweInPpg
    let ppgToHwe (ppg:float<ppg>) = ppg * hweInPpg
    let ToPound (kg:float<kg>) = poundPerKg * kg
    let ToKilograms (lb:float<lb>) = lb / poundPerKg
    let ToLitres (gallons:float<usGal>) = gallons * litresPerUsGallon
    let ToUsGallons (litres:float<L>) = litres / litresPerUsGallon