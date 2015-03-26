/// Brewing Calculations for all stages of the brewing process
///
/// Calculations
///
module Caculations 

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open LanguagePrimitives
open Units
open Conversions


(** Types for use with the calculations *)

///A particular amount of a malt with a specified potential
type Malt<[<Measure>] 'u> = 
    {Weight:float<'u>; Potential:float<gp/'u>;}

(**
Generic/Common Functions 
*)

///Converts a Specific Gravity into brewing Specific Gravity Points
let ToGravPoints (gravity:float<sg>) =
    (float gravity - 1.0) * 1000.0<gp>

///Converts the given GP into Specific Gravity
let ToGravity (gravityPoints:float<gp>) =
    ((float gravityPoints / 1000.0) + 1.0) * 1.0<sg>

///Estimated FG - Estimates the final gravity of a beer based on the starting gravity points and the yeast attenuation
let EstimateFinalGravity originalGravity (attenuation:float<percentage>) =
    let points = originalGravity |> ToGravPoints
    let pointsAfterFermentation = (points - (points * (float attenuation / 100.0))) 
    pointsAfterFermentation|> ToGravity

///Yeast attenuation - The difference in specific gravity between OG and FG
let YeastAttenuation (originalGravity:float<sg>) (finalGravity:float<sg>) =
    ((originalGravity - finalGravity) / (originalGravity - 1.0<sg>)) * 100.0<percentage>

///Estimated ABV
let AlcoholByVolume (originalGravity:float<sg>) (finalGravity:float<sg>) : float<ABV> = 
    FloatWithMeasure ((1.05 / 0.79) * ((originalGravity - finalGravity) / finalGravity)) * 100.0

let ABVsimple (og:float<sg>) (fg:float<sg>) : float<ABV> = 
    FloatWithMeasure 131.0 * (og - fg)

///Calculates required Grain in weight from the target gravity points and effective malt potential (in relation to a given weight).
let GrainRequired<[<Measure>]'u> (gravityPoints:float<gp>) (effectivePotential:float<gp/'u>) =
    gravityPoints / effectivePotential

///Mash Efficiency from points - Pre-boil points / Potential max points
let Efficiency (potential:float<gp>) (actual:float<gp>) = 
    (actual / potential) * 1.0<percentage>





(** 
Functions working on US/Metric Units. EG: PPG, lb, oz, usGallon 
*)

///Converts a points per gal (gp / usGal) and volume into total gravity points in that volume
let TotalGravityPoints (potential:float<gp / usGal>) (vol : float<usGal>) =  
    potential * vol

///Calculates the total specific gravity points needed to achieve the given volume at the given specific gravity - not taking into account malt or weight
let RequiredPoints (targetGravity:float<sg>) (vol:float<usGal>) = 
    TotalGravityPoints ((targetGravity |> ToGravPoints) / 1.0<usGal>) vol

///The maximum potential points (in ppg) for a given weight of grain with the given extract potential, divided by the target volume
let MaxPotentialPoints (grainPotential:float<pgp>) (grain:float<lb>) (vol:float<usGal>) :float<ppg> = 
    (grainPotential * grain) / vol

///Efficiency taking into account losses during process  
///Can be used to measure efficiency at various stages. Just substitute in the actual points and Vol at a particular time. eg pre or post boil
let CalculateBrewHouseEfficiency (potential:float<ppg>) (actual:float<ppg>) (targetVol:float<usGal>) (actualVol:float<usGal>) =
    ((actual * actualVol) / (potential * targetVol)) * 1.0<percentage>

///Required grain in pound based on a malt potential in %, mash efficiency and total gravity points
let RequiredGrainInPounds (gravityPoints:float<gp>) (potential:float<percentage>) (efficiency:float<percentage>) =
    GrainRequired<lb> gravityPoints (float((potential / 100.0) * (efficiency / 100.0) * 46.0<ppg>) * 1.0<pgp>)

///The estimated gravity of wort created from an amount of grain in lb with the given ppg, at a particular efficiency and for a target volume
let EstimateGravity  (vol:float<usGal>) (grain:float<lb>) (grainPotential:float<pgp>) (efficiency:float<percentage>) =
    ((grainPotential * grain * (float efficiency / 100.0)) / vol) * 1.0<usGal>
    |> ToGravity

let EstimateGravityFromGrainBill vol efficiency (grainBill:list<Malt<lb>>) = 
    List.fold (fun acc m -> acc + EstimateGravity vol m.Weight m.Potential efficiency) 0.0<sg> grainBill



(**
Functions that work on British/Imperial units - HWE, L, Kg
*)

let private GrainInKilo effectivePoints =
    float (effectivePoints * (46.0<ppg> |> ppgToHwe)) * 1.0<kg>

///Required grain in kilo based on a malt potential in HWE, mash efficiency and total gravity points
let RequiredGrainInKilo (gravityPoints:float<gp>) (potential:float<hwe>) (efficiency:float<percentage>)  =
    GrainRequired<kg> gravityPoints (float(potential * (efficiency / 100.0) * (46.0<ppg> |> ppgToHwe)) * 1.0<gp/kg>)

///Efficiency Calculation using Litres and HWE
let CalculateBrewHouseEfficiencyLitres (potential:float<hwe>) (actual:float<hwe>) (targetVol:float<L>) (actualVol:float<L>) =
    ((actual * actualVol) / (potential * targetVol)) * 1.0<percentage>

//Mash Efficiency - alternate - using a 'standard' 37.0 ppg. based on lager malt maximum. For use when individual grain potential is not known
//let alternativeEfficiency (og:float<sg>) (vol : float<'u>) (grain:float<'v>) :float<percentage> =
//    let ppg = totalGravityPoints og vol
//    ((float ppg / float grain) / 37.0) * 100.0<percentage>
//


///Bitterness
///Calculates Aplha Acid Units
let CalculateAplhaAcidUnits (weight:float<oz>) (alpha:float<percentage>) :float<AAU> =
    FloatWithMeasure (float(weight * alpha))

///Calculates IBU (mg/L) from AAUs
let CalculateIBUsFromAAU (utilisation:float<percentage>) (aau:float<AAU>)  (volume:float<usGal>)  :float<IBU> = 
    FloatWithMeasure (float(aau * utilisation * 74.89 / volume))

///Calculates IBUS (mg/L) from AA% weight
let EstimateIBUs (utilisation:float<percentage>) (alpha:float<percentage>) (weight:float<oz>) (volume:float<usGal>) :float<IBU> =
    CalculateIBUsFromAAU utilisation (CalculateAplhaAcidUnits weight alpha) volume

//The Tinseth Equation for hop utilisation. Time in minutes
let EstimateHopUtilisation (gravity:float<sg>) (time:float) :float<percentage> = 
    (1.65 * 0.000125 ** (float gravity - 1.0)) * ((1.0 - 2.71828 ** (-0.04 * time)) / 4.15)
    |> FloatWithMeasure


///Colour
///Calculates an MCU value from grain weight, colour in degrees lovibond and volume
let CalculateMcu (weight:float<lb>) (colour:float<degL>) (volume:float<usGal>) :float<MCU> =
    weight * colour / volume

///Returns the SRM value from an MCU value
let CalculateSrmFromMcu (mcu:float<MCU>) :float<SRM> = 
    FloatWithMeasure 1.4922 * (float mcu ** 0.6859)

//Taken from Malt.io
///Convert an SRM value into rough RGB 
let SrmToRgb (srm:float<SRM>) = 
    (System.Math.Round(min 255.0 (max 0.0 (255.0 * 0.975 ** float srm))),
     System.Math.Round(min 255.0 (max 0.0 (245.0 * 0.88 ** float srm))),
     System.Math.Round(min 255.0 (max 0.0 (220.0 * 0.7 ** float srm))))



//Priming Sugar 
//These calculations all expect grams and litre units

///Calculates the final CO2 level in a beer - co2PerGram is CO2 created per gram of sugar
let CalculateCo2 co2PerGram (currentCarb : float<CO2>) (sugar : float<g>) (volume : float<L>) =
    currentCarb + co2PerGram * sugar / volume

///Carbonation added by Corn sugar (Glucose Monohydrate)
let CalculatePrimingCornSugar = CalculateCo2 (0.5 * 0.91) 
///Carbonation with table sugar
let CalculatePrimingTableSugar = CalculateCo2 (0.5)
///Carbonation with Dried Malt Extract (DME)
let CalculatePrimingDme = CalculateCo2 (0.5 * 0.82 * 0.80)

///Calculates the amount of sugar required to create the target amount of CO2 - co2PerGram is CO2 created per gram of sugar
let CalculatePrimingSugar co2PerGram  (currentCarb : float<CO2>) (targetCarb : float<CO2>) (volume : float<L>) :float<g> =
    volume * (targetCarb - currentCarb) / co2PerGram

///Amount of Corn sugar to produce target CO2
let CalculateRequiredCornSugar = CalculatePrimingSugar (0.5 * 0.91)
///Amount of Table sugar to produce target CO2
let CalculateRequiredTableSugar = CalculatePrimingSugar (0.5)
///Amount of Dried MAlt Extrcat (DME) to produce target CO2
let CalculateRequiredDme = CalculatePrimingSugar (0.5 * 0.82 * 0.80)