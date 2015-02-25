namespace BrewCalculations

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open LanguagePrimitives
open Units
open Conversions


/// Brewing Calculations for all stages of the brewing process
///
/// ## Calculations
///
module Caculations = 

    (**
    Generic Functions 
    *)

    let PointsByVolume<[<Measure>] 'u> (gravity:float<sg>) = 
        FloatWithMeasure<'u> ((float gravity - 1.0) * 1000.0)

    let ToGravPoints (gravity:float<sg>) =
        (float gravity - 1.0) * 1000.0<gp>

    ///The gravity that a single gallon of wort created from a malt with the given PPG with yield
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
    let AlcoholByVolume (originalGravity:float<sg>) (finalGravity:float<sg>) : float<abv> = 
        FloatWithMeasure ((1.05 / 0.79) * ((originalGravity - finalGravity) / finalGravity)) * 100.0

    let ABVsimple (og:float<sg>) (fg:float<sg>) : float<abv> = 
        FloatWithMeasure 131.0 * (og - fg)

    ///Calculates required Grain in Pounds from the target gravity points, malt potential and efficiency.
    let GrainRequired<[<Measure>] 'u> (gravityPoints:float<gp>) effectivePotential :float<'u> =
        FloatWithMeasure (float gravityPoints / effectivePotential)

    ///Mash Efficiency from points - Pre-boil points / Potential max points
    let Efficiency (potential:float<gp>) (actual:float<gp>) = 
        (actual / potential) * 1.0<percentage>






    (** 
    Functions working on US/Metric Units. EG: PPG, lb, oz, usGallon 
    *)

    ///Converts a potential extract value (PPG) and volume into total gravity points
    let totalGravityPoints (potential:float<ppg>) (vol : float<usGal>) =  
        (float potential * float vol) * 1.0<gp>

    ///Gets a PPG value from a given specific gravity
    let toPpg (gravity:float<sg>) = 
        float (gravity |> ToGravPoints) * 1.0<ppg>

    ///Calculates the total gravity points needed to achieve the given volume at the given specific gravity
    let requiredExtract (targetGravity:float<sg>) (vol:float<usGal>) = 
        totalGravityPoints (targetGravity |> PointsByVolume) vol

    ///The maximum potential points for a given weight of grain with the given ppg, divided by the target volume
    let potentialPoints (grainPotential:float<ppg>) (grain:float<lb>) (vol:float<usGal>) = 
        float ((grainPotential * grain) / vol) * 1.0<gp>


    (**Efficiency taking into account losses during process
      Can be used to measure efficiency at various stages. Just substitute in the actual SG and Vol at a particular time. eg pre or post boil
    *)
    let calculateBrewHouseEfficiency (potential:float<ppg>) (actual:float<ppg>) (targetVol:float<usGal>) (actualVol:float<usGal>) =
        ((actual * actualVol) / (potential * targetVol)) * 1.0<percentage>

    ///Required grain in pound based on a malt potential in %, mash efficiency and total gravity points
    let requiredGrainInPounds (gravityPoints:float<gp>) (potential:float<percentage>) (efficiency:float<percentage>)  =
        GrainRequired<lb> gravityPoints (float((potential / 100.0) * (efficiency / 100.0) * 46.0))

    ///The potential gravity that an amount of grain in lb with the given ppg at a particular efficiency for a target volume 
    let estimateGravity  (vol:float<usGal>) (grain:float<lb>) (grainPotential:float<ppg>) (efficiency:float<percentage>) =
        float ((grainPotential * grain * (efficiency / 100.0)) / vol) * 1.0<sg>

    let estimateGravityFromGrainBill vol efficiency (grainBill:list<float<lb>*float<ppg>>) = 
        List.fold (fun acc g -> acc + estimateGravity vol (fst g) (snd g) efficiency) 0.0<sg> grainBill





    (**
    Functions that work on British/Imperial units - HWE, L, Kg
    *)

    //let calculateGravPointsHWE<[<Measure>] 'u> og (vol : float<L>) =
    //     pointsByVolume (og |> toGravityPoints<hwe>) vol

    let private grainInKilo effectivePoints =
        float (effectivePoints * (46.0<ppg> |> ppgToHwe)) * 1.0<kg>

    ///Required grain in kilo based on a malt potential in HWE, mash efficiency and total gravity points
    let requiredGrainInKilo (gravityPoints:float<gp>) (potential:float<hwe>) (efficiency:float<percentage>)  =
        GrainRequired<kg> gravityPoints (float(potential * (efficiency / 100.0) * (46.0<ppg> |> ppgToHwe)))




    //Mash Efficiency - alternate - using a 'standard' 37.0 ppg. based on lager malt maximum. For use when individual grain potential is not known
    //let alternativeEfficiency (og:float<sg>) (vol : float<'u>) (grain:float<'v>) :float<percentage> =
    //    let ppg = totalGravityPoints og vol
    //    ((float ppg / float grain) / 37.0) * 100.0<percentage>
    //


    //IBU