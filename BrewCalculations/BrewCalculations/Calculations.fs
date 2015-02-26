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
    let AlcoholByVolume (originalGravity:float<sg>) (finalGravity:float<sg>) : float<abv> = 
        FloatWithMeasure ((1.05 / 0.79) * ((originalGravity - finalGravity) / finalGravity)) * 100.0

    let ABVsimple (og:float<sg>) (fg:float<sg>) : float<abv> = 
        FloatWithMeasure 131.0 * (og - fg)

    ///Calculates required Grain in weight from the target gravity points and effective malt potential (in relation to a given weight).
    let GrainRequired<[<Measure>] 'u> (gravityPoints:float<gp>) effectivePotential :float<'u> =
        FloatWithMeasure (float gravityPoints / effectivePotential)

    ///Mash Efficiency from points - Pre-boil points / Potential max points
    let Efficiency (potential:float<gp>) (actual:float<gp>) = 
        (actual / potential) * 1.0<percentage>






    (** 
    Functions working on US/Metric Units. EG: PPG, lb, oz, usGallon 
    *)

    ///Converts a points per gal (gp / usGal) and volume into total gravity points in that volume
    let totalGravityPoints (potential:float<gp / usGal>) (vol : float<usGal>) =  
        potential * vol

    ///Gets a PPG value from a given specific gravity
    //let toPpg (gravity:float<sg>) = 
        //float (gravity |> ToGravPoints) * 1.0<ppg>

    ///Calculates the total specific gravity points needed to achieve the given volume at the given specific gravity - not taking into account malt or weight
    let RequiredPoints (targetGravity:float<sg>) (vol:float<usGal>) = 
        totalGravityPoints ((targetGravity |> ToGravPoints) / 1.0<usGal>) vol

    ///The maximum potential points (in ppg) for a given weight of grain with the given extract potential, divided by the target volume
    let maxPotentialPoints (grainPotential:float<pgp>) (grain:float<lb>) (vol:float<usGal>) :float<ppg> = 
        (grainPotential * grain) / vol


    (**Efficiency taking into account losses during process
      Can be used to measure efficiency at various stages. Just substitute in the actual SG and Vol at a particular time. eg pre or post boil
    *)
    let calculateBrewHouseEfficiency (potential:float<ppg>) (actual:float<ppg>) (targetVol:float<usGal>) (actualVol:float<usGal>) =
        ((actual * actualVol) / (potential * targetVol)) * 1.0<percentage>

    ///Required grain in pound based on a malt potential in %, mash efficiency and total gravity points
    let requiredGrainInPounds (gravityPoints:float<gp>) (potential:float<percentage>) (efficiency:float<percentage>)  =
        GrainRequired<lb> gravityPoints (float((potential / 100.0) * (efficiency / 100.0) * 46.0))

    ///The potential gravity that an amount of grain in lb with the given ppg at a particular efficiency for a target volume 
    let estimateGravity  (vol:float<usGal>) (grain:float<lb>) (grainPotential:float<gp / lb>) (efficiency:float<percentage>) =
        ((grainPotential * grain * (float efficiency / 100.0)) / vol) * 1.0<usGal>
        |> ToGravity

    let estimateGravityFromGrainBill vol efficiency (grainBill:list<float<lb>*float<pgp>>) = 
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

