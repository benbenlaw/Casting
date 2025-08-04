package com.benbenlaw.casting.config;

import net.neoforged.neoforge.common.ModConfigSpec;

public class CastingConfig {

    public static final ModConfigSpec.Builder BUILDER = new ModConfigSpec.Builder();
    public static final ModConfigSpec SPEC;
    public static final ModConfigSpec.ConfigValue<Double> oreMultiplier;
    public static final ModConfigSpec.ConfigValue<Integer> experienceGivenWhenMeltingAValidOre;
    public static final ModConfigSpec.ConfigValue<Integer> maxMultiblockControllerHeldItems;
    public static final ModConfigSpec.ConfigValue<Integer> maxMultiblockControllerHeldFluid;
    public static final ModConfigSpec.ConfigValue<Integer> timeInTicksThatMultiblockControllerDoesAStructureCheck;


    static {

        // Casting Configs
        BUILDER.comment("Casting Startup Config")
                .push("Casting");

        oreMultiplier = BUILDER.comment("The multiplier for ores, default = 1.5")
                .define("Ore Multiplier", 1.5);

        experienceGivenWhenMeltingAValidOre = BUILDER.comment("The amount of experience given when melting a valid ore in the casting:melting_produces_experience tag, default = 25")
                        .define("Experience Given When Melting A Valid Ore", 25);

        maxMultiblockControllerHeldItems = BUILDER.comment("The maximum number of items that can be held in the multiblock controller, default = 60")
                .defineInRange("Max Multiblock Controller Held Items", 60, 1, 512);

        maxMultiblockControllerHeldFluid = BUILDER.comment("The total mb that can be stores in multiblock controller, default = 1000000")
                .defineInRange("Max Multiblock Controller Held Items", 1000000, 1, 512);

        timeInTicksThatMultiblockControllerDoesAStructureCheck = BUILDER.comment("The time in ticks that the multiblock controller does a structure check, Multiblock validation is done when blocks are placed near the mutliblock, this check is just to confirm that the blocks are still present, default = 600 (30 seconds)")
                .defineInRange("Time In Ticks That Multiblock Controller Does A Structure Check", 600, 20, Integer.MAX_VALUE);



        BUILDER.pop();



        //LAST
        SPEC = BUILDER.build();
    }
}
