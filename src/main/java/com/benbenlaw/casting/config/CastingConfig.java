package com.benbenlaw.casting.config;

import net.neoforged.neoforge.common.ModConfigSpec;

public class CastingConfig {

    public static final ModConfigSpec.Builder BUILDER = new ModConfigSpec.Builder();
    public static final ModConfigSpec SPEC;
    public static final ModConfigSpec.ConfigValue<Double> oreMultiplier;
    public static final ModConfigSpec.ConfigValue<Integer> experienceGivenWhenMeltingAValidOre;


    static {

        // Casting Configs
        BUILDER.comment("Casting Startup Config")
                .push("Casting");

        oreMultiplier = BUILDER.comment("The multiplier for ores, default = 1.5")
                .define("Ore Multiplier", 1.5);

        experienceGivenWhenMeltingAValidOre = BUILDER.comment("The amount of experience given when melting a valid ore in the casting:melting_produces_experience tag, default = 25")
                        .define("Experience Given When Melting A Valid Ore", 25);


        BUILDER.pop();



        //LAST
        SPEC = BUILDER.build();
    }
}
