package com.benbenlaw.casting.config;

import net.neoforged.neoforge.common.ModConfigSpec;

public class CastingConfig {

    public static final ModConfigSpec.Builder BUILDER = new ModConfigSpec.Builder();
    public static final ModConfigSpec SPEC;
    public static final ModConfigSpec.ConfigValue<Double> oreMultiplier;


    static {

        // Casting Configs
        BUILDER.comment("Casting Startup Config")
                .push("Casting");

        oreMultiplier = BUILDER.comment("The multiplier for ores, default = 1.5")
                .define("Ore Multiplier", 1.5);


        BUILDER.pop();



        //LAST
        SPEC = BUILDER.build();
    }
}
