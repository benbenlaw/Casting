package com.benbenlaw.casting.config;

import net.neoforged.neoforge.common.ModConfigSpec;

public class ToolModifierConfig {

    public static final ModConfigSpec.Builder BUILDER = new ModConfigSpec.Builder();
    public static final ModConfigSpec SPEC;
    public static final ModConfigSpec.ConfigValue<Integer> maxFortuneAmount;
    public static final ModConfigSpec.ConfigValue<Integer> maxEfficiencyAmount;
    public static final ModConfigSpec.ConfigValue<Integer> maxUnbreakingAmount;
    public static final ModConfigSpec.ConfigValue<Integer> maxRepairingAmount;


    static {

        // Caveopolis Configs
        BUILDER.comment("Casting Startup Config")
                .push("Casting");

        maxFortuneAmount = BUILDER.comment("The max amount of fortune levels that can be applied to tools, default = 5, 0 disables")
                .define("Max Fortune Level", 5);

        maxEfficiencyAmount = BUILDER.comment("The max amount of efficiency levels that can be applied to tools, default = 7, 0 disables")
                .define("Max Efficiency Level", 7);

        maxUnbreakingAmount = BUILDER.comment("The max amount of unbreaking levels that can be applied to tools, default = 10, 10 and above is unbreakable, 0 disables")
                .define("Max Unbreaking Level", 10);

        maxRepairingAmount = BUILDER.comment("The max amount of repairing levels that can be applied to tools, default = 10 cannot be faster than 10, 0 disables")
                .define("Max Repairing Level", 10);




        BUILDER.pop();



        //LAST
        SPEC = BUILDER.build();

    }

}
