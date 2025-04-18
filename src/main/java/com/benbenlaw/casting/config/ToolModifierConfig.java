package com.benbenlaw.casting.config;

import net.neoforged.neoforge.common.ModConfigSpec;

public class ToolModifierConfig {

    public static final ModConfigSpec.Builder BUILDER = new ModConfigSpec.Builder();
    public static final ModConfigSpec SPEC;
    public static final ModConfigSpec.ConfigValue<Integer> maxFortuneAmount;
    public static final ModConfigSpec.ConfigValue<Integer> maxEfficiencyAmount;
    public static final ModConfigSpec.ConfigValue<Integer> maxUnbreakingAmount;
    public static final ModConfigSpec.ConfigValue<Integer> maxRepairingAmount;
    public static final ModConfigSpec.ConfigValue<Integer> maxLootingAmount;
    public static final ModConfigSpec.ConfigValue<Integer> maxSharpnessAmount;
    public static final ModConfigSpec.ConfigValue<Float> additionalMultiplierForSharpness;
    public static final ModConfigSpec.ConfigValue<Float> additionalAdditionForSharpness;
    public static final ModConfigSpec.ConfigValue<Integer> maxLifestealAmount;
    public static final ModConfigSpec.ConfigValue<Integer> maxKnockbackAmount;
    public static final ModConfigSpec.ConfigValue<Integer> maxIgniteAmount;
    public static final ModConfigSpec.ConfigValue<Integer> maxExcavationAmount;




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
        maxLootingAmount = BUILDER.comment("The max amount of looting levels that can be applied to tools, default = 5, 0 disables")
                .define("Max Looting Level", 5);
        maxSharpnessAmount = BUILDER.comment("The max amount of sharpness levels that can be applied to tools, default = 5, 0 disables")
                .comment("calculation is multiplier * level + addition, default = 0.5 * level + 0.5")
                .define("Max Sharpness Level", 10);
        additionalMultiplierForSharpness = BUILDER.comment("The additional multiplier for sharpness levels, default vanilla = 0.5, default casting = 1.0")
                .define("Additional Multiplier for Sharpness", 1.0f);
        additionalAdditionForSharpness = BUILDER.comment("The additional addition for sharpness levels, default = 0.5")
                .define("Additional Addition for Sharpness", 0.5f);
        maxLifestealAmount = BUILDER.comment("The max amount of lifesteal levels that can be applied to tools, default = 5, max = 10, 0 disables. Each level gives 10% lifesteal of damage dealt")
                .define("Max Lifesteal Level", 5);
        maxKnockbackAmount = BUILDER.comment("The max amount of knockback levels that can be applied to tools, default = 5, 0 disables")
                .define("Max Knockback Level", 5);
        maxIgniteAmount = BUILDER.comment("The max amount of ignite levels that can be applied to tools, default = 5, 0 disables. Each level is an additional second of burning")
                .define("Max Ignite Level", 5);
        maxExcavationAmount = BUILDER.comment("The max amount of excavation levels that can be applied to tools, default = 5, 0 disables. Each level adds an additional block in every direction")
                .define("Max Excavation Level", 3);



        BUILDER.pop();



        //LAST
        SPEC = BUILDER.build();

    }

}
