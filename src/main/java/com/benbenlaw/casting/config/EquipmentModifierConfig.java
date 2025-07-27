package com.benbenlaw.casting.config;

import net.neoforged.neoforge.common.ModConfigSpec;

public class EquipmentModifierConfig {

    public static final ModConfigSpec.Builder BUILDER = new ModConfigSpec.Builder();
    public static final ModConfigSpec SPEC;
    public static final ModConfigSpec.ConfigValue<Integer> maxEquipmentLevel;
    public static final ModConfigSpec.ConfigValue<Integer> experiencePerLevelForEquipmentLevel;
    public static final ModConfigSpec.ConfigValue<Float> experienceMultiplierPerLevel;
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
    public static final ModConfigSpec.ConfigValue<Integer> maxTeleportationAmount;
    public static final ModConfigSpec.ConfigValue<Integer> blocksPerLevelForTeleporting;
    public static final ModConfigSpec.ConfigValue<Integer> cooldownForTeleporting;
    public static final ModConfigSpec.ConfigValue<Integer> maxMagnetAmount;
    public static final ModConfigSpec.ConfigValue<Integer> maxProtectionAmount;
    public static final ModConfigSpec.ConfigValue<Float> percentageOfProtectionDamagePerProtectionLevel;
    public static final ModConfigSpec.ConfigValue<Integer> maxStepAssistAmount;
    public static final ModConfigSpec.ConfigValue<Integer> timeForDamageOnNightVision;
    public static final ModConfigSpec.ConfigValue<Integer> timeForDamageOnWaterBreathing;
    public static final ModConfigSpec.ConfigValue<Integer> maxSpeedAmount;
    public static final ModConfigSpec.ConfigValue<Integer> maxFeatherFallingAmount;
    public static final ModConfigSpec.ConfigValue<Integer> maxJetsAmount;




    static {

        // Caveopolis Configs
        BUILDER.comment("Casting Startup Config")
                .push("Casting");

        maxEquipmentLevel = BUILDER.comment("The max level of tools, default = 10, 0 disables")
                .define("Max Tool Level", 5);
        experiencePerLevelForEquipmentLevel = BUILDER.comment("The amount of experience needed per level for equipment level, default = 220")
                .comment("calculation (experiencePerLevelForEquipmentLevel) + (toolLevel * 0.15) = 253 for first level using 220 as the default")
                .define("Experience Per Level for Equipment Level", 220);
        experienceMultiplierPerLevel  = BUILDER.comment("The multiplier for experience per level, default = 0.15")
                .comment("calculation (experiencePerLevelForEquipmentLevel) + (toolLevel * experienceMultiplierPerLevel) = 253 for first level using 220 as the default")
                .define("Experience Multiplier Per Level", 0.15f);
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
        maxTeleportationAmount = BUILDER.comment("The max amount of teleportation levels that can be applied to tools, default = 5, 0 disables. Each level adds an additional 5 blocks to the teleportation distance")
                .define("Max Teleportation Level", 5);
        blocksPerLevelForTeleporting = BUILDER.comment("The amount of blocks per level for teleporting, default = 8")
                .define("Blocks Per Level for Teleporting", 8);
        cooldownForTeleporting = BUILDER.comment("The cooldown for teleporting, default = 50 (2.5 seconds), 20 ticks is 1 second")
                .define("Cooldown for Teleporting", 50);
        maxMagnetAmount = BUILDER.comment("The max amount of magnet levels that can be applied to tools, default = 8, 0 disables. Each level adds an additional block to the magnet range")
                .define("Max Magnet Level", 8);
        maxProtectionAmount = BUILDER.comment("The max amount of protection levels that can be applied to tools, default = 5, 0 disables. Each level adds an additional 1% protection")
                .define("Max Protection Level", 5);
        percentageOfProtectionDamagePerProtectionLevel = BUILDER.comment("The percentage of protection damage per protection level, default = 0.04, this is 4% per total protection level")
                .define("Percentage of Protection Damage Per Protection Level", 0.5f);
        maxStepAssistAmount = BUILDER.comment("The max amount of step assist levels that can be applied to tools, default = 1, 0 disables. Each level adds an additional 1 block to the step assist height")
                .define("Max Step Assist Level", 1);
        timeForDamageOnNightVision = BUILDER.comment("The time for damage on night vision, default = 600 (5 second), 20 ticks is 1 second")
                .define("Time for Damage on Night Vision", 600);
        timeForDamageOnWaterBreathing = BUILDER.comment("The time for damage on water breathing, default = 200 (5 second), 20 ticks is 1 second")
                .define("Time for Damage on Water Breathing", 100);
        maxSpeedAmount = BUILDER.comment("The max amount of speed levels that can be applied to tools, default = 5, 0 disables. Each level adds an additional level of the speed enchantment")
                .define("Max Speed Level", 5);
        maxFeatherFallingAmount = BUILDER.comment("The max amount of feather falling levels that can be applied to tools, default = 5, 0 disables. Each level adds an additional level of the feather falling enchantment")
                .define("Max Feather Falling Level", 5);
        maxJetsAmount = BUILDER.comment("The max amount of jets levels that can be applied to armor, default = 5, 0 disables. Each level increases speed")
                .define("Max Jets Level", 5);




        BUILDER.pop();



        //LAST
        SPEC = BUILDER.build();

    }

}
