package com.benbenlaw.casting.util;

import com.benbenlaw.casting.config.EquipmentModifierConfig;
import com.benbenlaw.casting.item.CastingDataComponents;
import net.minecraft.world.item.ItemStack;

import java.util.List;

import static com.benbenlaw.casting.util.ValidToolTypesForToolModifiers.*;

public class EquipmentModifierUtils {

    public static boolean isEffectAtMax(ItemStack stack, String effect) {
        boolean hasSilkTouch = stack.getOrDefault(CastingDataComponents.SILK_TOUCH, false);
        int currentFortune = stack.getOrDefault(CastingDataComponents.FORTUNE, 0);

        if (effect.contains(FORTUNE)) {
            return hasSilkTouch || currentFortune >= EquipmentModifierConfig.maxFortuneAmount.get();
        }
        if (effect.contains(EFFICIENCY)) {
            int currentEfficiency = stack.getOrDefault(CastingDataComponents.EFFICIENCY, 0);
            return currentEfficiency >= EquipmentModifierConfig.maxEfficiencyAmount.get();
        }
        if (effect.contains(SILK_TOUCH)) {
            return currentFortune > 0 || hasSilkTouch;
        }
        if (effect.contains(UNBREAKING)) {
            int currentUnbreaking = stack.getOrDefault(CastingDataComponents.UNBREAKING, 0);
            return currentUnbreaking >= EquipmentModifierConfig.maxUnbreakingAmount.get();
        }
        if (effect.contains(REPAIRING)) {
            int currentRepairing = stack.getOrDefault(CastingDataComponents.REPAIRING, 0);
            return currentRepairing >= EquipmentModifierConfig.maxRepairingAmount.get();
        }
        if (effect.contains(TORCH_PLACING)) {
            return stack.getOrDefault(CastingDataComponents.TORCH_PLACING, false);
        }
        if (effect.contains(AUTO_SMELT)) {
            return stack.getOrDefault(CastingDataComponents.AUTO_SMELT, false);
        }
        if (effect.contains(LOOTING)) {
            int currentLooting = stack.getOrDefault(CastingDataComponents.LOOTING, 0);
            return currentLooting >= EquipmentModifierConfig.maxLootingAmount.get();
        }
        if (effect.contains(SHARPNESS)) {
            int currentSharpness = stack.getOrDefault(CastingDataComponents.SHARPNESS, 0);
            return currentSharpness >= EquipmentModifierConfig.maxSharpnessAmount.get();
        }
        if (effect.contains(BEHEADING)) {
            return stack.getOrDefault(CastingDataComponents.BEHEADING, false);
        }
        if (effect.contains(LIFESTEAL)) {
            int currentLifesteal = stack.getOrDefault(CastingDataComponents.LIFESTEAL, 0);
            return currentLifesteal >= EquipmentModifierConfig.maxLifestealAmount.get();
        }
        if (effect.contains(PROTECTION)) {
            int currentProtection = stack.getOrDefault(CastingDataComponents.PROTECTION, 0);
            return currentProtection >= EquipmentModifierConfig.maxProtectionAmount.get();
        }
        if (effect.contains(KNOCKBACK)) {
            int currentKnockback = stack.getOrDefault(CastingDataComponents.KNOCKBACK, 0);
            return currentKnockback >= EquipmentModifierConfig.maxKnockbackAmount.get();
        }
        if (effect.contains(IGNITE)) {
            int currentKnockback = stack.getOrDefault(CastingDataComponents.IGNITE, 0);
            return currentKnockback >= EquipmentModifierConfig.maxIgniteAmount.get();
        }
        if (effect.contains(EXCAVATION)) {
            int currentKnockback = stack.getOrDefault(CastingDataComponents.EXCAVATION, 0);
            return currentKnockback >= EquipmentModifierConfig.maxExcavationAmount.get();
        }
        if (effect.contains(TELEPORTING)) {
            int currentTeleporting = stack.getOrDefault(CastingDataComponents.TELEPORTING, 0);
            return currentTeleporting >= EquipmentModifierConfig.maxTeleportationAmount.get();
        }
        if (effect.contains(MAGNET)) {
            int currentTeleporting = stack.getOrDefault(CastingDataComponents.MAGNET, 0);
            return currentTeleporting >= EquipmentModifierConfig.maxMagnetAmount.get();
        }
        if (effect.contains(STEP_ASSIST)) {
            int currentEfficiency = stack.getOrDefault(CastingDataComponents.STEP_ASSIST, 0);
            return currentEfficiency >= EquipmentModifierConfig.maxStepAssistAmount.get();
        }
        if (effect.contains(NIGHT_VISION)) {
            return stack.getOrDefault(CastingDataComponents.NIGHT_VISION, false);
        }
        if (effect.contains(WATER_BREATHING)) {
            return stack.getOrDefault(CastingDataComponents.WATER_BREATHING, false);
        }
        if (effect.contains(SPEED)) {
            int currentTeleporting = stack.getOrDefault(CastingDataComponents.SPEED, 0);
            return currentTeleporting >= EquipmentModifierConfig.maxSpeedAmount.get();
        }
        if (effect.contains(WATER_WALKER)) {
            return stack.getOrDefault(CastingDataComponents.WATER_WALKER, false);
        }
        if (effect.contains(LAVA_WALKER)) {
            return stack.getOrDefault(CastingDataComponents.LAVA_WALKER, false);
        }
        if (effect.contains(FEATHER_FALLING)) {
            int currentFeatherFalling = stack.getOrDefault(CastingDataComponents.FEATHER_FALLING, 0);
            return currentFeatherFalling >= EquipmentModifierConfig.maxFeatherFallingAmount.get();
        }

        return false;
    }



    public static ItemStack copyAndApplyEffect(ItemStack stack, String effect) {
        ItemStack copy = stack.copy();

        if (effect.contains(EQUIPMENT_LEVEL)) {
            int currentEfficiency = copy.getOrDefault(CastingDataComponents.EQUIPMENT_LEVEL, 0);
            int newToolLevel = Math.min(currentEfficiency + 1, EquipmentModifierConfig.maxEquipmentLevel.get());
            copy.set(CastingDataComponents.EQUIPMENT_LEVEL, newToolLevel);
        }
        if (effect.contains(FORTUNE)) {
            int currentFortune = copy.getOrDefault(CastingDataComponents.FORTUNE, 0);
            int newFortune = Math.min(currentFortune + 1, EquipmentModifierConfig.maxFortuneAmount.get());
            copy.set(CastingDataComponents.FORTUNE, newFortune);
        }
        if (effect.contains(EFFICIENCY)) {
            int currentEfficiency = copy.getOrDefault(CastingDataComponents.EFFICIENCY, 0);
            int newEfficiency = Math.min(currentEfficiency + 1, EquipmentModifierConfig.maxEfficiencyAmount.get());
            copy.set(CastingDataComponents.EFFICIENCY, newEfficiency);
        }
        if (effect.contains(SILK_TOUCH)) {
            boolean isSilkTouch = copy.getOrDefault(CastingDataComponents.SILK_TOUCH, false);
            copy.set(CastingDataComponents.SILK_TOUCH, !isSilkTouch);
        }
        if (effect.contains(UNBREAKING)) {
            int currentUnbreaking = copy.getOrDefault(CastingDataComponents.UNBREAKING, 0);
            int newUnbreaking = Math.min(currentUnbreaking + 1, EquipmentModifierConfig.maxUnbreakingAmount.get());
            copy.set(CastingDataComponents.UNBREAKING, newUnbreaking);
        }
        if (effect.contains(REPAIRING)) {
            int currentRepairing = copy.getOrDefault(CastingDataComponents.REPAIRING, 0);
            int newRepairing = Math.min(currentRepairing + 1, EquipmentModifierConfig.maxRepairingAmount.get());
            copy.set(CastingDataComponents.REPAIRING, newRepairing);
        }
        if (effect.contains(TORCH_PLACING)) {
            boolean isTorchPlacing = copy.getOrDefault(CastingDataComponents.TORCH_PLACING, false);
            copy.set(CastingDataComponents.TORCH_PLACING, !isTorchPlacing);
        }
        if (effect.contains(AUTO_SMELT)) {
            boolean isAutoSmelt = copy.getOrDefault(CastingDataComponents.AUTO_SMELT, false);
            copy.set(CastingDataComponents.AUTO_SMELT, !isAutoSmelt);
        }
        if (effect.contains(LOOTING)) {
            int currentLooting = copy.getOrDefault(CastingDataComponents.LOOTING, 0);
            int newLooting = Math.min(currentLooting + 1, EquipmentModifierConfig.maxLootingAmount.get());
            copy.set(CastingDataComponents.LOOTING, newLooting);
        }
        if (effect.contains(SHARPNESS)) {
            int currentSharpness = copy.getOrDefault(CastingDataComponents.SHARPNESS, 0);
            int newSharpness = Math.min(currentSharpness + 1, EquipmentModifierConfig.maxSharpnessAmount.get());
            copy.set(CastingDataComponents.SHARPNESS, newSharpness);
        }
        if (effect.contains(BEHEADING)) {
            boolean isBeheading = copy.getOrDefault(CastingDataComponents.BEHEADING, false);
            copy.set(CastingDataComponents.BEHEADING, !isBeheading);
        }
        if (effect.contains(LIFESTEAL)) {
            int currentLifesteal = copy.getOrDefault(CastingDataComponents.LIFESTEAL, 0);
            int newLifesteal = Math.min(currentLifesteal + 1, EquipmentModifierConfig.maxLifestealAmount.get());
            copy.set(CastingDataComponents.LIFESTEAL, newLifesteal);
        }
        if (effect.contains(PROTECTION)) {
            int currentProtection = copy.getOrDefault(CastingDataComponents.PROTECTION, 0);
            int newProtection = Math.min(currentProtection + 1, EquipmentModifierConfig.maxProtectionAmount.get());
            copy.set(CastingDataComponents.PROTECTION, newProtection);
        }
        if (effect.contains(KNOCKBACK)) {
            int currentKnockback = copy.getOrDefault(CastingDataComponents.KNOCKBACK, 0);
            int newKnockback = Math.min(currentKnockback + 1, EquipmentModifierConfig.maxKnockbackAmount.get());
            copy.set(CastingDataComponents.KNOCKBACK, newKnockback);
        }
        if (effect.contains(IGNITE)) {
            int currentIgnite = copy.getOrDefault(CastingDataComponents.IGNITE, 0);
            int newIgnite = Math.min(currentIgnite + 1, EquipmentModifierConfig.maxIgniteAmount.get());
            copy.set(CastingDataComponents.IGNITE, newIgnite);
        }
        if (effect.contains(EXCAVATION)) {
            int currentExcavation = copy.getOrDefault(CastingDataComponents.EXCAVATION, 0);
            int newExcavation = Math.min(currentExcavation + 1, EquipmentModifierConfig.maxExcavationAmount.get());
            copy.set(CastingDataComponents.EXCAVATION, newExcavation);
        }
        if (effect.contains(TELEPORTING)) {
            int currentTeleporting = copy.getOrDefault(CastingDataComponents.TELEPORTING, 0);
            int newTeleporting = Math.min(currentTeleporting + 1, EquipmentModifierConfig.maxTeleportationAmount.get());
            copy.set(CastingDataComponents.TELEPORTING, newTeleporting);
        }
        if (effect.contains(MAGNET)) {
            int currentMagnet = copy.getOrDefault(CastingDataComponents.MAGNET, 0);
            int newMagnet = Math.min(currentMagnet + 1, EquipmentModifierConfig.maxMagnetAmount.get());
            copy.set(CastingDataComponents.MAGNET, newMagnet);
        }
        if (effect.contains(STEP_ASSIST)) {
            int currentStepAssist = copy.getOrDefault(CastingDataComponents.STEP_ASSIST, 0);
            int newStepAssist = Math.min(currentStepAssist + 1, EquipmentModifierConfig.maxStepAssistAmount.get());
            copy.set(CastingDataComponents.STEP_ASSIST, newStepAssist);
        }
        if (effect.contains(NIGHT_VISION)) {
            boolean isNightVision = copy.getOrDefault(CastingDataComponents.NIGHT_VISION, false);
            copy.set(CastingDataComponents.NIGHT_VISION, !isNightVision);
        }
        if (effect.contains(WATER_BREATHING)) {
            boolean isWaterBreathing = copy.getOrDefault(CastingDataComponents.WATER_BREATHING, false);
            copy.set(CastingDataComponents.WATER_BREATHING, !isWaterBreathing);
        }
        if (effect.contains(SPEED)) {
            int currentSpeed = copy.getOrDefault(CastingDataComponents.SPEED, 0);
            int newSpeed = Math.min(currentSpeed + 1, EquipmentModifierConfig.maxSpeedAmount.get());
            copy.set(CastingDataComponents.SPEED, newSpeed);
        }
        if (effect.contains(WATER_WALKER)) {
            boolean isWaterWalker = copy.getOrDefault(CastingDataComponents.WATER_WALKER, false);
            copy.set(CastingDataComponents.WATER_WALKER, !isWaterWalker);
        }
        if (effect.contains(LAVA_WALKER)) {
            boolean isLavaWalker = copy.getOrDefault(CastingDataComponents.LAVA_WALKER, false);
            copy.set(CastingDataComponents.LAVA_WALKER, !isLavaWalker);
        }
        if (effect.contains(FLIGHT)) {
            boolean isFlight = copy.getOrDefault(CastingDataComponents.FLIGHT, false);
            copy.set(CastingDataComponents.FLIGHT, !isFlight);
        }
        if (effect.contains(FEATHER_FALLING)) {
            int currentSpeed = copy.getOrDefault(CastingDataComponents.FEATHER_FALLING, 0);
            int newFeatherFalling = Math.min(currentSpeed + 1, EquipmentModifierConfig.maxFeatherFallingAmount.get());
            copy.set(CastingDataComponents.FEATHER_FALLING, newFeatherFalling);
        }


        return copy;
    }

    public static boolean hasEnoughFreeModifiers(ItemStack stack, String effectToApply) {

        if (!stack.has(CastingDataComponents.EQUIPMENT_LEVEL.get())) {
            return true;
        }

        var effectComponent = getDataComponentFromString(effectToApply);

        // If the tool already has the effect, skip modifier count check
        assert effectComponent != null;
        if (stack.has(effectComponent)) {
            return true;
        }

        int toolLevel = stack.getOrDefault(CastingDataComponents.EQUIPMENT_LEVEL.get(), 0);
        int modifierCount = 0;

        List<String> modifiers = ValidToolTypesForToolModifiers.VALID_MODIFIERS.get(ALL_MODIFIERS);;

        for (String modifier : modifiers) {
            if (modifier.equals(effectToApply)) {
                continue;
            }

            var component = getDataComponentFromString(modifier);
            if (component != null && stack.has(component)) {
                modifierCount++;
            }
        }

        return toolLevel > modifierCount;
    }

    public static double getExperienceModifierLevel(int toolLevel) {
        return toolLevel * EquipmentModifierConfig.experienceMultiplierPerLevel.get();

    }
}
