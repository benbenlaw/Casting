package com.benbenlaw.casting.event.client;

import com.benbenlaw.casting.Casting;
import com.benbenlaw.casting.config.EquipmentModifierConfig;
import com.benbenlaw.casting.item.CastingDataComponents;
import com.benbenlaw.casting.item.ModItems;
import net.minecraft.ChatFormatting;
import net.minecraft.client.gui.screens.Screen;
import net.minecraft.network.chat.Component;
import net.minecraft.world.item.ItemStack;
import net.neoforged.api.distmarker.Dist;
import net.neoforged.bus.api.SubscribeEvent;
import net.neoforged.fml.common.EventBusSubscriber;
import net.neoforged.neoforge.event.entity.player.ItemTooltipEvent;

import java.util.List;
import java.util.Optional;

import static com.benbenlaw.casting.util.EquipmentModifierUtils.getExperienceModifierLevel;

@EventBusSubscriber(modid = Casting.MOD_ID ,value = Dist.CLIENT)

public class EquipmentModifiersTooltip {

    @SubscribeEvent
    public static void addRepairingMoldTooltip(ItemTooltipEvent event) {
        ItemStack stack = event.getItemStack();
        if (stack.is(ModItems.REPAIRING_MOLD.get())) {

            if (Screen.hasShiftDown()) {

                event.getToolTip().add(Component.translatable("tooltips.casting.repairing_mold").withStyle(ChatFormatting.YELLOW));
            } else {
                event.getToolTip().add(Component.translatable("tooltips.bblcore.shift").withStyle(ChatFormatting.YELLOW));
            }
        }
    }

    @SubscribeEvent
    public static void onItemTooltip(ItemTooltipEvent event) {

        ItemStack tool = event.getItemStack();
        List<Component> components = event.getToolTip();

        //Tool Modifiers
        boolean hasEquipmentLevel = tool.getComponents().keySet().contains(CastingDataComponents.EQUIPMENT_LEVEL.get());
        boolean hasSilkTouch = Boolean.TRUE.equals(tool.getComponents().get(CastingDataComponents.SILK_TOUCH.get()));
        boolean hasEfficiency = tool.getComponents().keySet().contains(CastingDataComponents.EFFICIENCY.get());
        boolean hasFortune = tool.getComponents().keySet().contains(CastingDataComponents.FORTUNE.get());
        boolean hasUnbreaking = tool.getComponents().keySet().contains(CastingDataComponents.UNBREAKING.get());
        boolean hasRepairing = tool.getComponents().keySet().contains(CastingDataComponents.REPAIRING.get());
        boolean hasTorchPlacing = tool.getComponents().keySet().contains(CastingDataComponents.TORCH_PLACING.get());
        boolean hasAutoSmelt = tool.getComponents().keySet().contains(CastingDataComponents.AUTO_SMELT.get());
        boolean hasLooting = tool.getComponents().keySet().contains(CastingDataComponents.LOOTING.get());
        boolean hasSharpness = tool.getComponents().keySet().contains(CastingDataComponents.SHARPNESS.get());
        boolean hasBeheading = tool.getComponents().keySet().contains(CastingDataComponents.BEHEADING.get());
        boolean hasLifesteal = tool.getComponents().keySet().contains(CastingDataComponents.LIFESTEAL.get());
        boolean hasKnockback = tool.getComponents().keySet().contains(CastingDataComponents.KNOCKBACK.get());
        boolean hasIgnite = tool.getComponents().keySet().contains(CastingDataComponents.IGNITE.get());
        boolean hasExcavation = tool.getComponents().keySet().contains(CastingDataComponents.EXCAVATION.get());
        boolean hasTeleporting = tool.getComponents().keySet().contains(CastingDataComponents.TELEPORTING.get());
        boolean hasMagnet = tool.getComponents().keySet().contains(CastingDataComponents.MAGNET.get());
        boolean hasProtection = tool.getComponents().keySet().contains(CastingDataComponents.PROTECTION.get());
        boolean hasStepAssist = tool.getComponents().keySet().contains(CastingDataComponents.STEP_ASSIST.get());
        boolean hasNightVision = tool.getComponents().keySet().contains(CastingDataComponents.NIGHT_VISION.get());
        boolean hasWaterBreathing = tool.getComponents().keySet().contains(CastingDataComponents.WATER_BREATHING.get());

        boolean hasEffects = hasWaterBreathing || hasNightVision || hasEquipmentLevel || hasStepAssist || hasProtection || hasMagnet || hasTeleporting || hasExcavation || hasIgnite || hasLifesteal || hasKnockback || hasBeheading || hasSharpness || hasLooting || hasAutoSmelt || hasTorchPlacing || hasRepairing || hasUnbreaking || hasFortune || hasEfficiency || hasSilkTouch;

        if (Screen.hasShiftDown() && (hasEffects)) {

            int index = 1;
            int toolLevel = Optional.ofNullable(tool.getComponents().get(CastingDataComponents.EQUIPMENT_LEVEL.get())).orElse(0);


            if (hasEquipmentLevel) {
                int currentExperience = Optional.ofNullable(tool.getComponents().get(CastingDataComponents.EQUIPMENT_EXPERIENCE.get())).orElse(0);
                double modifierLevel = getExperienceModifierLevel(toolLevel);
                int totalExperienceToNextLevel = (int) (EquipmentModifierConfig.experiencePerLevelForEquipmentLevel.get() + (EquipmentModifierConfig.experiencePerLevelForEquipmentLevel.get() * modifierLevel));
                components.add(index, Component.translatable("tooltips.casting.equipment_level", toolLevel, currentExperience, totalExperienceToNextLevel).withStyle(ChatFormatting.GOLD));
                index++;
            }
            components.add(index, Component.empty());
            index++;
            if (hasSilkTouch) {
                components.add(index, Component.translatable("tooltips.casting.silk_touch").withStyle(ChatFormatting.BLUE));
                index++;
            }
            if (hasEfficiency) {
                int efficiencyLevel = Optional.ofNullable(tool.getComponents().get(CastingDataComponents.EFFICIENCY.get())).orElse(0);
                components.add(index, Component.translatable("tooltips.casting.efficiency", efficiencyLevel).withStyle(ChatFormatting.BLUE));
                index++;
            }
            if (hasFortune) {
                int fortuneLevel = Optional.ofNullable(tool.getComponents().get(CastingDataComponents.FORTUNE.get())).orElse(0);
                components.add(index, Component.translatable("tooltips.casting.fortune", fortuneLevel).withStyle(ChatFormatting.BLUE));
                index++;
            }
            if (hasUnbreaking) {
                int unbreakingLevel = Optional.ofNullable(tool.getComponents().get(CastingDataComponents.UNBREAKING.get())).orElse(0);
                components.add(index, Component.translatable("tooltips.casting.unbreaking", unbreakingLevel).withStyle(ChatFormatting.BLUE));
                index++;
            }
            if (hasRepairing) {
                int repairingLevel = Optional.ofNullable(tool.getComponents().get(CastingDataComponents.REPAIRING.get())).orElse(0);
                components.add(index, Component.translatable("tooltips.casting.repairing", repairingLevel).withStyle(ChatFormatting.BLUE));
                index++;
            }
            if (hasTorchPlacing) {
                components.add(index, Component.translatable("tooltips.casting.torch_placing").withStyle(ChatFormatting.BLUE));
                index++;
            }
            if (hasAutoSmelt) {
                components.add(index, Component.translatable("tooltips.casting.auto_smelt").withStyle(ChatFormatting.BLUE));
                index++;
            }
            if (hasLooting) {
                int lootingLevel = Optional.ofNullable(tool.getComponents().get(CastingDataComponents.LOOTING.get())).orElse(0);
                components.add(index, Component.translatable("tooltips.casting.looting", lootingLevel).withStyle(ChatFormatting.BLUE));
                index++;
            }
            if (hasSharpness) {
                int sharpnessLevel = Optional.ofNullable(tool.getComponents().get(CastingDataComponents.SHARPNESS.get())).orElse(0);
                int totalAdditionalDamage = (int) (EquipmentModifierConfig.additionalMultiplierForSharpness.get() * sharpnessLevel + EquipmentModifierConfig.additionalAdditionForSharpness.get());
                components.add(index, Component.translatable("tooltips.casting.sharpness", sharpnessLevel, totalAdditionalDamage).withStyle(ChatFormatting.BLUE));
                index++;
            }
            if (hasBeheading) {
                components.add(index, Component.translatable("tooltips.casting.beheading").withStyle(ChatFormatting.BLUE));
                index++;
            }
            if (hasLifesteal) {
                int lifestealLevel = Optional.ofNullable(tool.getComponents().get(CastingDataComponents.LIFESTEAL.get())).orElse(0);
                int percentageRestored = 10 * lifestealLevel;
                components.add(index, Component.translatable("tooltips.casting.lifesteal", lifestealLevel, percentageRestored).withStyle(ChatFormatting.BLUE));
                index++;
            }
            if (hasKnockback) {
                int knockbackLevel = Optional.ofNullable(tool.getComponents().get(CastingDataComponents.KNOCKBACK.get())).orElse(0);
                components.add(index, Component.translatable("tooltips.casting.knockback", knockbackLevel).withStyle(ChatFormatting.BLUE));
                index++;
            }
            if (hasIgnite) {
                int igniteLevel = Optional.ofNullable(tool.getComponents().get(CastingDataComponents.IGNITE.get())).orElse(0);
                components.add(index, Component.translatable("tooltips.casting.ignite", igniteLevel).withStyle(ChatFormatting.BLUE));
                index++;
            }
            if (hasExcavation) {
                int excavationLevel = Optional.ofNullable(tool.getComponents().get(CastingDataComponents.EXCAVATION.get())).orElse(0);
                int excavationArea = 1 + (excavationLevel * 2);
                components.add(index, Component.translatable("tooltips.casting.excavation", excavationLevel, excavationArea, excavationArea).withStyle(ChatFormatting.BLUE));
                index++;
            }
            if (hasTeleporting) {
                int teleportingLevel = Optional.ofNullable(tool.getComponents().get(CastingDataComponents.TELEPORTING.get())).orElse(0);
                int blockAmount = EquipmentModifierConfig.blocksPerLevelForTeleporting.get() * teleportingLevel;
                components.add(index, Component.translatable("tooltips.casting.teleporting", teleportingLevel, blockAmount).withStyle(ChatFormatting.BLUE));
                index++;
            }
            if (hasMagnet) {
                int magnetLevel = Optional.ofNullable(tool.getComponents().get(CastingDataComponents.MAGNET.get())).orElse(0);
                components.add(index, Component.translatable("tooltips.casting.magnet", magnetLevel).withStyle(ChatFormatting.BLUE));
                index++;
            }
            if (hasProtection) {
                int protectionLevel = Optional.ofNullable(tool.getComponents().get(CastingDataComponents.PROTECTION.get())).orElse(0);
                components.add(index, Component.translatable("tooltips.casting.protection", protectionLevel).withStyle(ChatFormatting.BLUE));
                index++;
            }
            if (hasStepAssist) {
                int stepAssistLevel = Optional.ofNullable(tool.getComponents().get(CastingDataComponents.STEP_ASSIST.get())).orElse(0);
                components.add(index, Component.translatable("tooltips.casting.step_assist", stepAssistLevel).withStyle(ChatFormatting.BLUE));
                index++;
            }
            if (hasNightVision) {
                components.add(index, Component.translatable("tooltips.casting.night_vision").withStyle(ChatFormatting.BLUE));
                index++;
            }
            if (hasWaterBreathing) {
                components.add(index, Component.translatable("tooltips.casting.water_breathing").withStyle(ChatFormatting.BLUE));
                index++;
            }

            components.add(1, Component.translatable("tooltips.casting.modifiers", index - 3, toolLevel).withStyle(ChatFormatting.GOLD));

        } else {
            if (hasEffects) {
                components.add(1, Component.translatable("tooltips.bblcore.shift").withStyle(ChatFormatting.YELLOW));
            }
        }
    }
}
