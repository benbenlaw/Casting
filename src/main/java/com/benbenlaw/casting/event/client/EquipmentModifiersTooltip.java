package com.benbenlaw.casting.event.client;

import com.benbenlaw.casting.Casting;
import com.benbenlaw.casting.config.EquipmentModifierConfig;
import com.benbenlaw.casting.item.CastingItems;
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

import static com.benbenlaw.casting.item.EquipmentModifier.*;
import static com.benbenlaw.casting.util.EquipmentModifierUtils.getExperienceModifierLevel;

@EventBusSubscriber(modid = Casting.MOD_ID ,value = Dist.CLIENT)

public class EquipmentModifiersTooltip {

    @SubscribeEvent
    public static void addRepairingMoldTooltip(ItemTooltipEvent event) {
        ItemStack stack = event.getItemStack();
        if (stack.is(CastingItems.REPAIRING_MOLD.get())) {

            if (Screen.hasShiftDown()) {

                event.getToolTip().add(Component.translatable("tooltips.casting.stats.repairing_mold").withStyle(ChatFormatting.YELLOW));
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
        boolean hasEquipmentLevel = tool.getComponents().keySet().contains(EQUIPMENT_LEVEL.dataComponent.get());
        boolean hasSilkTouch = tool.getComponents().keySet().contains(SILK_TOUCH.dataComponent.get());
        boolean hasEfficiency = tool.getComponents().keySet().contains(EFFICIENCY.dataComponent.get());
        boolean hasFortune = tool.getComponents().keySet().contains(FORTUNE.dataComponent.get());
        boolean hasUnbreaking = tool.getComponents().keySet().contains(UNBREAKING.dataComponent.get());
        boolean hasRepairing = tool.getComponents().keySet().contains(REPAIRING.dataComponent.get());
        boolean hasTorchPlacing = tool.getComponents().keySet().contains(TORCH_PLACING.dataComponent.get());
        boolean hasAutoSmelt = tool.getComponents().keySet().contains(AUTO_SMELT.dataComponent.get());
        boolean hasLooting = tool.getComponents().keySet().contains(LOOTING.dataComponent.get());
        boolean hasSharpness = tool.getComponents().keySet().contains(SHARPNESS.dataComponent.get());
        boolean hasBeheading = tool.getComponents().keySet().contains(BEHEADING.dataComponent.get());
        boolean hasLifesteal = tool.getComponents().keySet().contains(LIFESTEAL.dataComponent.get());
        boolean hasKnockback = tool.getComponents().keySet().contains(KNOCKBACK.dataComponent.get());
        boolean hasIgnite = tool.getComponents().keySet().contains(IGNITE.dataComponent.get());
        boolean hasExcavation = tool.getComponents().keySet().contains(EXCAVATION.dataComponent.get());
        boolean hasTeleporting = tool.getComponents().keySet().contains(TELEPORTING.dataComponent.get());
        boolean hasMagnet = tool.getComponents().keySet().contains(MAGNET.dataComponent.get());
        boolean hasProtection = tool.getComponents().keySet().contains(PROTECTION.dataComponent.get());
        boolean hasStepAssist = tool.getComponents().keySet().contains(STEP_ASSIST.dataComponent.get());
        boolean hasNightVision = tool.getComponents().keySet().contains(NIGHT_VISION.dataComponent.get());
        boolean hasWaterBreathing = tool.getComponents().keySet().contains(WATER_BREATHING.dataComponent.get());
        boolean hasWalterWalker = tool.getComponents().keySet().contains(WATER_WALKER.dataComponent.get());
        boolean hasLavaWalker = tool.getComponents().keySet().contains(LAVA_WALKER.dataComponent.get());
        boolean hasSpeed = tool.getComponents().keySet().contains(SPEED.dataComponent.get());
        boolean hasFlight = tool.getComponents().keySet().contains(FLIGHT.dataComponent.get());
        boolean hasFeatherFalling = tool.getComponents().keySet().contains(FEATHER_FALLING.dataComponent.get());

        boolean hasToggleableModifiers = tool.getComponents().keySet().contains(TOGGLEABLE_MODIFIERS.get());

        boolean hasEffects = hasFeatherFalling || hasFlight || hasWalterWalker || hasLavaWalker || hasSpeed || hasWaterBreathing || hasNightVision || hasEquipmentLevel || hasStepAssist || hasProtection || hasMagnet || hasTeleporting || hasExcavation || hasIgnite || hasLifesteal || hasKnockback || hasBeheading || hasSharpness || hasLooting || hasAutoSmelt || hasTorchPlacing || hasRepairing || hasUnbreaking || hasFortune || hasEfficiency || hasSilkTouch;

        if (Screen.hasShiftDown() && (hasEffects)) {

            int index = 1;
            int toolLevel = (int) tool.getComponents().getOrDefault(EQUIPMENT_LEVEL.dataComponent.get(), 0);

            if (hasEquipmentLevel) {
                int currentExperience = Optional.ofNullable(tool.getComponents().get(EQUIPMENT_EXPERIENCE.get())).orElse(0);
                double modifierLevel = getExperienceModifierLevel(toolLevel);
                int totalExperienceToNextLevel = (int) (EquipmentModifierConfig.experiencePerLevelForEquipmentLevel.get() + (EquipmentModifierConfig.experiencePerLevelForEquipmentLevel.get() * modifierLevel));
                components.add(index, Component.translatable("tooltips.casting.stats.equipment_level", toolLevel, currentExperience, totalExperienceToNextLevel).withStyle(ChatFormatting.GOLD));
                index++;
            }
            components.add(index, Component.empty());
            index++;
            if (hasSilkTouch) {

                if (hasToggleableModifiers && Boolean.TRUE.equals(tool.getComponents().get(TOGGLEABLE_MODIFIERS.get()))) {
                    components.add(index, Component.translatable("tooltips.casting.stats.silk_touch")
                            .withStyle(ChatFormatting.BLUE).append(Component.literal(" (✔)").withStyle(ChatFormatting.GREEN)));
                }
                else if (hasToggleableModifiers && Boolean.FALSE.equals(tool.getComponents().get(TOGGLEABLE_MODIFIERS.get()))) {
                    components.add(index, Component.translatable("tooltips.casting.stats.silk_touch")
                            .withStyle(ChatFormatting.BLUE).append(Component.literal(" (❌)").withStyle(ChatFormatting.RED)));
                }
                else {
                    components.add(index, Component.translatable("tooltips.casting.stats.silk_touch").withStyle(ChatFormatting.BLUE));
                }

                index++;
            }
            if (hasEfficiency) {
                int efficiencyLevel = Optional.ofNullable((int) tool.getComponents().get(EFFICIENCY.dataComponent.get())).orElse(0);
                components.add(index, Component.translatable("tooltips.casting.stats.efficiency", efficiencyLevel).withStyle(ChatFormatting.BLUE));
                index++;
            }
            if (hasFortune) {
                int fortuneLevel = Optional.ofNullable((int) tool.getComponents().get(FORTUNE.dataComponent.get())).orElse(0);
                components.add(index, Component.translatable("tooltips.casting.stats.fortune", fortuneLevel).withStyle(ChatFormatting.BLUE));
                index++;
            }
            if (hasUnbreaking) {
                int unbreakingLevel = Optional.ofNullable((int) tool.getComponents().get(UNBREAKING.dataComponent.get())).orElse(0);
                components.add(index, Component.translatable("tooltips.casting.stats.unbreaking", unbreakingLevel).withStyle(ChatFormatting.BLUE));
                index++;
            }
            if (hasRepairing) {
                int repairingLevel = Optional.ofNullable((int) tool.getComponents().get(REPAIRING.dataComponent.get())).orElse(0);
                components.add(index, Component.translatable("tooltips.casting.stats.repairing", repairingLevel).withStyle(ChatFormatting.BLUE));
                index++;
            }
            if (hasTorchPlacing) {
                components.add(index, Component.translatable("tooltips.casting.stats.torch_placing").withStyle(ChatFormatting.BLUE));
                index++;
            }
            if (hasAutoSmelt) {


                if (hasToggleableModifiers && Boolean.TRUE.equals(tool.getComponents().get(TOGGLEABLE_MODIFIERS.get()))) {
                    components.add(index, Component.translatable("tooltips.casting.stats.auto_smelt")
                            .withStyle(ChatFormatting.BLUE).append(Component.literal(" (✔)").withStyle(ChatFormatting.GREEN)));
                }
                else if (hasToggleableModifiers && Boolean.FALSE.equals(tool.getComponents().get(TOGGLEABLE_MODIFIERS.get()))) {
                    components.add(index, Component.translatable("tooltips.casting.stats.auto_smelt")
                            .withStyle(ChatFormatting.BLUE).append(Component.literal(" (❌)").withStyle(ChatFormatting.RED)));
                }
                else {
                    components.add(index, Component.translatable("tooltips.casting.stats.auto_smelt").withStyle(ChatFormatting.BLUE));
                }

                index++;

            }
            if (hasLooting) {
                int lootingLevel = Optional.ofNullable((int) tool.getComponents().get(LOOTING.dataComponent.get())).orElse(0);
                components.add(index, Component.translatable("tooltips.casting.stats.looting", lootingLevel).withStyle(ChatFormatting.BLUE));
                index++;
            }
            if (hasSharpness) {
                int sharpnessLevel = Optional.ofNullable((int) tool.getComponents().get(SHARPNESS.dataComponent.get())).orElse(0);
                int totalAdditionalDamage = (int) (EquipmentModifierConfig.additionalMultiplierForSharpness.get() * sharpnessLevel + EquipmentModifierConfig.additionalAdditionForSharpness.get());
                components.add(index, Component.translatable("tooltips.casting.stats.sharpness", sharpnessLevel, totalAdditionalDamage).withStyle(ChatFormatting.BLUE));
                index++;
            }
            if (hasBeheading) {
                components.add(index, Component.translatable("tooltips.casting.stats.beheading").withStyle(ChatFormatting.BLUE));
                index++;
            }
            if (hasLifesteal) {
                int lifestealLevel = Optional.ofNullable((int) tool.getComponents().get(LIFESTEAL.dataComponent.get())).orElse(0);
                int percentageRestored = 10 * lifestealLevel;
                components.add(index, Component.translatable("tooltips.casting.stats.lifesteal", lifestealLevel, percentageRestored).withStyle(ChatFormatting.BLUE));
                index++;
            }
            if (hasKnockback) {
                int knockbackLevel = Optional.ofNullable((int) tool.getComponents().get(KNOCKBACK.dataComponent.get())).orElse(0);
                components.add(index, Component.translatable("tooltips.casting.stats.knockback", knockbackLevel).withStyle(ChatFormatting.BLUE));
                index++;
            }
            if (hasIgnite) {
                int igniteLevel = Optional.ofNullable((int) tool.getComponents().get(IGNITE.dataComponent.get())).orElse(0);
                components.add(index, Component.translatable("tooltips.casting.stats.ignite", igniteLevel).withStyle(ChatFormatting.BLUE));
                index++;
            }
            if (hasExcavation) {

                int excavationLevel = Optional.ofNullable((int) tool.getComponents().get(EXCAVATION.dataComponent.get())).orElse(0);
                int excavationArea = 1 + (excavationLevel * 2);

                if (hasToggleableModifiers && Boolean.TRUE.equals(tool.getComponents().get(TOGGLEABLE_MODIFIERS.get()))) {
                    components.add(index, Component.translatable("tooltips.casting.stats.excavation", excavationLevel, excavationArea, excavationArea)
                            .withStyle(ChatFormatting.BLUE).append(Component.literal(" (✔)").withStyle(ChatFormatting.GREEN)));
                }
                else if (hasToggleableModifiers && Boolean.FALSE.equals(tool.getComponents().get(TOGGLEABLE_MODIFIERS.get()))) {
                    components.add(index, Component.translatable("tooltips.casting.stats.excavation", excavationLevel, excavationArea, excavationArea)
                            .withStyle(ChatFormatting.BLUE).append(Component.literal(" (❌)").withStyle(ChatFormatting.RED)));
                }
                else {
                    components.add(index, Component.translatable("tooltips.casting.stats.excavation", excavationLevel, excavationArea, excavationArea).withStyle(ChatFormatting.BLUE));
                }

                index++;


            }
            if (hasTeleporting) {
                int teleportingLevel = Optional.ofNullable((int) tool.getComponents().get(TELEPORTING.dataComponent.get())).orElse(0);
                int blockAmount = EquipmentModifierConfig.blocksPerLevelForTeleporting.get() * teleportingLevel;
                components.add(index, Component.translatable("tooltips.casting.stats.teleporting", teleportingLevel, blockAmount).withStyle(ChatFormatting.BLUE));
                index++;
            }
            if (hasMagnet) {
                int magnetLevel = Optional.ofNullable((int) tool.getComponents().get(MAGNET.dataComponent.get())).orElse(0);

                if (hasToggleableModifiers && Boolean.TRUE.equals(tool.getComponents().get(TOGGLEABLE_MODIFIERS.get()))) {
                    components.add(index, Component.translatable("tooltips.casting.stats.magnet", magnetLevel)
                            .withStyle(ChatFormatting.BLUE).append(Component.literal(" (✔)").withStyle(ChatFormatting.GREEN)));
                }
                else if (hasToggleableModifiers && Boolean.FALSE.equals(tool.getComponents().get(TOGGLEABLE_MODIFIERS.get()))) {
                    components.add(index, Component.translatable("tooltips.casting.stats.magnet", magnetLevel)
                            .withStyle(ChatFormatting.BLUE).append(Component.literal(" (❌)").withStyle(ChatFormatting.RED)));
                }
                else {
                    components.add(index, Component.translatable("tooltips.casting.stats.magnet", magnetLevel).withStyle(ChatFormatting.BLUE));
                }

                index++;
            }
            if (hasProtection) {
                int protectionLevel = Optional.ofNullable((int) tool.getComponents().get(PROTECTION.dataComponent.get())).orElse(0);
                components.add(index, Component.translatable("tooltips.casting.stats.protection", protectionLevel).withStyle(ChatFormatting.BLUE));
                index++;
            }
            if (hasStepAssist) {

                int stepAssistLevel = Optional.ofNullable((int) tool.getComponents().get(STEP_ASSIST.dataComponent.get())).orElse(0);

                if (hasToggleableModifiers && Boolean.TRUE.equals(tool.getComponents().get(TOGGLEABLE_MODIFIERS.get()))) {
                    components.add(index, Component.translatable("tooltips.casting.stats.step_assist", stepAssistLevel)
                            .withStyle(ChatFormatting.BLUE).append(Component.literal(" (✔)").withStyle(ChatFormatting.GREEN)));
                }
                else if (hasToggleableModifiers && Boolean.FALSE.equals(tool.getComponents().get(TOGGLEABLE_MODIFIERS.get()))) {
                    components.add(index, Component.translatable("tooltips.casting.stats.step_assist", stepAssistLevel)
                            .withStyle(ChatFormatting.BLUE).append(Component.literal(" (❌)").withStyle(ChatFormatting.RED)));
                }
                else {
                    components.add(index, Component.translatable("tooltips.casting.stats.step_assist", stepAssistLevel).withStyle(ChatFormatting.BLUE));
                }

                index++;
            }
            if (hasNightVision) {

                if (hasToggleableModifiers && Boolean.TRUE.equals(tool.getComponents().get(TOGGLEABLE_MODIFIERS.get()))) {
                    components.add(index, Component.translatable("tooltips.casting.stats.night_vision")
                            .withStyle(ChatFormatting.BLUE).append(Component.literal(" (✔)").withStyle(ChatFormatting.GREEN)));
                }
                else if (hasToggleableModifiers && Boolean.FALSE.equals(tool.getComponents().get(TOGGLEABLE_MODIFIERS.get()))) {
                    components.add(index, Component.translatable("tooltips.casting.stats.night_vision")
                            .withStyle(ChatFormatting.BLUE).append(Component.literal(" (❌)").withStyle(ChatFormatting.RED)));
                }
                else {
                    components.add(index, Component.translatable("tooltips.casting.stats.night_vision").withStyle(ChatFormatting.BLUE));
                }
                index++;
            }
            if (hasWaterBreathing) {
                components.add(index, Component.translatable("tooltips.casting.stats.water_breathing").withStyle(ChatFormatting.BLUE));
                index++;
            }
            if (hasWalterWalker) {
                if (hasToggleableModifiers && Boolean.TRUE.equals(tool.getComponents().get(TOGGLEABLE_MODIFIERS.get()))) {
                    components.add(index, Component.translatable("tooltips.casting.stats.water_walker")
                            .withStyle(ChatFormatting.BLUE).append(Component.literal(" (✔)").withStyle(ChatFormatting.GREEN)));
                }
                else if (hasToggleableModifiers && Boolean.FALSE.equals(tool.getComponents().get(TOGGLEABLE_MODIFIERS.get()))) {
                    components.add(index, Component.translatable("tooltips.casting.stats.water_walker")
                            .withStyle(ChatFormatting.BLUE).append(Component.literal(" (❌)").withStyle(ChatFormatting.RED)));
                }
                else {
                    components.add(index, Component.translatable("tooltips.casting.stats.water_walker").withStyle(ChatFormatting.BLUE));
                }
                index++;
            }
            if (hasLavaWalker) {
                if (hasToggleableModifiers && Boolean.TRUE.equals(tool.getComponents().get(TOGGLEABLE_MODIFIERS.get()))) {
                    components.add(index, Component.translatable("tooltips.casting.stats.lava_walker")
                            .withStyle(ChatFormatting.BLUE).append(Component.literal(" (✔)").withStyle(ChatFormatting.GREEN)));
                }
                else if (hasToggleableModifiers && Boolean.FALSE.equals(tool.getComponents().get(TOGGLEABLE_MODIFIERS.get()))) {
                    components.add(index, Component.translatable("tooltips.casting.stats.lava_walker")
                            .withStyle(ChatFormatting.BLUE).append(Component.literal(" (❌)").withStyle(ChatFormatting.RED)));
                }
                else {
                    components.add(index, Component.translatable("tooltips.casting.stats.lava_walker").withStyle(ChatFormatting.BLUE));
                }
                index++;
            }
            if (hasSpeed) {
                int speedLevel = Optional.ofNullable((int) tool.getComponents().get(SPEED.dataComponent.get())).orElse(0);

                if (hasToggleableModifiers && Boolean.TRUE.equals(tool.getComponents().get(TOGGLEABLE_MODIFIERS.get()))) {
                    components.add(index, Component.translatable("tooltips.casting.stats.speed", speedLevel)
                            .withStyle(ChatFormatting.BLUE).append(Component.literal(" (✔)").withStyle(ChatFormatting.GREEN)));
                }
                else if (hasToggleableModifiers && Boolean.FALSE.equals(tool.getComponents().get(TOGGLEABLE_MODIFIERS.get()))) {
                    components.add(index, Component.translatable("tooltips.casting.stats.speed", speedLevel)
                            .withStyle(ChatFormatting.BLUE).append(Component.literal(" (❌)").withStyle(ChatFormatting.RED)));
                }
                else {
                    components.add(index, Component.translatable("tooltips.casting.stats.speed", speedLevel).withStyle(ChatFormatting.BLUE));
                }
                index++;
            }
            if (hasFlight) {
                if (hasToggleableModifiers && Boolean.TRUE.equals(tool.getComponents().get(TOGGLEABLE_MODIFIERS.get()))) {
                    components.add(index, Component.translatable("tooltips.casting.stats.flight")
                            .withStyle(ChatFormatting.BLUE).append(Component.literal(" (✔)").withStyle(ChatFormatting.GREEN)));
                }
                else if (hasToggleableModifiers && Boolean.FALSE.equals(tool.getComponents().get(TOGGLEABLE_MODIFIERS.get()))) {
                    components.add(index, Component.translatable("tooltips.casting.stats.flight")
                            .withStyle(ChatFormatting.BLUE).append(Component.literal(" (❌)").withStyle(ChatFormatting.RED)));
                }
                else {
                    components.add(index, Component.translatable("tooltips.casting.stats.flight").withStyle(ChatFormatting.BLUE));
                }
                index++;
            }
            if (hasFeatherFalling) {
                int featherFallingLevel = Optional.ofNullable((int) tool.getComponents().get(FEATHER_FALLING.dataComponent.get())).orElse(0);
                components.add(index, Component.translatable("tooltips.casting.stats.feather_falling", featherFallingLevel).withStyle(ChatFormatting.BLUE));
                index++;
            }

            components.add(1, Component.translatable("tooltips.casting.stats.modifiers", index - 3, toolLevel).withStyle(ChatFormatting.GOLD));

        } else {
            if (hasEffects) {
                components.add(1, Component.translatable("tooltips.bblcore.shift").withStyle(ChatFormatting.YELLOW));
            }
        }
    }
}