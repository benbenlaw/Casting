package com.benbenlaw.casting.event.client;

import com.benbenlaw.casting.Casting;
import com.benbenlaw.casting.config.ToolModifierConfig;
import com.benbenlaw.casting.item.CastingDataComponents;
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

@EventBusSubscriber(modid = Casting.MOD_ID ,value = Dist.CLIENT)

public class ToolModifiersTooltip {

    @SubscribeEvent
    public static void onItemTooltip(ItemTooltipEvent event) {

        ItemStack tool = event.getItemStack();
        List<Component> components = event.getToolTip();

        //Tool Modifiers
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

        boolean hasEffects = hasExcavation || hasIgnite || hasLifesteal || hasKnockback || hasBeheading || hasSharpness || hasLooting || hasAutoSmelt || hasTorchPlacing || hasRepairing || hasUnbreaking || hasFortune || hasEfficiency || hasSilkTouch;

        if (Screen.hasShiftDown() && (hasEffects)) {

            int index = 1;
            components.add(1, Component.translatable("tooltips.casting.tool_modifiers").withStyle(ChatFormatting.GOLD));
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
                int totalAdditionalDamage = (int) (ToolModifierConfig.additionalMultiplierForSharpness.get() * sharpnessLevel + ToolModifierConfig.additionalAdditionForSharpness.get());
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

        } else {
            if (hasEffects) {
                components.add(1, Component.translatable("tooltips.item.shift.not_held").withStyle(ChatFormatting.YELLOW));
            }
        }
    }
}
