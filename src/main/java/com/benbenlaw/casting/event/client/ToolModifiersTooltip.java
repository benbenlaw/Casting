package com.benbenlaw.casting.event.client;

import com.benbenlaw.casting.Casting;
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


        if (Screen.hasShiftDown() && (hasSilkTouch || hasEfficiency || hasFortune || hasUnbreaking || hasRepairing)) {

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

        } else {
            if (hasSilkTouch || hasEfficiency || hasFortune || hasUnbreaking || hasRepairing) {
                components.add(1, Component.translatable("tooltips.item.shift.not_held").withStyle(ChatFormatting.YELLOW));
            }
        }
    }
}
