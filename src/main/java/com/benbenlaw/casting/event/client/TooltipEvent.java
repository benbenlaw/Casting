package com.benbenlaw.casting.event.client;

import com.benbenlaw.casting.Casting;
import com.benbenlaw.casting.item.CastingDataComponents;
import com.benbenlaw.casting.item.CastingItems;
import com.benbenlaw.casting.item.util.FluidListComponent;
import com.benbenlaw.core.util.TooltipUtil;
import net.minecraft.ChatFormatting;
import net.minecraft.client.Minecraft;
import net.minecraft.network.chat.Component;
import net.minecraft.world.item.ItemStack;
import net.neoforged.bus.api.SubscribeEvent;
import net.neoforged.fml.common.EventBusSubscriber;
import net.neoforged.neoforge.event.entity.player.ItemTooltipEvent;
import net.neoforged.neoforge.fluids.FluidStack;

import java.util.List;

@EventBusSubscriber(modid = Casting.MOD_ID)
public class TooltipEvent {

    @SubscribeEvent
    public static void onTooltipEvent(ItemTooltipEvent event) {
        ItemStack stack = event.getItemStack();

        TooltipUtil.addShiftTooltip(stack, event, CastingItems.EXPERIENCE_BALL.get(), "tooltip.casting.experience_ball");
        FluidListComponent fluidListComponent = stack.get(CastingDataComponents.FLUIDS.get());

        if (fluidListComponent != null) {

            boolean hasFluids = fluidListComponent.fluids().stream().anyMatch(f -> !f.isEmpty());

            if (hasFluids) {

                if (Minecraft.getInstance().hasShiftDown()) {
                    event.getToolTip().add(Component.translatable("tooltip.casting.fluids_header").withStyle(ChatFormatting.BLUE));

                    for (FluidStack fluid : fluidListComponent.fluids()) {
                        if (fluid.isEmpty()) continue;

                        event.getToolTip().add(
                                Component.literal(" - ")
                                        .append(Component.literal(fluid.getAmount() + "mB "))
                                        .append(fluid.getHoverName())
                                        .withStyle(ChatFormatting.BLUE)
                        );
                    }

                } else {
                    event.getToolTip().add(Component.translatable("tooltip.bblcore.shift") .withStyle(ChatFormatting.YELLOW));
                }
            }
        }

        List<ItemStack> molds = stack.get(CastingDataComponents.STORED_MOLDS.get());

        if (molds != null) {

            boolean hasMolds = molds.stream().anyMatch(m -> !m.isEmpty());

            if (hasMolds) {

                if (Minecraft.getInstance().hasShiftDown()) {

                    event.getToolTip().add(Component.translatable("tooltip.casting.molds_header").withStyle(ChatFormatting.GOLD));

                    for (ItemStack mold : molds) {
                        if (mold.isEmpty()) continue;

                        event.getToolTip().add(
                                Component.literal(" - ")
                                        .append(mold.getHoverName())
                                        .withStyle(ChatFormatting.GOLD)
                        );
                    }

                } else {
                    event.getToolTip().add(
                            Component.translatable("tooltip.bblcore.shift")
                                    .withStyle(ChatFormatting.YELLOW)
                    );
                }
            }
        }
    }
}
