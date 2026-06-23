package com.benbenlaw.casting.event.client;

import com.benbenlaw.casting.Casting;
import com.benbenlaw.casting.item.CastingDataComponents;
import com.benbenlaw.casting.item.CastingItems;
import com.benbenlaw.casting.item.FluidMoverItem;
import com.benbenlaw.casting.item.util.FluidListComponent;
import com.benbenlaw.casting.network.packet.ChangeFluidMangerSelectedFluidPacket;
import com.benbenlaw.core.util.TooltipUtil;
import net.minecraft.ChatFormatting;
import net.minecraft.client.Minecraft;
import net.minecraft.network.chat.Component;
import net.minecraft.world.item.ItemStack;
import net.neoforged.bus.api.SubscribeEvent;
import net.neoforged.fml.common.EventBusSubscriber;
import net.neoforged.neoforge.client.event.InputEvent;
import net.neoforged.neoforge.client.network.ClientPacketDistributor;
import net.neoforged.neoforge.event.entity.player.ItemTooltipEvent;
import net.neoforged.neoforge.fluids.FluidStack;

import java.util.List;

@EventBusSubscriber(modid = Casting.MOD_ID)
public class ClientEvents {

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

    @SubscribeEvent
    public static void onMouseScroll(InputEvent.MouseScrollingEvent event) {
        Minecraft mc = Minecraft.getInstance();
        if (mc.player == null || mc.level == null) return;

        ItemStack stack = mc.player.getMainHandItem();
        if (stack.getItem() instanceof FluidMoverItem && mc.player.isShiftKeyDown()) {

            FluidListComponent fluidListComponent = stack.get(CastingDataComponents.FLUIDS.get());
            int storedCount = fluidListComponent != null
                    ? (int) fluidListComponent.fluids().stream().filter(f -> !f.isEmpty()).count()
                    : 0;

            // selectable range: 0..storedCount-1 are real fluids, storedCount itself is "new empty slot"
            // (only offered if there's still room to add one)
            int maxIndex = storedCount < FluidMoverItem.MAX_FLUID_TYPES ? storedCount : storedCount - 1;

            int selected = stack.getOrDefault(CastingDataComponents.FLUID_MANAGER_SELECTED_FLUID.get(), 0);
            selected = Math.min(selected, maxIndex); // clamp in case the list shrank since last selection

            boolean changed = false;
            if (event.getScrollDeltaY() > 0) {
                selected = (selected + 1) % (maxIndex + 1);
                changed = true;
            } else if (event.getScrollDeltaY() < 0) {
                selected = (selected - 1 + (maxIndex + 1)) % (maxIndex + 1);
                changed = true;
            }

            String fluid = "";
            if (fluidListComponent != null && selected < fluidListComponent.fluids().size()) {
                FluidStack selectedStack = fluidListComponent.fluids().get(selected);
                if (!selectedStack.isEmpty()) {
                    fluid = selectedStack.getHoverName().getString();
                }
            }

            if (changed) {
                ClientPacketDistributor.sendToServer(new ChangeFluidMangerSelectedFluidPacket(selected));
                stack.set(CastingDataComponents.FLUID_MANAGER_SELECTED_FLUID.get(), selected);

                mc.gui.setOverlayMessage(
                        fluid.isEmpty()
                                ? Component.translatable("tooltip.casting.empty")
                                : Component.translatable("tooltip.casting.fluid", fluid),
                        false
                );

                event.setCanceled(true);
            }
        }
    }
}
