package com.benbenlaw.casting.item;

import net.minecraft.ChatFormatting;
import net.minecraft.client.gui.screens.Screen;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.TooltipFlag;
import net.neoforged.neoforge.fluids.FluidType;
import org.jetbrains.annotations.NotNull;

import java.util.List;

public class FluidMoverItem extends Item {
    public FluidMoverItem(Properties properties) {
        super(properties);
    }


    @Override
    public void appendHoverText(ItemStack itemStack, @NotNull TooltipContext context, @NotNull List<Component> components, @NotNull TooltipFlag flag) {

        if (Screen.hasShiftDown()) {
            if (itemStack.has(CastingDataComponents.FLUID_TYPE)) {
                String fluidAsString = itemStack.get(CastingDataComponents.FLUID_TYPE);
                int fluidAmount = itemStack.get(CastingDataComponents.FLUID_AMOUNT);
                assert fluidAsString != null;
                FluidType fluid = BuiltInRegistries.FLUID.get(ResourceLocation.tryParse(fluidAsString)).getFluidType();
                components.add(Component.literal("Contains: ").append(fluidAmount + "mb ").append(Component.translatable(fluid.getDescriptionId())).withStyle(ChatFormatting.GREEN));
            }
        } else {
            components.add(Component.translatable("tooltips.blocks.with_fluid.shift").withStyle(ChatFormatting.BLUE));
        }

        super.appendHoverText(itemStack, context, components, flag);

    }
}
