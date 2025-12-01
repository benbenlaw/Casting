package com.benbenlaw.casting.item;

import com.benbenlaw.casting.item.util.FluidListComponent;
import net.minecraft.ChatFormatting;
import net.minecraft.client.gui.screens.Screen;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.TooltipFlag;
import net.neoforged.neoforge.fluids.FluidStack;
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
            FluidListComponent component = itemStack.get(CastingDataComponents.FLUIDS);
            if (component != null) {
                List<FluidStack> fluids = component.fluids();
                if (!fluids.isEmpty()) {
                    FluidType fluid = fluids.get(0).getFluidType();
                    int fluidAmount = fluids.get(0).getAmount();
                    components.add(Component.literal("Fluids: ").withStyle(ChatFormatting.BLUE));
                    components.add(Component.literal("- ").append(fluidAmount + "mb ").append(Component.translatable(fluid.getDescriptionId())).withStyle(ChatFormatting.GREEN));
                }
            }
        } else {
            components.add(Component.translatable("tooltips.bblcore.shift").withStyle(ChatFormatting.YELLOW));
        }

        super.appendHoverText(itemStack, context, components, flag);
    }

}
