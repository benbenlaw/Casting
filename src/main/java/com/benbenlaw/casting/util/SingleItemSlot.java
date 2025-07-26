package com.benbenlaw.casting.util;

import net.minecraft.world.item.ItemStack;
import net.neoforged.neoforge.items.IItemHandler;
import net.neoforged.neoforge.items.SlotItemHandler;

public class SingleItemSlot extends SlotItemHandler {
    public SingleItemSlot(IItemHandler itemHandler, int index, int xPosition, int yPosition) {
        super(itemHandler, index, xPosition, yPosition);
    }

    @Override
    public int getMaxStackSize() {
        return 1; // Limit to only 1 item
    }

    @Override
    public int getMaxStackSize(ItemStack stack) {
        return 1;
    }
}