package com.benbenlaw.casting.util;

import com.benbenlaw.casting.block.entity.multiblock.MultiblockControllerBlockEntity;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.neoforged.neoforge.items.IItemHandler;
import net.neoforged.neoforge.items.SlotItemHandler;
import org.jetbrains.annotations.NotNull;

public class ConditionalSlotItemHandler extends SlotItemHandler {

    private final MultiblockControllerBlockEntity blockEntity;

    public ConditionalSlotItemHandler(MultiblockControllerBlockEntity blockEntity, IItemHandler itemHandler, int index, int xPosition, int yPosition) {
        super(itemHandler, index, xPosition, yPosition);
        this.blockEntity = blockEntity;
    }

    @Override
    public boolean mayPlace(@NotNull ItemStack stack) {
        // Allow placement only if the slot is enabled
        return blockEntity.isSlotEnabled(getSlotIndex());
    }

    @Override
    public boolean mayPickup(@NotNull Player player) {
        // Optional: Prevent extraction if needed
        return blockEntity.isSlotEnabled(getSlotIndex());
    }
}
