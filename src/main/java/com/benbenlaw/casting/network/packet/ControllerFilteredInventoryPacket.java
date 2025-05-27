package com.benbenlaw.casting.network.packet;

import com.benbenlaw.casting.block.entity.multiblock.MultiblockControllerBlockEntity;
import com.benbenlaw.casting.block.entity.multiblock.MultiblockMixerBlockEntity;
import com.benbenlaw.casting.network.payload.ControllerFilteredInventoryPayload;
import com.benbenlaw.casting.network.payload.MixerSelectedFluidPayload;
import net.minecraft.core.BlockPos;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.entity.BlockEntity;
import net.neoforged.neoforge.network.handling.IPayloadContext;

public class ControllerFilteredInventoryPacket {

    public static final ControllerFilteredInventoryPacket INSTANCE = new ControllerFilteredInventoryPacket();

    public static ControllerFilteredInventoryPacket get() {
        return INSTANCE;
    }

    public void handle(final ControllerFilteredInventoryPayload payload, IPayloadContext context) {
        Player player = context.player();
        Level level = player.level();
        int slot = payload.slot();
        BlockPos pos = payload.controllerPos();
        BlockEntity blockEntity = level.getBlockEntity(pos);

        if (blockEntity instanceof MultiblockControllerBlockEntity controllerBlockEntity) {
            ItemStack stack = payload.item();
            if (stack.isEmpty()) {
                controllerBlockEntity.setAllowedItems(slot, null);
            } else {
                controllerBlockEntity.setAllowedItems(slot, stack.getItem());
            }
        }
    }
}