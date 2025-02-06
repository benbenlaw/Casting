package com.benbenlaw.casting.networking.packets;

import com.benbenlaw.casting.block.entity.ControllerBlockEntity;
import com.benbenlaw.casting.block.entity.MixerBlockEntity;
import com.benbenlaw.casting.block.entity.SolidifierBlockEntity;
import com.benbenlaw.casting.networking.payload.ClearTankPayload;
import com.benbenlaw.casting.networking.payload.LockSolidifierPayload;
import net.minecraft.core.BlockPos;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.entity.BlockEntity;
import net.minecraft.world.level.block.state.BlockState;
import net.neoforged.neoforge.network.handling.IPayloadContext;

public record LockSolidifierPacket() {

    public static final LockSolidifierPacket INSTANCE = new LockSolidifierPacket();

    public static LockSolidifierPacket get() {
        return INSTANCE;
    }

    public void handle(final LockSolidifierPayload payload, IPayloadContext context) {

        Player player = context.player();
        Level level = player.level();
        BlockPos blockPos = payload.blockPos();
        BlockEntity blockEntity = level.getBlockEntity(blockPos);

        if (blockEntity instanceof SolidifierBlockEntity solidifierBlockEntity) {
            solidifierBlockEntity.toggleLimitMode();
            solidifierBlockEntity.sync();
        }

        //ADD CODE HERE
    }
}
