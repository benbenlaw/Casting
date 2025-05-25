package com.benbenlaw.casting.network.packet;

import com.benbenlaw.casting.block.entity.SolidifierBlockEntity;
import com.benbenlaw.casting.network.payload.LockSolidifierPayload;
import net.minecraft.core.BlockPos;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.entity.BlockEntity;
import net.neoforged.neoforge.network.handling.IPayloadContext;
@Deprecated(since = "2.0.0")
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
