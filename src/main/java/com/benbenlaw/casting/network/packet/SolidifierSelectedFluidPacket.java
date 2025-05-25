package com.benbenlaw.casting.network.packet;

import com.benbenlaw.casting.block.entity.multiblock.MultiblockSolidifierBlockEntity;
import com.benbenlaw.casting.network.payload.SolidifierSelectedFluidPayload;
import net.minecraft.core.BlockPos;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.entity.BlockEntity;
import net.neoforged.neoforge.network.handling.IPayloadContext;

public record SolidifierSelectedFluidPacket() {

    public static final SolidifierSelectedFluidPacket INSTANCE = new SolidifierSelectedFluidPacket();

    public static SolidifierSelectedFluidPacket get() {
        return INSTANCE;
    }

    public void handle(final SolidifierSelectedFluidPayload payload, IPayloadContext context) {

        Player player = context.player();
        Level level = player.level();
        BlockPos blockPos = payload.pos();
        BlockEntity blockEntity = level.getBlockEntity(blockPos);

        if (blockEntity instanceof MultiblockSolidifierBlockEntity solidifierBlockEntity) {
            solidifierBlockEntity.setSelectedFluid(payload.fluid());
        }
    }
}
