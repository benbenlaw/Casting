package com.benbenlaw.casting.network.packet;

import com.benbenlaw.casting.block.entity.multiblock.MultiblockMixerBlockEntity;
import com.benbenlaw.casting.network.payload.MixerSelectedFluidPayload;
import net.minecraft.core.BlockPos;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.entity.BlockEntity;
import net.neoforged.neoforge.network.handling.IPayloadContext;

public record MixerSelectedFluidPacket() {

    public static final MixerSelectedFluidPacket INSTANCE = new MixerSelectedFluidPacket();

    public static MixerSelectedFluidPacket get() {
        return INSTANCE;
    }

    public void handle(final MixerSelectedFluidPayload payload, IPayloadContext context) {

        Player player = context.player();
        Level level = player.level();
        BlockPos blockPos = payload.pos();
        BlockEntity blockEntity = level.getBlockEntity(blockPos);

        if (blockEntity instanceof MultiblockMixerBlockEntity mixerBlockEntity) {
            mixerBlockEntity.setSelectedFluid(payload.fluid());
        }
    }
}
