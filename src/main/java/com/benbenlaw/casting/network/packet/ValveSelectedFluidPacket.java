package com.benbenlaw.casting.network.packet;

import com.benbenlaw.casting.block.entity.multiblock.MultiblockValveBlockEntity;
import com.benbenlaw.casting.network.payload.ValveSelectedFluidPayload;
import net.minecraft.core.BlockPos;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.entity.BlockEntity;
import net.neoforged.neoforge.network.handling.IPayloadContext;

public record ValveSelectedFluidPacket() {

    public static final ValveSelectedFluidPacket INSTANCE = new ValveSelectedFluidPacket();

    public static ValveSelectedFluidPacket get() {
        return INSTANCE;
    }

    public void handle(final ValveSelectedFluidPayload payload, IPayloadContext context) {

        Player player = context.player();
        Level level = player.level();
        BlockPos blockPos = payload.pos();
        BlockEntity blockEntity = level.getBlockEntity(blockPos);

        if (blockEntity instanceof MultiblockValveBlockEntity valveBlockEntity) {
            valveBlockEntity.setSelectedFluid(payload.fluid());
        }
    }
}
