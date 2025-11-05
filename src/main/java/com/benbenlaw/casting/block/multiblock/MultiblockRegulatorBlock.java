package com.benbenlaw.casting.block.multiblock;

import com.benbenlaw.core.block.SyncableBlock;
import com.mojang.serialization.MapCodec;
import net.minecraft.world.level.block.BaseEntityBlock;
import net.minecraft.world.level.block.Block;
import org.jetbrains.annotations.NotNull;

public class MultiblockRegulatorBlock extends SyncableBlock {

    public static final MapCodec<MultiblockRegulatorBlock> CODEC = simpleCodec(MultiblockRegulatorBlock::new);

    public @NotNull MapCodec<MultiblockRegulatorBlock> codec() {
        return CODEC;
    }

    public MultiblockRegulatorBlock(Properties properties) {
        super(properties);
    }


}
