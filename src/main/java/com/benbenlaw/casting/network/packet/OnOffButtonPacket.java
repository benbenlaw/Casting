package com.benbenlaw.casting.network.packet;

import com.benbenlaw.casting.block.CastingBlocks;
import com.benbenlaw.casting.block.multiblock.MultiblockControllerBlock;
import com.benbenlaw.casting.block.multiblock.MultiblockMixerBlock;
import com.benbenlaw.casting.block.multiblock.MultiblockSolidifierBlock;
import com.benbenlaw.casting.block.multiblock.MultiblockValveBlock;
import com.benbenlaw.casting.network.payload.OnOffButtonPayload;
import net.minecraft.core.BlockPos;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.state.BlockState;
import net.neoforged.neoforge.network.handling.IPayloadContext;

//Todo; move the on off packet into bbl core and come up with an interface to handle the on off button across multiple mods
//  , cloche, utilities and casting all use a similar on off button packet so this needs to be unified across the mods
public record OnOffButtonPacket() {

    public static final OnOffButtonPacket INSTANCE = new OnOffButtonPacket();

    public static OnOffButtonPacket get() {
        return INSTANCE;
    }

    public void handle(final OnOffButtonPayload payload, IPayloadContext context) {

        Player player = context.player();
        Level level = player.level();
        BlockPos blockPos = payload.blockPos();
        BlockState blockState = level.getBlockState(blockPos);

        //Controller On Off Button
        if (blockState.getBlock() instanceof MultiblockControllerBlock) {

            if (blockState.getValue(MultiblockControllerBlock.ENABLED)) {
                level.setBlockAndUpdate(blockPos, CastingBlocks.MULTIBLOCK_CONTROLLER.get().defaultBlockState().setValue(MultiblockControllerBlock.ENABLED, false)
                        .setValue(MultiblockControllerBlock.FACING, blockState.getValue(MultiblockControllerBlock.FACING))
                        .setValue(MultiblockControllerBlock.WORKING, blockState.getValue(MultiblockControllerBlock.WORKING)));
            } else {
                level.setBlockAndUpdate(blockPos, CastingBlocks.MULTIBLOCK_CONTROLLER.get().defaultBlockState().setValue(MultiblockControllerBlock.ENABLED, true)
                        .setValue(MultiblockControllerBlock.FACING, blockState.getValue(MultiblockControllerBlock.FACING))
                        .setValue(MultiblockControllerBlock.WORKING, blockState.getValue(MultiblockControllerBlock.WORKING)));
            }
        }

        //Solidifier On Off Button
        if (blockState.getBlock() instanceof MultiblockSolidifierBlock) {
            if (blockState.getValue(MultiblockSolidifierBlock.ENABLED)) {
                level.setBlockAndUpdate(blockPos, CastingBlocks.MULTIBLOCK_SOLIDIFIER.get().defaultBlockState().setValue(MultiblockSolidifierBlock.ENABLED, false)
                        .setValue(MultiblockSolidifierBlock.FACING, blockState.getValue(MultiblockSolidifierBlock.FACING))
                        .setValue(MultiblockSolidifierBlock.WORKING, blockState.getValue(MultiblockSolidifierBlock.WORKING)));


            } else {
                level.setBlockAndUpdate(blockPos, CastingBlocks.MULTIBLOCK_SOLIDIFIER.get().defaultBlockState().setValue(MultiblockSolidifierBlock.ENABLED, true)
                        .setValue(MultiblockSolidifierBlock.FACING, blockState.getValue(MultiblockSolidifierBlock.FACING))
                        .setValue(MultiblockSolidifierBlock.WORKING, blockState.getValue(MultiblockSolidifierBlock.WORKING)));

            }
        }

        //Valve On Off Button
        if (blockState.getBlock() instanceof MultiblockValveBlock) {
            if (blockState.getValue(MultiblockValveBlock.ENABLED)) {
                level.setBlockAndUpdate(blockPos, CastingBlocks.MULTIBLOCK_VALVE.get().defaultBlockState().setValue(MultiblockValveBlock.ENABLED, false)
                        .setValue(MultiblockValveBlock.FACING, blockState.getValue(MultiblockValveBlock.FACING))
                        .setValue(MultiblockValveBlock.WORKING, blockState.getValue(MultiblockValveBlock.WORKING)));


            } else {
                level.setBlockAndUpdate(blockPos, CastingBlocks.MULTIBLOCK_VALVE.get().defaultBlockState().setValue(MultiblockValveBlock.ENABLED, true)
                        .setValue(MultiblockValveBlock.FACING, blockState.getValue(MultiblockValveBlock.FACING))
                        .setValue(MultiblockValveBlock.WORKING, blockState.getValue(MultiblockValveBlock.WORKING)));

            }
        }

        //Mixer On Off Button
        if (blockState.getBlock() instanceof MultiblockMixerBlock) {
            if (blockState.getValue(MultiblockMixerBlock.ENABLED)) {
                level.setBlockAndUpdate(blockPos, CastingBlocks.MULTIBLOCK_MIXER.get().defaultBlockState().setValue(MultiblockMixerBlock.ENABLED, false)
                        .setValue(MultiblockMixerBlock.FACING, blockState.getValue(MultiblockMixerBlock.FACING))
                        .setValue(MultiblockMixerBlock.WORKING, blockState.getValue(MultiblockMixerBlock.WORKING)));
            } else {
                level.setBlockAndUpdate(blockPos, CastingBlocks.MULTIBLOCK_MIXER.get().defaultBlockState().setValue(MultiblockMixerBlock.ENABLED, true)
                        .setValue(MultiblockMixerBlock.FACING, blockState.getValue(MultiblockMixerBlock.FACING))
                        .setValue(MultiblockMixerBlock.WORKING, blockState.getValue(MultiblockMixerBlock.WORKING)));
            }
        }
    }
}