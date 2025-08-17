package com.benbenlaw.casting.screen.util;

import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.state.BlockState;

public record BlockInformation(BlockState state, Level level, long tickPlace) {
}
