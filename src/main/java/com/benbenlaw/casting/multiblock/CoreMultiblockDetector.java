package com.benbenlaw.casting.multiblock;

import com.mojang.datafixers.util.Pair;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.block.state.properties.BlockStateProperties;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Predicate;
//TODO Move to core

//Inspired from productive lib multiblock detector
//https://github.com/JDKDigital/productivelib/blob/dev-1.21.0/src/main/java/cy/jdkdigital/productivelib/util/MultiBlockDetector.java

public class CoreMultiblockDetector {

    public static MultiblockData findMultiblock(Level level, BlockPos controllerPos, Block controllerBlock, Predicate<BlockState> wallBlocks,
                                                Predicate<BlockState> floorBlocks, Predicate<BlockState> validExtraBlocks, boolean hollow,
                                                boolean optionalCorners, int maxVolume, int radius, int maxHeight) {

        BlockPos.MutableBlockPos top = controllerPos.mutable().move(Direction.UP);
        while (top.getY() < level.getMaxBuildHeight() && wallBlocks.test(level.getBlockState(top))) {
            top.move(Direction.UP);
        }
        top.move(Direction.DOWN);
        BlockState controllerState = level.getBlockState(controllerPos);
        if (controllerState.isAir()) {
            return null;
        }

        Direction controllerDirection = controllerState.getValue(BlockStateProperties.HORIZONTAL_FACING);
        List<BlockPos> extraValidBlocks = new ArrayList<>();
        Pair<BlockPos, BlockPos> topCorners = findEdges(
                level, controllerPos, controllerBlock, controllerDirection.getClockWise(), top.immutable(),
                wallBlocks, validExtraBlocks, optionalCorners, radius, extraValidBlocks);

        if (topCorners == null) {
            return null;
        }

        if (floorBlocks == null) {
            return null;
        }

        BlockPos.MutableBlockPos bottomCorner;
        bottomCorner = topCorners.getFirst()
                .relative(controllerDirection.getOpposite())
                .relative(controllerDirection.getCounterClockWise())
                .mutable();

        int height = 0;
        while (height++ < maxHeight &&
                !getExtraBlocks(floorBlocks, wallBlocks).test(level.getBlockState(bottomCorner.move(Direction.DOWN)))) {
        }

        if (!floorBlocks.test(level.getBlockState(bottomCorner))) {
            return null;
        }

        List<BlockPos> notFloorBlocks = BlockPos.betweenClosedStream(
                        topCorners.getFirst().relative(controllerDirection.getOpposite()).relative(controllerDirection.getCounterClockWise()).below(height),
                        topCorners.getSecond().relative(controllerDirection).relative(controllerDirection.getClockWise()).below(height))
                .filter(pos -> !floorBlocks.test(level.getBlockState(pos)))
                .toList();

        if (!notFloorBlocks.isEmpty()) {
            return null;
        }

        for (int slice = 1; slice <= height; ++slice) {
            if (!optionalCorners || slice != height) {
                Pair<BlockPos, BlockPos> corners = findEdges(
                        level, controllerPos, controllerBlock, controllerDirection.getClockWise(),
                        top.immutable().below(slice), wallBlocks, validExtraBlocks, optionalCorners, radius, extraValidBlocks);

                if (corners == null) {
                    return null;
                }
            }
        }

        int volume = (int) BlockPos.betweenClosedStream(
                        topCorners.getFirst().relative(controllerDirection.getOpposite()).relative(controllerDirection.getCounterClockWise()),
                        topCorners.getSecond().relative(controllerDirection).relative(controllerDirection.getClockWise()).below(height - 1))
                .count();

        if (volume > maxVolume) {
            return null;
        }

        if (hollow) {
            List<BlockPos> notAirBlocks = BlockPos.betweenClosedStream(
                            topCorners.getFirst().relative(controllerDirection.getOpposite()).relative(controllerDirection.getCounterClockWise()),
                            topCorners.getSecond().relative(controllerDirection).relative(controllerDirection.getClockWise()).below(height - 1))
                    .filter(pos -> !level.getBlockState(pos).isAir())
                    .toList();

            if (!notAirBlocks.isEmpty()) {
                return null;
            }
        }

        return new MultiblockData(controllerPos, topCorners, extraValidBlocks, height, volume);
    }


    private static Predicate<BlockState> getExtraBlocks(Predicate<BlockState> floorBlocks, Predicate<BlockState> wallBlocks) {
        return floorBlocks != null ? floorBlocks : wallBlocks;
    }

    private static Pair<BlockPos, BlockPos> findEdges(Level level, BlockPos controllerPos, Block controllerBlock, Direction direction, BlockPos startPos,
                                                      Predicate<BlockState> validBlocks, Predicate<BlockState> validExtraBlocks,boolean optionalCorners,
                                                      int radius, List<BlockPos> extraValidBlocks) {

        BlockPos firstCorner = new BlockPos(startPos);
        BlockPos secondCorner = new BlockPos(startPos);
        BlockPos.MutableBlockPos pointer = new BlockPos.MutableBlockPos(startPos.getX(), startPos.getY(), startPos.getZ());

        int turns = 0, maxSize = radius;

        while (turns <= 4 &&
                maxSize-- > 0 &&
                validBlocks.test(level.getBlockState(pointer)) &&
                (turns == 0 || !pointer.equals(startPos))) {

            BlockPos nextPos = pointer.relative(direction);
            BlockState nextBlockState = level.getBlockState(nextPos);

            if (turns < 4 && !validBlocks.test(nextBlockState)) {
                BlockPos cornerCheck = pointer.relative(direction.getClockWise());
                if (optionalCorners && !validBlocks.test(level.getBlockState(cornerCheck))) {
                    pointer.move(direction);
                }

                if (turns == 0) {
                    firstCorner = pointer.immutable();
                }
                if (turns == 2) {
                    secondCorner = pointer.immutable();
                }

                direction = direction.getClockWise();
                turns++;
            }

            pointer.move(direction);
            BlockState targetState = level.getBlockState(pointer);
            if (validExtraBlocks.test(targetState)) {
                extraValidBlocks.add(pointer.immutable());
            }

            if (targetState.is(controllerBlock) && !pointer.equals(controllerPos)) {
                return null;
            }
        }

        if (turns == 4 && pointer.immutable().equals(startPos)) {
            return Pair.of(firstCorner, secondCorner);
        }

        return null;
    }




}
