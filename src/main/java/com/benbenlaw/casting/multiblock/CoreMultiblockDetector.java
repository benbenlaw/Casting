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

public class CoreMultiblockDetector {

    public static MultiblockData findMultiblock(Level level, BlockPos controllerPos, Block controllerBlock, Predicate<BlockState> wallBlocks,
                                                Predicate<BlockState> floorBlocks, Predicate<BlockState> validExtraBlocks, boolean hollow,
                                                boolean optionalCorners, int maxVolume, int radius, int maxHeight) {
        // 1. Find the top wall block above the controller
        BlockPos.MutableBlockPos top = controllerPos.mutable().move(Direction.UP);
        while (top.getY() < level.getMaxBuildHeight() && wallBlocks.test(level.getBlockState(top))) {
            top.move(Direction.UP);
        }
        top.move(Direction.DOWN); // Step back to last valid wall block

        // 2. Validate controller state
        BlockState controllerState = level.getBlockState(controllerPos);
        if (controllerState.isAir()) {
            System.out.println("Controller block is not valid");
            return null;
        }

        Direction controllerFacing = controllerState.getValue(BlockStateProperties.HORIZONTAL_FACING);
        List<BlockPos> extraValidBlocks = new ArrayList<>();

        // 3. Find top corners
        Pair<BlockPos, BlockPos> topCorners = findEdges(
                level, controllerPos, controllerBlock, controllerFacing.getClockWise(), top.immutable(),
                wallBlocks, validExtraBlocks, optionalCorners, radius, extraValidBlocks);

        if (topCorners == null) {
            System.out.println("topCorners is null — aborting multiblock detection.");
            return null;
        }

        if (floorBlocks == null) {
            System.out.println("topCorners is null — aborting multiblock detection.");
            return null;
        }

        // 4. Determine the bottom-most position of the structure
        BlockPos.MutableBlockPos bottomCorner;
        bottomCorner = topCorners.getFirst()
                .relative(controllerFacing.getOpposite())
                .relative(controllerFacing.getCounterClockWise())
                .mutable();

        // 5. Determine height
        int height = 0;
        while (height++ < maxHeight &&
                !getPredicateOrWall(floorBlocks, wallBlocks).test(level.getBlockState(bottomCorner.move(Direction.DOWN)))) {
        }

        // 6. Validate bottom layer
        if (!floorBlocks.test(level.getBlockState(bottomCorner))) {
            System.out.println("Invalid bottom starting block at: " + bottomCorner);
            return null;
        }

        List<BlockPos> notFloorBlocks = BlockPos.betweenClosedStream(
                        topCorners.getFirst().relative(controllerFacing.getOpposite()).relative(controllerFacing.getCounterClockWise()).below(height),
                        topCorners.getSecond().relative(controllerFacing).relative(controllerFacing.getClockWise()).below(height))
                .filter(pos -> !floorBlocks.test(level.getBlockState(pos)))
                .toList();

        if (!notFloorBlocks.isEmpty()) {
            System.out.println("Invalid floor blocks found at: " + notFloorBlocks.getFirst());
            return null;
        }

        // 7. Validate each wall slice
        for (int slice = 1; slice <= height; ++slice) {
            if (!optionalCorners || slice != height) {
                Pair<BlockPos, BlockPos> corners = findEdges(
                        level, controllerPos, controllerBlock, controllerFacing.getClockWise(),
                        top.immutable().below(slice), wallBlocks, validExtraBlocks, optionalCorners, radius, extraValidBlocks);

                if (corners == null) {
                    System.out.println("Wall slice at level " + slice + " is invalid.");
                    return null;
                }
            }
        }

        // 8. Compute internal volume
        int volume = (int) BlockPos.betweenClosedStream(
                        topCorners.getFirst().relative(controllerFacing.getOpposite()).relative(controllerFacing.getCounterClockWise()),
                        topCorners.getSecond().relative(controllerFacing).relative(controllerFacing.getClockWise()).below(height - 1))
                .count();

        if (volume > maxVolume) {
            System.out.println("Structure volume exceeds limit: " + volume + " > " + maxVolume);
            return null;
        }

        // 9. Ensure hollow interior (if required)
        if (hollow) {
            List<BlockPos> notAirBlocks = BlockPos.betweenClosedStream(
                            topCorners.getFirst().relative(controllerFacing.getOpposite()).relative(controllerFacing.getCounterClockWise()),
                            topCorners.getSecond().relative(controllerFacing).relative(controllerFacing.getClockWise()).below(height - 1))
                    .filter(pos -> !level.getBlockState(pos).isAir())
                    .toList();

            if (!notAirBlocks.isEmpty()) {
                System.out.println("Structure is not hollow. Block found at: " + notAirBlocks.getFirst());
                return null;
            }
        }

        // 10. All good
        return new MultiblockData(controllerPos, topCorners, extraValidBlocks, height, volume);
    }


    private static Predicate<BlockState> getPredicateOrWall(Predicate<BlockState> floorBlocks, Predicate<BlockState> wallBlocks) {
        return floorBlocks != null ? floorBlocks : wallBlocks;
    }

    private static Pair<BlockPos, BlockPos> findEdges(Level level,
                                                      BlockPos controllerPos,
                                                      Block controllerBlock,
                                                      Direction dir,
                                                      BlockPos initialPosition,
                                                      Predicate<BlockState> validBlocks,
                                                      Predicate<BlockState> validExtraBlocks,
                                                      boolean optionalCorners,
                                                      int radius,
                                                      List<BlockPos> extraValidBlocks) {

        BlockPos firstCorner = new BlockPos(initialPosition);
        BlockPos secondCorner = new BlockPos(initialPosition);
        BlockPos.MutableBlockPos pointer = new BlockPos.MutableBlockPos(initialPosition.getX(), initialPosition.getY(), initialPosition.getZ());

        int turns = 0, maxSize = radius;

        while (turns <= 4 &&
                maxSize-- > 0 &&
                validBlocks.test(level.getBlockState(pointer)) &&
                (turns == 0 || !pointer.equals(initialPosition))) {

            // Check next block in current direction
            BlockPos nextPos = pointer.relative(dir);
            BlockState nextBlockState = level.getBlockState(nextPos);

            if (turns < 4 && !validBlocks.test(nextBlockState)) {
                // Try to handle optional (empty) corners
                BlockPos cornerCheck = pointer.relative(dir.getClockWise());
                if (optionalCorners && !validBlocks.test(level.getBlockState(cornerCheck))) {
                    pointer.move(dir); // move past empty corner
                }

                if (turns == 0) {
                    firstCorner = pointer.immutable();
                }
                if (turns == 2) {
                    secondCorner = pointer.immutable();
                }

                dir = dir.getClockWise();
                turns++;
            }

            pointer.move(dir);

            BlockState targetState = level.getBlockState(pointer);

            // Register multiblock Extra Valid Blocks
            if (validExtraBlocks.test(targetState)) {
                extraValidBlocks.add(pointer.immutable());
            }

            // Prevent multiple controllers in the structure
            if (targetState.is(controllerBlock) && !pointer.equals(controllerPos)) {
                System.out.println("Multiple controllers are not allowed");
                return null;
            }
        }

        if (turns == 4 && pointer.immutable().equals(initialPosition)) {
            return Pair.of(firstCorner, secondCorner);
        }


        System.out.println("Structure did not form a valid closed loop");
        return null;
    }




}
