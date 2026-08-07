package com.benbenlaw.casting.block.entity;

import com.benbenlaw.core.block.entity.handler.fluid.SyncableFluidHandler;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.entity.BlockEntity;
import net.neoforged.neoforge.fluids.FluidStack;
import net.neoforged.neoforge.transfer.fluid.FluidResource;
import net.neoforged.neoforge.transfer.fluid.FluidUtil;
import net.neoforged.neoforge.transfer.transaction.Transaction;

public interface FluidSending {

    SyncableFluidHandler fluidHandler();

    int[] sendingTanks();

    default void tickResourceSending(Level level, BlockPos pos) {
        if (level == null || level.isClientSide()) return;

        SyncableFluidHandler myHandler = fluidHandler();

        for (Direction direction : Direction.values()) {
            BlockPos neighborPos = pos.relative(direction);
            BlockEntity neighbourBlockEntity = level.getBlockEntity(neighborPos);

            if (neighbourBlockEntity instanceof FluidAccepting accepting) {
                var neighborInput = accepting.receivingHandler();
                var neighborFilter = accepting.getFilter();
                int[] acceptingTanks = accepting.acceptingTanks();

                if (neighborInput == null) continue;

                for (int tank : sendingTanks()) {

                    while (true) {
                        FluidStack myStack = FluidUtil.getStack(myHandler, tank);
                        if (myStack.isEmpty()) break;

                        long totalMoved = 0;

                        try (Transaction tx = Transaction.open(null)) {
                            for (int j : acceptingTanks) {

                                if (neighborFilter != null) {
                                    FluidStack filterStack = FluidUtil.getStack(neighborFilter, j);
                                    if (!filterStack.isEmpty()
                                            && !FluidStack.isSameFluidSameComponents(myStack, filterStack)) {
                                        continue;
                                    }
                                }

                                int inserted = neighborInput.insert(j, FluidResource.of(myStack), myStack.getAmount(), tx);
                                if (inserted <= 0) continue;

                                int extracted = myHandler.extract(tank, FluidResource.of(myStack), inserted, tx);

                                if (extracted > 0) {
                                    totalMoved = extracted;
                                    break;
                                }
                            }

                            if (totalMoved > 0) {
                                tx.commit();
                            } else {
                                break;
                            }
                        }
                    }
                }
            }
        }
    }
}