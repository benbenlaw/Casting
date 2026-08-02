package com.benbenlaw.casting.block.entity;

import com.benbenlaw.core.block.entity.handler.fluid.FilterFluidHandler;
import com.benbenlaw.core.block.entity.handler.fluid.SyncableFluidHandler;

import javax.annotation.Nullable;

public interface FluidAccepting {

    SyncableFluidHandler receivingHandler();

    int[] acceptingTanks();

    default @Nullable FilterFluidHandler getFilter() {
        return null;
    }

}