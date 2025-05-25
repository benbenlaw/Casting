package com.benbenlaw.casting.util;

import net.neoforged.neoforge.fluids.FluidStack;
import net.neoforged.neoforge.fluids.capability.IFluidHandler;
import net.neoforged.neoforge.fluids.capability.templates.FluidTank;

public class SingleFluidTank extends FluidTank implements IFluidHandler {
    protected FluidStack fluid = FluidStack.EMPTY;
    protected int capacity;
    public SingleFluidTank(int capacity) {
        super(capacity);
        this.capacity = capacity;
    }
}
