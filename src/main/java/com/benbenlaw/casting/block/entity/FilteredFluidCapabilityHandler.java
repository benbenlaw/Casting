package com.benbenlaw.casting.block.entity;

import com.benbenlaw.core.block.entity.handler.fluid.FilterFluidHandler;
import net.neoforged.neoforge.fluids.FluidStack;
import net.neoforged.neoforge.transfer.ResourceHandler;
import net.neoforged.neoforge.transfer.fluid.FluidResource;
import net.neoforged.neoforge.transfer.transaction.TransactionContext;

public class FilteredFluidCapabilityHandler implements ResourceHandler<FluidResource> {

    private final ResourceHandler<FluidResource> delegate;
    private final FilterFluidHandler filter;
    private final int[] filteredTanks;

    public FilteredFluidCapabilityHandler(ResourceHandler<FluidResource> delegate, FilterFluidHandler filter, int[] filteredTanks) {
        this.delegate = delegate;
        this.filter = filter;
        this.filteredTanks = filteredTanks;
    }

    private boolean isFiltered(int index) {
        for (int tank : filteredTanks) {
            if (tank == index) return true;
        }
        return false;
    }

    @Override
    public int size() {
        return delegate.size();
    }

    @Override
    public FluidResource getResource(int index) {
        return delegate.getResource(index);
    }

    @Override
    public long getAmountAsLong(int index) {
        return delegate.getAmountAsLong(index);
    }

    @Override
    public long getCapacityAsLong(int index, FluidResource resource) {
        if (!resource.isEmpty() && !isAllowed(index, resource)) {
            return 0;
        }
        return delegate.getCapacityAsLong(index, resource);
    }

    @Override
    public boolean isValid(int index, FluidResource resource) {
        if (!resource.isEmpty() && !isAllowed(index, resource)) {
            return false;
        }
        return delegate.isValid(index, resource);
    }

    @Override
    public int insert(int index, FluidResource resource, int amount, TransactionContext transaction) {
        if (!resource.isEmpty() && !isAllowed(index, resource)) {
            return 0;
        }
        return delegate.insert(index, resource, amount, transaction);
    }

    @Override
    public int extract(int index, FluidResource resource, int amount, TransactionContext transaction) {
        return delegate.extract(index, resource, amount, transaction);
    }

    public boolean isAllowed(int index, FluidResource resource) {
        if (!isFiltered(index)) {
            return true;
        }

        FluidStack filterStack = filter.getFilter(index);
        if (filterStack.isEmpty()) {
            return true;
        }

        return filter.matchesFluid(resource, filterStack);

    }
}
