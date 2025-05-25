package com.benbenlaw.casting.util;

import net.minecraft.core.HolderLookup;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.nbt.Tag;
import net.neoforged.neoforge.fluids.FluidStack;
import net.neoforged.neoforge.fluids.capability.IFluidHandler;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

//TODO Move to core
/**
 * A fluid tank that can hold multiple types of fluids with a shared capacity.
 * This tank allows for the merging of fluids and has a maximum capacity that can be changed.
 */
public class MultiFluidTankSharedCapacity implements IFluidHandler {
    public final List<FluidStack> fluids = new ArrayList<>();
    private final int maxCapacity;
    private int enabledCapacity;

    public MultiFluidTankSharedCapacity(int maxCapacity) {
        this.maxCapacity = maxCapacity;
        this.enabledCapacity = maxCapacity;
    }

    public void setEnabledCapacity(int newCapacity) {
        this.enabledCapacity = Math.min(newCapacity, maxCapacity);
        int total = getTotalFluidAmount();
        if (total > enabledCapacity) {
            reduceToCapacity();
        }
    }

    public int getEnabledCapacity() {
        return enabledCapacity;
    }

    public int getTotalFluidAmount() {
        return fluids.stream().mapToInt(FluidStack::getAmount).sum();
    }

    public int getRemainingSpace() {
        return enabledCapacity - getTotalFluidAmount();
    }

    @Override
    public int getTanks() {
        return fluids.size();
    }

    @Override
    public FluidStack getFluidInTank(int tank) {
        return tank >= 0 && tank < fluids.size() ? fluids.get(tank) : FluidStack.EMPTY;
    }

    @Override
    public int getTankCapacity(int tank) {
        return enabledCapacity;
    }

    @Override
    public boolean isFluidValid(int tank, @NotNull FluidStack stack) {
        return true;
    }

    @Override
    public int fill(FluidStack resource, @NotNull FluidAction action) {
        if (resource.isEmpty() || resource.getAmount() <= 0) return 0;

        int spaceLeft = getRemainingSpace();
        if (spaceLeft <= 0) return 0;

        for (FluidStack stored : fluids) {
            if (FluidStack.isSameFluidSameComponents(resource, stored)) {
                int fillAmount = Math.min(spaceLeft, resource.getAmount());
                if (fillAmount > 0 && action.execute()) {
                    stored.grow(fillAmount);
                    onContentsChanged();
                }
                return fillAmount;
            }
        }

        int fillAmount = Math.min(spaceLeft, resource.getAmount());
        if (fillAmount > 0 && action.execute()) {
            FluidStack toAdd = resource.copy();
            fluids.add(toAdd);
            onContentsChanged();
        }

        return fillAmount;
    }

    @Override
    public @NotNull FluidStack drain(FluidStack resource, @NotNull FluidAction action) {
        if (resource.isEmpty()) return FluidStack.EMPTY;

        for (int i = 0; i < fluids.size(); i++) {
            FluidStack stored = fluids.get(i);
            if (FluidStack.isSameFluidSameComponents(resource, stored)) {
                int drainAmount = Math.min(stored.getAmount(), resource.getAmount());
                FluidStack drained = new FluidStack(stored.getFluid(), drainAmount);

                if (action.execute()) {
                    stored.shrink(drainAmount);
                    if (stored.getAmount() <= 0) fluids.remove(i);
                    onContentsChanged();
                }

                return drained;
            }
        }

        return FluidStack.EMPTY;
    }

    @Override
    public @NotNull FluidStack drain(int maxDrain, @NotNull FluidAction action) {
        if (fluids.isEmpty()) return FluidStack.EMPTY;

        FluidStack stored = fluids.getFirst();
        int drainAmount = Math.min(stored.getAmount(), maxDrain);
        FluidStack drained = new FluidStack(stored.getFluid(), drainAmount);

        if (action.execute()) {
            stored.shrink(drainAmount);
            if (stored.getAmount() <= 0) fluids.removeFirst();
            onContentsChanged();
        }

        return drained;
    }

    public List<FluidStack> getFluids() {
        return fluids;
    }

    public void clear() {
        fluids.clear();
    }

    protected void onContentsChanged() {}

    private void reduceToCapacity() {
        int allowed = enabledCapacity;
        Iterator<FluidStack> iterator = fluids.iterator();

        while (iterator.hasNext()) {
            FluidStack stack = iterator.next();
            if (allowed <= 0) {
                iterator.remove();
                continue;
            }

            if (stack.getAmount() > allowed) {
                stack.setAmount(allowed);
            }

            allowed -= stack.getAmount();
        }
    }

    public FluidStack drain(int tankIndex, int maxDrain, FluidAction action) {
        if (tankIndex < 0 || tankIndex >= fluids.size()) return FluidStack.EMPTY;

        FluidStack stored = fluids.get(tankIndex);
        int drainAmount = Math.min(stored.getAmount(), maxDrain);
        FluidStack drained = new FluidStack(stored.getFluid(), drainAmount);

        if (action.execute()) {
            stored.shrink(drainAmount);
            if (stored.getAmount() <= 0) fluids.remove(tankIndex);
            onContentsChanged();
        }

        return drained;
    }

    public void readFromNBT(HolderLookup.Provider provider, Tag nbt) {
        if (nbt instanceof ListTag listTag) {
            this.fluids.clear();
            for (int i = 0; i < listTag.size(); i++) {
                CompoundTag tag = listTag.getCompound(i);
                this.fluids.add(FluidStack.parseOptional(provider, tag));
            }
        }
    }

    public Tag writeToNBT(HolderLookup.Provider provider) {
        ListTag fluidList = new ListTag();
        for (FluidStack fluid : this.fluids) {
            if (!fluid.isEmpty()) {
                fluidList.add(fluid.save(provider));
            }
        }
        return fluidList;
    }
}
