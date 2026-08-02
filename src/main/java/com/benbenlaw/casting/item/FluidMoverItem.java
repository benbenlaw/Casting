package com.benbenlaw.casting.item;

import com.benbenlaw.casting.item.util.FluidListComponent;
import com.benbenlaw.core.block.entity.handler.fluid.FilterFluidHandler;
import com.benbenlaw.core.block.entity.handler.fluid.SyncableFluidHandler;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.neoforged.neoforge.fluids.FluidStack;
import net.neoforged.neoforge.transfer.fluid.FluidResource;
import net.neoforged.neoforge.transfer.fluid.FluidUtil;
import net.neoforged.neoforge.transfer.transaction.Transaction;

import java.util.ArrayList;
import java.util.List;

public class FluidMoverItem extends Item {

    public static final int CAPACITY = 8000;
    public static final int MAX_FLUID_TYPES = 8;

    public FluidMoverItem(Properties properties) {
        super(properties);
    }

    public static FluidStack getStoredFluid(ItemStack stack, int index) {
        FluidListComponent component = stack.get(CastingDataComponents.FLUIDS.get());
        if (component == null) return FluidStack.EMPTY;
        List<FluidStack> fluids = component.fluids();
        return index < fluids.size() ? fluids.get(index) : FluidStack.EMPTY;
    }

    public static void setStoredFluid(ItemStack stack, int index, FluidStack fluid) {
        FluidListComponent component = stack.get(CastingDataComponents.FLUIDS.get());
        List<FluidStack> fluids = component != null
                ? new ArrayList<>(component.fluids())
                : new ArrayList<>();

        if (fluid.isEmpty()) {
            if (index < fluids.size()) {
                fluids.remove(index);
            }
        } else if (index < fluids.size()) {
            fluids.set(index, fluid);
        } else {
            fluids.add(fluid);
        }

        if (fluids.isEmpty()) {
            stack.remove(CastingDataComponents.FLUIDS.get());
        } else {
            stack.set(CastingDataComponents.FLUIDS.get(), new FluidListComponent(fluids));
        }
    }

    public static boolean onBlockInteract(ItemStack moverStack, SyncableFluidHandler handler,
                                          int[] outputTanks, int[] inputTanks) {
        int selected = moverStack.getOrDefault(CastingDataComponents.FLUID_MANAGER_SELECTED_FLUID.get(), 0);
        FluidStack stored = getStoredFluid(moverStack, selected);

        for (int tank : outputTanks) {
            if (tryCollectFromTank(moverStack, handler, tank, selected, stored)) return true;
        }
        for (int tank : inputTanks) {
            if (tryCollectFromTank(moverStack, handler, tank, selected, stored)) return true;
        }

        if (!stored.isEmpty()) {
            for (int tank : inputTanks) {
                if (tryDeposit(moverStack, handler, tank, selected, stored)) return true;
            }
        }

        return false;
    }

    public static boolean onBlockInteract(ItemStack moverStack, SyncableFluidHandler handler, FilterFluidHandler filter,
                                          int[] outputTanks, int[] inputTanks) {
        int selected = moverStack.getOrDefault(CastingDataComponents.FLUID_MANAGER_SELECTED_FLUID.get(), 0);
        FluidStack stored = getStoredFluid(moverStack, selected);

        for (int tank : outputTanks) {
            if (tryCollectFromTank(moverStack, handler, tank, selected, stored)) return true;
        }
        for (int tank : inputTanks) {
            if (tryCollectFromTank(moverStack, handler, tank, selected, stored)) return true;
        }

        if (!stored.isEmpty()) {
            int[] orderedTanks = prioritizeMatchingFilteredTank(filter, inputTanks, stored);
            for (int tank : orderedTanks) {
                if (!isAllowedAtTank(filter, tank, stored)) continue;
                if (tryDeposit(moverStack, handler, tank, selected, stored)) return true;
            }
        }

        return false;
    }

    private static boolean isAllowedAtTank(FilterFluidHandler filter, int tank, FluidStack stack) {
        FluidStack filterStack = filter.getFilter(tank);
        if (filterStack.isEmpty()) return true;
        return filter.matchesFluid(FluidResource.of(stack), filterStack);
    }

    private static int[] prioritizeMatchingFilteredTank(FilterFluidHandler filter, int[] inputTanks, FluidStack stored) {
        for (int tank : inputTanks) {
            FluidStack filterStack = filter.getFilter(tank);
            if (!filterStack.isEmpty() && filter.matchesFluid(FluidResource.of(stored), filterStack)) {
                int[] reordered = new int[inputTanks.length];
                reordered[0] = tank;
                int idx = 1;
                for (int t : inputTanks) {
                    if (t != tank) reordered[idx++] = t;
                }
                return reordered;
            }
        }
        return inputTanks;
    }

    private static boolean tryCollectFromTank(ItemStack moverStack, SyncableFluidHandler handler,
                                              int tank, int selected, FluidStack stored) {
        FluidStack inTank = FluidUtil.getStack(handler, tank);
        if (inTank.isEmpty()) return false;

        boolean compatible = stored.isEmpty() || FluidStack.isSameFluidSameComponents(stored, inTank);
        if (!compatible || stored.getAmount() >= CAPACITY) return false;

        return tryCollect(moverStack, handler, tank, selected, inTank);
    }

    private static int findMatchingOrEmptySlot(ItemStack moverStack, FluidStack inTank) {
        int firstEmpty = -1;
        for (int i = 0; i < MAX_FLUID_TYPES; i++) {
            FluidStack stored = getStoredFluid(moverStack, i);
            if (stored.isEmpty()) {
                if (firstEmpty == -1) firstEmpty = i;
            } else if (FluidStack.isSameFluidSameComponents(stored, inTank) && stored.getAmount() < CAPACITY) {
                return i;
            }
        }
        return firstEmpty;
    }

    private static boolean tryCollect(ItemStack moverStack, SyncableFluidHandler handler, int tankSlot,
                                      int storedSlot, FluidStack inTank) {
        FluidStack stored = getStoredFluid(moverStack, storedSlot);
        int room = CAPACITY - stored.getAmount();
        if (room <= 0) return false;

        return handler.runInternal(() -> {
            try (Transaction tx = Transaction.open(null)) {
                int wanted = Math.min(room, inTank.getAmount());
                int extracted = handler.extract(tankSlot, FluidResource.of(inTank), wanted, tx);
                if (extracted <= 0) return false;

                FluidStack newStored = stored.isEmpty()
                        ? inTank.copyWithAmount(extracted)
                        : stored.copyWithAmount(stored.getAmount() + extracted);

                tx.commit();
                setStoredFluid(moverStack, storedSlot, newStored);
                return true;
            }
        });
    }

    private static boolean tryDeposit(ItemStack moverStack, SyncableFluidHandler handler, int tankSlot, int storedSlot, FluidStack stored) {
        try (Transaction tx = Transaction.open(null)) {
            int inserted = handler.insert(tankSlot, FluidResource.of(stored), stored.getAmount(), tx);
            if (inserted <= 0) return false;

            tx.commit();
            int remaining = stored.getAmount() - inserted;
            setStoredFluid(moverStack, storedSlot, remaining <= 0 ? FluidStack.EMPTY : stored.copyWithAmount(remaining));
            return true;
        }
    }
}