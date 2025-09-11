package com.benbenlaw.casting.util;

import net.minecraft.core.Direction;
import net.minecraft.world.item.ItemStack;
import net.neoforged.neoforge.common.util.TriPredicate;
import net.neoforged.neoforge.items.IItemHandlerModifiable;
import org.jetbrains.annotations.NotNull;

import java.util.function.BiPredicate;
import java.util.function.Predicate;

public class SidedInputOutputItemHandler implements IItemHandlerModifiable {
    private final IItemHandlerModifiable handler;
    private final Direction direction;
    private final TriPredicate<Direction, Integer, ItemStack> canInput;
    private final BiPredicateWithSide canOutput;

    @FunctionalInterface
    public interface TriPredicate<A, B, C> {
        boolean test(A a, B b, C c);
    }

    @FunctionalInterface
    public interface BiPredicateWithSide {
        boolean test(Direction side, int slot);
    }

    public SidedInputOutputItemHandler(IItemHandlerModifiable handler, Direction direction, TriPredicate<Direction, Integer, ItemStack> canInput, BiPredicateWithSide canOutput) {
        this.handler = handler;
        this.direction = direction;
        this.canInput = canInput;
        this.canOutput = canOutput;
    }

    public void setStackInSlot(int slot, @NotNull ItemStack stack) {
        this.handler.setStackInSlot(slot, stack);
    }

    public @NotNull ItemStack getStackInSlot(int slot) {
        return this.handler.getStackInSlot(slot);
    }

    public int getSlots() {
        return this.handler.getSlots();
    }

    public @NotNull ItemStack insertItem(int slot, @NotNull ItemStack stack, boolean simulate) {
        return this.canInput.test(direction, slot, stack) ? this.handler.insertItem(slot, stack, simulate) : stack;
    }

    public @NotNull ItemStack extractItem(int slot, int amount, boolean simulate) {
        return this.canOutput.test(direction, slot) ? this.handler.extractItem(slot, amount, simulate) : ItemStack.EMPTY;
    }

    public int getSlotLimit(int slot) {
        return this.handler.getSlotLimit(slot);
    }

    public boolean isItemValid(int slot, @NotNull ItemStack stack) {
        return this.canInput.test(direction, slot, stack) && this.handler.isItemValid(slot, stack);
    }
}
