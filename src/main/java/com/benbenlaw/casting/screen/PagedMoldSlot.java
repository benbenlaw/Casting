package com.benbenlaw.casting.screen;

import com.benbenlaw.casting.util.CastingTags;
import com.benbenlaw.core.block.entity.handler.item.SyncableItemHandler;
import net.minecraft.world.SimpleContainer;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.inventory.Slot;
import net.minecraft.world.item.ItemStack;
import net.neoforged.neoforge.transfer.item.ItemResource;
import net.neoforged.neoforge.transfer.item.ItemUtil;
import net.neoforged.neoforge.transfer.transaction.Transaction;

public class PagedMoldSlot extends Slot {

    private final SolidifierMenu menu;
    private final int index;

    public PagedMoldSlot(SolidifierMenu menu, int index, int x, int y) {
        super(new SimpleContainer(1), 0, x, y);
        this.menu = menu;
        this.index = index;
    }

    private int realIndex() {
        return menu.getMoldPage() * 5 + index;
    }

    private SyncableItemHandler handler() {
        return menu.blockEntity.getStoredMolds();
    }

    @Override
    public boolean hasItem() {
        return !getItem().isEmpty();
    }

    @Override
    public ItemStack getItem() {
        int real = realIndex();
        var h = handler();

        if (real < 0 || real >= h.size()) return ItemStack.EMPTY;
        return ItemUtil.getStack(h, real);
    }

    @Override
    public void set(ItemStack stack) {
        int real = realIndex();
        var h = handler();

        if (real < 0 || real >= h.size()) return;

        if (!stack.isEmpty()) {

            for (int i = 0; i < h.size(); i++) {
                if (i == real) continue;

                ItemStack existing = ItemUtil.getStack(h, i);

                if (!existing.isEmpty() &&
                        ItemStack.isSameItemSameComponents(existing, stack)) {
                    return;
                }
            }
        }

        h.runInternal(() -> {
            try (Transaction tx = Transaction.openRoot()) {

                if (stack.isEmpty()) {
                    h.set(real, ItemResource.EMPTY, 0);
                } else {
                    h.set(real, ItemResource.of(stack.copy()), 1);
                }

                tx.commit();
            }
        });

        setChanged();
    }

    @Override
    public ItemStack remove(int amount) {
        ItemStack current = getItem();
        if (current.isEmpty()) return ItemStack.EMPTY;

        int take = Math.min(amount, current.getCount());

        ItemStack result = current.copy();
        result.setCount(take);
        ItemStack remaining = current.copy();
        remaining.shrink(take);
        set(remaining);

        return result;
    }

    @Override
    public boolean mayPlace(ItemStack stack) {
        if (!stack.is(CastingTags.Items.MOLDS)) return false;

        var h = handler();
        for (int i = 0; i < h.size(); i++) {
            ItemStack existing = ItemUtil.getStack(h, i);

            if (!existing.isEmpty() &&
                    ItemStack.isSameItemSameComponents(existing, stack)) {
                return false;
            }
        }

        return true;
    }

    @Override
    public boolean mayPickup(Player player) {
        return true;
    }

    @Override
    public int getMaxStackSize() {
        return 1;
    }
}