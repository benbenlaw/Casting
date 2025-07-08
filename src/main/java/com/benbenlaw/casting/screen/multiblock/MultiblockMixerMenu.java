package com.benbenlaw.casting.screen.multiblock;

import com.benbenlaw.casting.block.CastingBlocks;
import com.benbenlaw.casting.block.entity.multiblock.MultiblockMixerBlockEntity;
import com.benbenlaw.casting.screen.CastingMenuTypes;
import net.minecraft.core.BlockPos;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.inventory.*;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.Level;
import org.jetbrains.annotations.NotNull;

public class MultiblockMixerMenu extends AbstractContainerMenu {

    protected MultiblockMixerBlockEntity blockEntity;
    protected Level level;
    protected ContainerData data;
    protected Player player;
    protected BlockPos blockPos;

    public MultiblockMixerMenu(int containerID, Inventory inventory, FriendlyByteBuf extraData) {
        this(containerID, inventory, extraData.readBlockPos(), new SimpleContainerData(2));

    }

    public MultiblockMixerMenu(int containerID, Inventory inventory, BlockPos blockPos, ContainerData data) {
        super(CastingMenuTypes.MULTIBLOCK_MIXER_MENU.get(), containerID);
        this.player = inventory.player;
        this.blockPos = blockPos;
        this.level = inventory.player.level();
        this.data = data;
        this.blockEntity = (MultiblockMixerBlockEntity) this.level.getBlockEntity(blockPos);

        checkContainerSize(inventory, 2);
        addPlayerInventory(inventory);
        addPlayerHotbar(inventory);

        addDataSlots(data);

    }

    @Override
    public ItemStack quickMoveStack(Player p_38941_, int p_38942_) {
        return ItemStack.EMPTY;
    }

    @Override
    public boolean stillValid(@NotNull Player player) {
        return stillValid(ContainerLevelAccess.create(player.level(), blockPos),
                player, CastingBlocks.MULTIBLOCK_MIXER.get());
    }

    private void addPlayerInventory(Inventory playerInventory) {
        for (int i = 0; i < 3; ++i) {
            for (int l = 0; l < 9; ++l) {
                this.addSlot(new Slot(playerInventory, l + i * 9 + 9, 8 + l * 18, 86 + i * 18));
            }
        }
    }

    private void addPlayerHotbar(Inventory playerInventory) {
        for (int i = 0; i < 9; ++i) {
            this.addSlot(new Slot(playerInventory, i, 8 + i * 18, 144));
        }
    }
}
