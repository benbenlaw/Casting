package com.benbenlaw.casting.block.entity.multiblock;

import com.benbenlaw.casting.block.CastingBlocks;
import com.benbenlaw.casting.block.entity.CastingBlockEntities;
import com.benbenlaw.casting.block.multiblock.MultiblockFuelTankBlock;
import com.benbenlaw.casting.item.CastingDataComponents;
import com.benbenlaw.casting.item.util.FluidListComponent;
import com.benbenlaw.casting.screen.multiblock.MultiblockFuelTankMenu;
import com.benbenlaw.casting.util.SingleFluidTank;
import com.benbenlaw.core.block.entity.SyncableBlockEntity;
import com.benbenlaw.core.block.entity.handler.IInventoryHandlingBlockEntity;
import com.benbenlaw.core.block.entity.handler.InputOutputItemHandler;
import com.benbenlaw.core.util.FakePlayerUtil;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.core.HolderLookup;
import net.minecraft.core.component.DataComponentMap;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.Containers;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.MenuProvider;
import net.minecraft.world.SimpleContainer;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.inventory.AbstractContainerMenu;
import net.minecraft.world.inventory.ContainerData;
import net.minecraft.world.item.BucketItem;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.block.state.BlockState;
import net.neoforged.neoforge.common.util.FakePlayer;
import net.neoforged.neoforge.fluids.FluidStack;
import net.neoforged.neoforge.fluids.FluidType;
import net.neoforged.neoforge.fluids.FluidUtil;
import net.neoforged.neoforge.fluids.capability.IFluidHandler;
import net.neoforged.neoforge.items.IItemHandler;
import net.neoforged.neoforge.items.ItemStackHandler;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;

public class MultiblockFuelTankBlockEntity extends SyncableBlockEntity implements MenuProvider, IInventoryHandlingBlockEntity {

    private final ItemStackHandler itemHandler = new ItemStackHandler(2) {
        @Override
        protected void onContentsChanged(int slot) {
            setChanged();
            sync();
        }
    };

    private final IItemHandler controllerTankItemHandler = new InputOutputItemHandler(itemHandler,
            (i, stack) -> i == 0,
            i -> i == 1
    );

    public @Nullable IItemHandler getItemHandlerCapability(@Nullable Direction side) {
        return controllerTankItemHandler;
    }

    @Override
    public void setHandler(ItemStackHandler itemStackHandler) {
        for (int i = 0; i < itemStackHandler.getSlots(); i++) {
            this.itemHandler.setStackInSlot(i, itemStackHandler.getStackInSlot(i));
        }
    }

    public ItemStackHandler getItemStackHandler() {
        return itemHandler;
    }

    public final SingleFluidTank fluidHandler = new SingleFluidTank(4000) {
        @Override
        protected void onContentsChanged() {
            setChanged();
            sync();
        }
    };

    public IFluidHandler getFluidHandlerCapability(Direction side) {
        return fluidHandler;
    }
    public final ContainerData data;
    private FakePlayer fakePlayer;

    public MultiblockFuelTankBlockEntity(BlockPos pos, BlockState state) {
        super(CastingBlockEntities.MULTIBLOCK_FUEL_TANK_BLOCK_ENTITY.get(), pos, state);
        this.data = new ContainerData() {
            @Override
            public int get(int p_39284_) {
                return 0;
            }

            @Override
            public void set(int p_39285_, int p_39286_) {

            }

            @Override
            public int getCount() {
                return 0;
            }
        };
        if (level instanceof ServerLevel serverLevel) {
            this.fakePlayer = FakePlayerUtil.createFakePlayer(serverLevel, "castingFuelTank");
        }

    }

    @Override
    public @NotNull Component getDisplayName() {
        return CastingBlocks.MULTIBLOCK_FUEL_TANK.get().getName();
    }

    @Nullable
    @Override
    public AbstractContainerMenu createMenu(int container, @NotNull Inventory inventory, @NotNull Player player) {
        return new MultiblockFuelTankMenu(container, inventory, this.getBlockPos(), data);
    }

    public void drops() {
        SimpleContainer inventory = new SimpleContainer(itemHandler.getSlots());
        for (int i = 0; i < itemHandler.getSlots(); i++) {
            inventory.setItem(i, itemHandler.getStackInSlot(i));
        }
        assert this.level != null;
        Containers.dropContents(this.level, this.worldPosition, inventory);
    }

    public void tick() {

        if (this.fakePlayer == null && level instanceof ServerLevel serverLevel) {
            this.fakePlayer = FakePlayerUtil.createFakePlayer(serverLevel, "castingControllerTank");
        }

        FluidStack fluidStack = fluidHandler.getFluid();
        int fluidLightLevel = fluidHandler.isEmpty() ? 0 : fluidStack.getFluid().getFluidType().getLightLevel();
        BlockState state = this.getBlockState();

        if (state.getValue(MultiblockFuelTankBlock.LIGHT_LEVEL) != fluidLightLevel) {
            assert this.level != null;
            this.level.setBlock(this.worldPosition, state.setValue(MultiblockFuelTankBlock.LIGHT_LEVEL, fluidLightLevel), 3);
        }

        if (!itemHandler.getStackInSlot(0).isEmpty() && itemHandler.getStackInSlot(1).isEmpty() && fakePlayer != null) {
            ItemStack stack = itemHandler.getStackInSlot(0);
            Item item = stack.getItem();

            FluidStack fluidStackFromBucket = FluidUtil.getFluidContained(stack).orElse(FluidStack.EMPTY);

            if (fluidStackFromBucket.isEmpty()) {
                FluidStack fluidInTank = fluidHandler.getFluid();
                if (!fluidInTank.isEmpty() && fluidInTank.getAmount() >= FluidType.BUCKET_VOLUME) {
                    ItemStack filledBucket = FluidUtil.tryFillContainer(stack, fluidHandler, FluidType.BUCKET_VOLUME, null, true).getResult();
                    if (!filledBucket.isEmpty()) {
                        itemHandler.extractItem(0, 1, false);
                        itemHandler.insertItem(1, filledBucket.copy(), false);
                    }
                }

            } else {
                if (item instanceof BucketItem) {
                    if (fluidHandler.fill(fluidStackFromBucket, IFluidHandler.FluidAction.SIMULATE) > 0) {
                        ItemStack emptyBucket = BucketItem.getEmptySuccessItem(stack, fakePlayer);

                        itemHandler.extractItem(0, 1, false);
                        itemHandler.insertItem(1, emptyBucket.copy(), false);
                        fluidHandler.fill(fluidStackFromBucket, IFluidHandler.FluidAction.EXECUTE);
                    }
                }
            }
        }
        sync();
    }

    @Override
    protected void saveAdditional(@NotNull CompoundTag compoundTag, HolderLookup.@NotNull Provider provider) {
        super.saveAdditional(compoundTag, provider);
        compoundTag.put("inventory", this.itemHandler.serializeNBT(provider));
        compoundTag.put("fluidTank", fluidHandler.writeToNBT(provider, new CompoundTag()));
    }

    @Override
    protected void loadAdditional(CompoundTag compoundTag, HolderLookup.@NotNull Provider provider) {
        itemHandler.deserializeNBT(provider, compoundTag.getCompound("inventory"));
        fluidHandler.readFromNBT(provider, compoundTag.getCompound("fluidTank"));

        super.loadAdditional(compoundTag, provider);
    }

    public boolean onPlayerUse(Player player, InteractionHand hand) {
        return FluidUtil.interactWithFluidHandler(player, hand, fluidHandler);
    }

    @Override
    protected void collectImplicitComponents(DataComponentMap.Builder builder) {
        super.collectImplicitComponents(builder);

        FluidStack fluid = this.fluidHandler.getFluid();
        if (!fluid.isEmpty()) {
            builder.set(CastingDataComponents.FLUIDS, new FluidListComponent(List.of(fluid.copy())));
        }
    }

    @Override
    protected void applyImplicitComponents(DataComponentInput input) {
        super.applyImplicitComponents(input);
        FluidListComponent component = input.get(CastingDataComponents.FLUIDS);
        if (component != null && !component.fluids().isEmpty()) {
            this.fluidHandler.setFluid(component.fluids().get(0).copy());
        }
    }

}
