package com.benbenlaw.casting.block.entity.multiblock;

import com.benbenlaw.casting.block.CastingBlocks;
import com.benbenlaw.casting.block.entity.CastingBlockEntities;
import com.benbenlaw.casting.block.multiblock.MultiblockValveBlock;
import com.benbenlaw.casting.screen.multiblock.MultiblockControllerScreen;
import com.benbenlaw.casting.screen.multiblock.MultiblockFuelTankMenu;
import com.benbenlaw.casting.util.MultiFluidTankSharedCapacity;
import com.benbenlaw.core.block.entity.SyncableBlockEntity;
import com.benbenlaw.core.block.entity.handler.IInventoryHandlingBlockEntity;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.core.HolderLookup;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.Component;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.MenuProvider;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.inventory.AbstractContainerMenu;
import net.minecraft.world.inventory.ContainerData;
import net.minecraft.world.level.block.entity.BlockEntity;
import net.minecraft.world.level.block.state.BlockState;
import net.neoforged.neoforge.fluids.FluidStack;
import net.neoforged.neoforge.fluids.FluidUtil;
import net.neoforged.neoforge.fluids.capability.IFluidHandler;
import net.neoforged.neoforge.items.IItemHandler;
import net.neoforged.neoforge.items.ItemStackHandler;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class MultiblockValveBlockEntity extends SyncableBlockEntity implements MenuProvider, IInventoryHandlingBlockEntity {

    public final ContainerData data;
    public String selectedFluidString;
    public MultiblockControllerBlockEntity controller;
    public BlockPos controllerPos;
    public MultiblockValveBlockEntity(BlockPos pos, BlockState state) {
        super(CastingBlockEntities.MULTIBLOCK_VALVE_BLOCK_ENTITY.get(), pos, state);
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
    }


    @Override
    public void setHandler(ItemStackHandler itemStackHandler) {
        for (int i = 0; i < itemStackHandler.getSlots(); i++) {
            this.controller.itemHandler.setStackInSlot(i, itemStackHandler.getStackInSlot(i));
        }
    }

    @Override
    public ItemStackHandler getItemStackHandler() {

        if (controller == null) {
            if (controllerPos != null && level != null) {
                controller = (MultiblockControllerBlockEntity) level.getBlockEntity(controllerPos);
            }
        }
        assert controller != null;
        return controller.itemHandler;
    }
    public @Nullable IItemHandler getItemHandlerCapability(@Nullable Direction side) {
        if (controller == null && controllerPos != null && level != null) {
            BlockEntity be = level.getBlockEntity(controllerPos);
            if (be instanceof MultiblockControllerBlockEntity controllerEntity) {
                controller = controllerEntity;
            }
        }

        if (controller != null) {
            return controller.controllerItemHandler;
        } else {
            return null;
        }
    }

    public IFluidHandler getFilteredFluidHandler(Direction side) {
        return new IFluidHandler() {
            private MultiblockControllerBlockEntity getCurrentController() {
                return getController();
            }

            private IFluidHandler getOriginalHandler() {
                MultiblockControllerBlockEntity controller = getCurrentController();
                return controller != null ? controller.fluidHandler : null;
            }

            @Override
            public int getTanks() {
                IFluidHandler handler = getOriginalHandler();
                return handler != null ? handler.getTanks() : 0;
            }

            @Override
            public @NotNull FluidStack getFluidInTank(int tank) {
                IFluidHandler handler = getOriginalHandler();
                return handler != null ? handler.getFluidInTank(tank) : FluidStack.EMPTY;
            }

            @Override
            public int getTankCapacity(int tank) {
                IFluidHandler handler = getOriginalHandler();
                return handler != null ? handler.getTankCapacity(tank) : 0;
            }

            private boolean isFiltering() {
                return selectedFluidString != null && !selectedFluidString.equals("minecraft:empty");
            }

            private boolean isSelectedFluid(FluidStack stack) {
                if (stack.isEmpty()) return false;
                if (!isFiltering()) return true;
                String fluidName = stack.getFluid().toString();
                return fluidName.equals(selectedFluidString);
            }

            @Override
            public boolean isFluidValid(int tank, @NotNull FluidStack stack) {
                return isSelectedFluid(stack);
            }

            private void setWorkingState() {
                BlockState state = getBlockState();
                if (!state.getValue(MultiblockValveBlock.WORKING)) {
                    assert level != null;
                    level.setBlock(getBlockPos(), state.setValue(MultiblockValveBlock.WORKING, true), 3);
                }
            }

            private boolean isEnabled() {
                BlockState state = getBlockState();
                return state.getValue(MultiblockValveBlock.ENABLED);
            }

            @Override
            public int fill(@NotNull FluidStack resource, @NotNull FluidAction action) {
                if (!isEnabled() || resource.isEmpty() || !isSelectedFluid(resource)) return 0;
                IFluidHandler handler = getOriginalHandler();
                if (handler == null) return 0;
                int filled = handler.fill(resource, action);
                if (filled > 0) {
                    setWorkingState();
                }
                return filled;
            }

            @Override
            public @NotNull FluidStack drain(@NotNull FluidStack resource, @NotNull FluidAction action) {
                if (!isEnabled() || resource.isEmpty() || !isSelectedFluid(resource)) return FluidStack.EMPTY;
                IFluidHandler handler = getOriginalHandler();
                if (handler == null) return FluidStack.EMPTY;
                FluidStack drained = handler.drain(resource, action);
                if (!drained.isEmpty()) {
                    setWorkingState();
                }
                return drained;
            }

            @Override
            public @NotNull FluidStack drain(int maxDrain, @NotNull FluidAction action) {
                if (!isEnabled()) return FluidStack.EMPTY;
                IFluidHandler handler = getOriginalHandler();
                if (handler == null) return FluidStack.EMPTY;

                if (!isFiltering()) {
                    FluidStack drained = handler.drain(maxDrain, action);
                    if (!drained.isEmpty()) {
                        setWorkingState();
                    }
                    return drained;
                }

                if (handler instanceof MultiFluidTankSharedCapacity multiTank) {
                    for (int tank = 0; tank < multiTank.getTanks(); tank++) {
                        FluidStack stackInTank = multiTank.getFluidInTank(tank);
                        if (isSelectedFluid(stackInTank)) {
                            FluidStack drained = multiTank.drain(tank, maxDrain, action);
                            if (!drained.isEmpty()) {
                                setWorkingState();
                                return drained;
                            }
                        }
                    }
                }
                return FluidStack.EMPTY;
            }
        };
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

    public void tick() {
        assert level != null;

        if (controller != null && controller.isRemoved()) {
            controller.fluidHandler = null;
            controller = null;
        }

        if (!level.isClientSide()) {

            BlockState state = getBlockState();
            if (state.getValue(MultiblockValveBlock.WORKING)) {
                level.setBlock(getBlockPos(), state.setValue(MultiblockValveBlock.WORKING, false), 3);
            }
        }
    }

    public IFluidHandler getFluidHandlerCapability(Direction side) {
        return getFilteredFluidHandler(side);
    }

    public void setControllerBlockEntity(MultiblockControllerBlockEntity entity) {
        this.controller = entity;
    }

    public void setControllerPos(BlockPos controllerPos) {
        this.controllerPos = controllerPos;
    }

    public void setSelectedFluid(String fluid) {
        selectedFluidString = fluid;
    }

    private MultiblockControllerBlockEntity getController() {
        if (controller != null && controller.isRemoved()) {
            controller = null;
        }

        if (controller == null && controllerPos != null && level != null) {
            BlockEntity be = level.getBlockEntity(controllerPos);
            if (be instanceof MultiblockControllerBlockEntity controllerEntity) {
                controller = controllerEntity;
            }
        }

        return controller;
    }

    public void refreshController() {
        if (level != null && controllerPos != null) {
            BlockEntity be = level.getBlockEntity(controllerPos);
            if (be instanceof MultiblockControllerBlockEntity controllerEntity) {
                this.controller = controllerEntity;
            } else {
                this.controller = null;
            }
        }
    }

    @Override
    protected void saveAdditional(@NotNull CompoundTag compoundTag, HolderLookup.@NotNull Provider provider) {
        super.saveAdditional(compoundTag, provider);

        if (selectedFluidString == null) {
            selectedFluidString = "minecraft:empty";
        } else {
            compoundTag.putString("selectedFluidString", selectedFluidString);
        }

        if (controllerPos != null) {
            compoundTag.putInt("controllerPosX", controllerPos.getX());
            compoundTag.putInt("controllerPosY", controllerPos.getY());
            compoundTag.putInt("controllerPosZ", controllerPos.getZ());
        }
    }

    @Override
    protected void loadAdditional(CompoundTag compoundTag, HolderLookup.@NotNull Provider provider) {

        if (compoundTag.contains("selectedFluidString")) {
            selectedFluidString = compoundTag.getString("selectedFluidString");
        } else {
            selectedFluidString = "minecraft:empty";
        }

        if (compoundTag.contains("controllerPosX") && compoundTag.contains("controllerPosY") && compoundTag.contains("controllerPosZ")) {
            controllerPos = new BlockPos(compoundTag.getInt("controllerPosX"), compoundTag.getInt("controllerPosY"), compoundTag.getInt("controllerPosZ"));
        } else {
            controllerPos = null;
        }

        if (controllerPos != null && level != null) {
            BlockEntity be = level.getBlockEntity(controllerPos);
            if (be instanceof MultiblockControllerBlockEntity controllerEntity) {
                this.controller = controllerEntity;
            }
        }

        super.loadAdditional(compoundTag, provider);
    }

    public boolean onPlayerUse(Player player, InteractionHand hand) {

        MultiblockControllerBlockEntity controllerBlockEntity = getController();
        if (controllerBlockEntity != null) {
            return FluidUtil.interactWithFluidHandler(player, hand, controllerBlockEntity.fluidHandler);
        } else {
            System.out.println("Controller is null, cannot interact with fluid handler");
            return false;
        }
    }
}
