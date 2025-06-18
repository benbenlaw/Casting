package com.benbenlaw.casting.block.entity.multiblock;

import com.benbenlaw.casting.block.CastingBlocks;
import com.benbenlaw.casting.block.entity.CastingBlockEntities;
import com.benbenlaw.casting.recipe.CoolantRecipe;
import com.benbenlaw.casting.recipe.SolidifierRecipe;
import com.benbenlaw.casting.screen.multiblock.MultiblockSolidifierMenu;
import com.benbenlaw.casting.util.CastingTags;
import com.benbenlaw.casting.util.SingleFluidTank;
import com.benbenlaw.core.block.entity.SyncableBlockEntity;
import com.benbenlaw.core.block.entity.handler.IInventoryHandlingBlockEntity;
import com.benbenlaw.core.block.entity.handler.InputOutputItemHandler;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.core.HolderLookup;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.Containers;
import net.minecraft.world.MenuProvider;
import net.minecraft.world.SimpleContainer;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.inventory.AbstractContainerMenu;
import net.minecraft.world.inventory.ContainerData;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.crafting.RecipeHolder;
import net.minecraft.world.item.crafting.RecipeInput;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.material.Fluid;
import net.neoforged.neoforge.common.crafting.SizedIngredient;
import net.neoforged.neoforge.fluids.FluidStack;
import net.neoforged.neoforge.fluids.capability.IFluidHandler;
import net.neoforged.neoforge.items.IItemHandler;
import net.neoforged.neoforge.items.ItemStackHandler;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;

import static com.benbenlaw.casting.block.multiblock.MultiblockSolidifierBlock.ENABLED;
import static com.benbenlaw.casting.block.multiblock.MultiblockSolidifierBlock.WORKING;

public class MultiblockSolidifierBlockEntity extends SyncableBlockEntity implements MenuProvider, IInventoryHandlingBlockEntity {

    private final ItemStackHandler itemHandler = new ItemStackHandler(2) {
        @Override
        protected void onContentsChanged(int slot) {
            setChanged();
            sync();
        }

        @Override
        protected int getStackLimit(int slot, ItemStack stack) {
            if (slot == 0 && stack.is(CastingTags.Items.MOLDS)) {
                return 1;
            }
            return 64;
        }

        @Override
        public int getSlotLimit(int slot) {
            if (slot == 0 && itemHandler.getStackInSlot(slot).is(CastingTags.Items.MOLDS)) {
                return 1;
            }
            return 64;
        }
    };
    public IFluidHandler getFluidHandlerCapability(Direction side) {
        return null;
        //disabled because we dont actuaslly need fluid handing on the solidifier
        //return controller.fluidHandler;
    }

    public final ContainerData data;
    public int progress = 0;
    public int maxProgress;
    public int SOLIDIFIER_MAX_PROGRESS = 300;
    public String selectedFluidString;
    public SingleFluidTank coolantTank;
    public MultiblockControllerBlockEntity controller;
    public BlockPos controllerPos;

    public FluidStack coolantFluidStack = FluidStack.EMPTY;
    private final IItemHandler solidifierItemHandler = new InputOutputItemHandler(itemHandler,
            (i, stack) -> i == 0,  //
            i -> i == 1
    );

    public @Nullable IItemHandler getItemHandlerCapability(@Nullable Direction side) {
        return solidifierItemHandler;
    }

    public void setHandler(ItemStackHandler handler) {
        for (int i = 0; i < handler.getSlots(); i++) {
            this.itemHandler.setStackInSlot(i, handler.getStackInSlot(i));
        }
    }

    public ItemStackHandler getItemStackHandler() {
        return this.itemHandler;
    }

    public MultiblockSolidifierBlockEntity(BlockPos pos, BlockState state) {
        super(CastingBlockEntities.MULTIBLOCK_SOLIDIFIER_BLOCK_ENTITY.get(), pos, state);
        this.data = new ContainerData() {
            public int get(int index) {
                return switch (index) {
                    case 0 -> MultiblockSolidifierBlockEntity.this.progress;
                    case 1 -> MultiblockSolidifierBlockEntity.this.maxProgress;
                    default -> 0;
                };
            }

            public void set(int index, int value) {
                switch (index) {
                    case 0 -> MultiblockSolidifierBlockEntity.this.progress = value;
                    case 1 -> MultiblockSolidifierBlockEntity.this.maxProgress = value;
                }
            }

            public int getCount() {
                return 2;
            }
        };
    }


    @Override
    public Component getDisplayName() {
        return CastingBlocks.MULTIBLOCK_SOLIDIFIER.get().getName();
    }

    @Nullable
    @Override
    public AbstractContainerMenu createMenu(int container, @NotNull Inventory inventory, @NotNull Player player) {
        return new MultiblockSolidifierMenu(container, inventory, this.getBlockPos(), data);
    }
    public void tick() {
        assert level != null;

        if (controller != null && controller.isRemoved()) {
            controller.fluidHandler = null;
            controller = null;
        }

        if (!level.isClientSide()) {
            if (!this.getBlockState().getValue(ENABLED)) {
                level.setBlock(worldPosition, getBlockState().setValue(WORKING, false), 3);
                return;
            }

            if (controller == null) {
                level.setBlock(worldPosition, getBlockState().setValue(WORKING, false), 3);
                resetProgress();
                return;
            }

            RecipeInput inventory = new RecipeInput() {
                @Override
                public @NotNull ItemStack getItem(int index) {
                    return itemHandler.getStackInSlot(index);
                }

                @Override
                public int size() {
                    return itemHandler.getSlots();
                }
            };

            sync();

            if (itemHandler.getStackInSlot(0).isEmpty()) {
                resetProgress();
                level.setBlock(worldPosition, getBlockState().setValue(WORKING, false), 3);
                return;
            }

            boolean foundMatch = false;

            for (RecipeHolder<SolidifierRecipe> recipeHolder : level.getRecipeManager().getRecipesFor(SolidifierRecipe.Type.INSTANCE, inventory, level)) {
                SolidifierRecipe recipe = recipeHolder.value();
                Fluid recipeFluid = recipe.fluid().getFluid();

                // Use selected fluid as a filter if it's set
                boolean filterMatches = selectedFluidString.equals("minecraft:empty") ||
                        getSelectedFluid().isSame(recipeFluid);

                if (filterMatches &&
                        recipe.mold().test(itemHandler.getStackInSlot(0)) &&
                        hasEnoughFluid(recipe.fluid()) &&
                        hasCorrectInputAmount(recipe.mold())) {

                    FluidStack output = recipe.fluid();
                    updateSpeed();

                    if (hasOutputSpaceMaking(this, recipe)) {
                        progress++;


                        level.setBlock(worldPosition, getBlockState().setValue(WORKING, true), 3);

                        if (progress >= maxProgress) {
                            extractFluid(output, output.getAmount());

                            if (!itemHandler.getStackInSlot(0).is(CastingTags.Items.MOLDS)) {
                                itemHandler.getStackInSlot(0).shrink(recipe.mold().count());
                            }

                            itemHandler.setStackInSlot(1, new ItemStack(recipe.output().getItems()[0].getItem(),
                                    recipe.output().count() + itemHandler.getStackInSlot(1).getCount()));
                            setChanged();
                            resetProgress();
                        }

                        foundMatch = true;
                        break;
                    }
                }
            }

            if (!foundMatch) {
                resetProgress();
            }
        }
    }


    public void setCoolantTank(SingleFluidTank coolantTank) {
        this.coolantTank = coolantTank;
    }

    public Fluid getSelectedFluid() {
        return BuiltInRegistries.FLUID.get(ResourceLocation.parse(selectedFluidString));
    }
    public void setSelectedFluid(String fluid) {
        selectedFluidString = fluid;
        sync();
    }

    private boolean hasCorrectInputAmount(SizedIngredient mold) {
        return itemHandler.getStackInSlot(0).getCount() >= mold.count();
    }


    private void updateSpeed() {
        maxProgress = SOLIDIFIER_MAX_PROGRESS;
        assert level != null;

        if (coolantTank == null) return;

        List<RecipeHolder<CoolantRecipe>> allFuels = level.getRecipeManager().getAllRecipesFor(CoolantRecipe.Type.INSTANCE);

        for (RecipeHolder<CoolantRecipe> recipeHolder : allFuels) {
            CoolantRecipe recipe = recipeHolder.value();
            if (recipe.fluid().getFluid() == coolantTank.getFluid().getFluid() && coolantTank.getFluidAmount() >= recipe.fluid().getAmount()) {
                maxProgress = Math.max(recipe.duration(), 10);
                coolantFluidStack = recipe.fluid();
            }
        }
    }

    private void resetProgress() {
        progress = 0;
    }

    public void setControllerBlockEntity(MultiblockControllerBlockEntity entity) {
        this.controller = entity;
    }

    public void setControllerPos(BlockPos controllerPos) {
        this.controllerPos = controllerPos;
    }

    private void extractFluid(FluidStack output, int amount) {
        Fluid recipeFluid = output.getFluid();

        // Drain Coolant Tank
        if (coolantTank != null) {
            if (maxProgress <= SOLIDIFIER_MAX_PROGRESS && !coolantFluidStack.isEmpty()) {
                coolantTank.drain(coolantFluidStack, IFluidHandler.FluidAction.EXECUTE);
            }
        }

        // Drain Controller Tank (match fluid used by recipe)
        List<FluidStack> fluids = controller.fluidHandler.getFluids();
        for (FluidStack fluid : fluids) {
            if (!fluid.isEmpty() && fluid.getFluid() == recipeFluid && fluid.getAmount() >= amount) {
                controller.fluidHandler.drain(new FluidStack(recipeFluid, amount), IFluidHandler.FluidAction.EXECUTE);
                return;
            }
        }
    }


    private boolean hasEnoughFluid(FluidStack required) {
        if (controller == null) return false;
        List<FluidStack> fluids = controller.fluidHandler.getFluids();
        for (FluidStack fluid : fluids) {
            if (!fluid.isEmpty() && fluid.getFluid() == required.getFluid()) {
                return fluid.getAmount() >= required.getAmount();
            }
        }
        return false;
    }

    private boolean tankIsValidForSlot(FluidStack stack, int slot) {
        Fluid selected = getSelectedFluid();
        return selected != null && stack.getFluid() == selected;
    }

    private boolean hasOutputSpaceMaking(MultiblockSolidifierBlockEntity entity, SolidifierRecipe recipe) {
        ItemStack outputSlotStack = entity.itemHandler.getStackInSlot(1);
        SizedIngredient resultStack = recipe.output();

        if (outputSlotStack.isEmpty()) {
            return  recipe.output().count() <= resultStack.getItems()[0].getItem().getDefaultMaxStackSize();
        } else if (outputSlotStack.getItem() == resultStack.getItems()[0].getItem()) {
            return outputSlotStack.getCount() + recipe.output().count() <= outputSlotStack.getMaxStackSize();
        } else {
            return false;
        }
    }

    public void drops() {
        SimpleContainer inventory = new SimpleContainer(itemHandler.getSlots());
        for (int i = 0; i < itemHandler.getSlots(); i++) {
            inventory.setItem(i, itemHandler.getStackInSlot(i));
        }
        assert this.level != null;
        Containers.dropContents(this.level, this.worldPosition, inventory);
    }


    @Override
    protected void saveAdditional(@NotNull CompoundTag compoundTag, HolderLookup.@NotNull Provider provider) {
        super.saveAdditional(compoundTag, provider);
        compoundTag.put("inventory", this.itemHandler.serializeNBT(provider));
        compoundTag.putInt("progress", progress);
        compoundTag.putInt("maxProgress", maxProgress);

        if (selectedFluidString == null) {
            selectedFluidString = "minecraft:empty";
        } else {
            compoundTag.putString("selectedFluidString", selectedFluidString);
        }

        if (coolantTank != null) {
            compoundTag.put("coolantTank", coolantTank.writeToNBT(provider, new CompoundTag()));
        }

        if (controllerPos != null) {
            compoundTag.putInt("controllerPosX", controllerPos.getX());
            compoundTag.putInt("controllerPosY", controllerPos.getY());
            compoundTag.putInt("controllerPosZ", controllerPos.getZ());
        }

    }

    @Override
    protected void loadAdditional(CompoundTag compoundTag, HolderLookup.@NotNull Provider provider) {
        this.itemHandler.deserializeNBT(provider, compoundTag.getCompound("inventory"));
        progress = compoundTag.getInt("progress");
        maxProgress = compoundTag.getInt("maxProgress");

        if (compoundTag.contains("selectedFluidString")) {
            selectedFluidString = compoundTag.getString("selectedFluidString");
        } else {
            selectedFluidString = "minecraft:empty";
        }

        if (compoundTag.contains("coolantTank") && coolantTank != null) {
            coolantTank.readFromNBT(provider, compoundTag.getCompound("coolantTank"));
        }

        if (compoundTag.contains("controllerPosX") && compoundTag.contains("controllerPosY") && compoundTag.contains("controllerPosZ")) {
            controllerPos = new BlockPos(compoundTag.getInt("controllerPosX"), compoundTag.getInt("controllerPosY"), compoundTag.getInt("controllerPosZ"));
        } else {
            controllerPos = null;
        }
    }
}