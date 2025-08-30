package com.benbenlaw.casting.block.entity.multiblock;

import com.benbenlaw.casting.block.CastingBlocks;
import com.benbenlaw.casting.block.entity.CastingBlockEntities;
import com.benbenlaw.casting.block.multiblock.MultiblockMixerBlock;
import com.benbenlaw.casting.recipe.MixingRecipe;
import com.benbenlaw.casting.screen.multiblock.MultiblockFuelTankMenu;
import com.benbenlaw.core.block.entity.SyncableBlockEntity;
import com.benbenlaw.core.recipe.NoInventoryRecipe;
import net.minecraft.core.BlockPos;
import net.minecraft.core.HolderLookup;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.nbt.StringTag;
import net.minecraft.nbt.Tag;
import net.minecraft.network.chat.Component;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.MenuProvider;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.inventory.AbstractContainerMenu;
import net.minecraft.world.inventory.ContainerData;
import net.minecraft.world.item.crafting.RecipeHolder;
import net.minecraft.world.level.block.state.BlockState;
import net.neoforged.neoforge.fluids.FluidStack;
import net.neoforged.neoforge.fluids.FluidUtil;
import net.neoforged.neoforge.fluids.capability.IFluidHandler;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.List;

public class MultiblockMixerBlockEntity extends SyncableBlockEntity implements MenuProvider {

    public final ContainerData data;
    public String selectedFluidString;
    public MultiblockControllerBlockEntity controller;
    public BlockPos controllerPos;
    public List<String> availableAlloys = new ArrayList<>();
    public MultiblockMixerBlockEntity(BlockPos pos, BlockState state) {
        super(CastingBlockEntities.MULTIBLOCK_MIXER_BLOCK_ENTITY.get(), pos, state);
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
            return;
        }

        if (!level.isClientSide()) {
            sync();

            if (controller != null) {
                findMixerRecipes();
                craftAlloy();
            }
        }
    }

    public void findMixerRecipes() {

        assert level != null;
        availableAlloys.clear();

        if (controller == null) return;

        for (RecipeHolder<MixingRecipe> recipeHolder : level.getRecipeManager().getRecipesFor(MixingRecipe.Type.INSTANCE, NoInventoryRecipe.INSTANCE, level)) {

            MixingRecipe recipe = recipeHolder.value();
            List<FluidStack> recipeFluids = recipe.getAllFluids();

            List<FluidStack> controllerFluids = controller.fluidHandler.getFluids();
            boolean allFluidsPresent = true;

            for (FluidStack requiredFluid : recipeFluids) {
                boolean found = false;

                for (FluidStack controllerFluid : controllerFluids) {
                    if (FluidStack.isSameFluid(controllerFluid, requiredFluid)
                            && controllerFluid.getAmount() >= requiredFluid.getAmount()) {
                        found = true;
                        break;
                    }
                }
                if (!found) {
                    allFluidsPresent = false;
                    break;
                }
            }

            if (allFluidsPresent) {
                availableAlloys.add(recipe.outputFluid().getFluid().toString());
            }
        }
    }

    public void craftAlloy() {

        if (!this.getBlockState().getValue(MultiblockMixerBlock.ENABLED)) return;

        boolean didCraft = false;

        assert level != null;
        for (RecipeHolder<MixingRecipe> recipeHolder : level.getRecipeManager().getRecipesFor(MixingRecipe.Type.INSTANCE, NoInventoryRecipe.INSTANCE, level)) {

            MixingRecipe recipe = recipeHolder.value();
            List<FluidStack> recipeFluids = recipe.getAllFluids();
            List<FluidStack> controllerFluids = controller.fluidHandler.getFluids();
            boolean canCraft = true;

            if (recipe.outputFluid().getFluid().toString().contains(selectedFluidString)) {

                for (FluidStack requiredFluid : recipeFluids) {
                    boolean found = false;
                    for (FluidStack controllerFluid : controllerFluids) {
                        if (FluidStack.isSameFluid(controllerFluid, requiredFluid)
                                && controllerFluid.getAmount() >= requiredFluid.getAmount()) {
                            found = true;
                            break;
                        }
                    }
                    if (!found) {
                        canCraft = false;
                        break;
                    }
                }

                if (canCraft) {

                    int filledAmount = controller.fluidHandler.fill(recipe.outputFluid(), IFluidHandler.FluidAction.SIMULATE);
                    if (filledAmount >= recipe.outputFluid().getAmount()) {

                        for (FluidStack requiredFluid : recipeFluids) {
                            controller.fluidHandler.drain(requiredFluid, IFluidHandler.FluidAction.EXECUTE);
                        }
                        controller.fluidHandler.fill(recipe.outputFluid(), IFluidHandler.FluidAction.EXECUTE);

                        didCraft = true;
                        controller.errorMessage = "";
                        break;
                    } else {
                        controller.errorMessage = "output_full_mixer";
                    }
                }
            }
        }

        BlockState currentState = this.getBlockState();
        if (currentState.getValue(MultiblockMixerBlock.WORKING) != didCraft) {
            level.setBlock(worldPosition, currentState.setValue(MultiblockMixerBlock.WORKING, didCraft), 3);
        }
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

        ListTag availableAlloysTag = new ListTag();
        for (String alloy : availableAlloys) {
            availableAlloysTag.add(StringTag.valueOf(alloy));
        }
        compoundTag.put("availableAlloys", availableAlloysTag);


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

        availableAlloys.clear();
        if (compoundTag.contains("availableAlloys")) {
            ListTag availableAlloysTag = compoundTag.getList("availableAlloys", StringTag.TAG_STRING);
            for (Tag tag : availableAlloysTag) {
                if (tag instanceof StringTag stringTag) {
                    availableAlloys.add(stringTag.getAsString());
                }
            }
        }

        super.loadAdditional(compoundTag, provider);
    }

    public boolean onPlayerUse(Player player, InteractionHand hand) {
        if (controller != null) {
            return FluidUtil.interactWithFluidHandler(player, hand, controller.fluidHandler);
        } else {
            return false;
        }
    }
}
