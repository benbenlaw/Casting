package com.benbenlaw.casting.block.entity;

import com.benbenlaw.casting.block.CastingBlockEntities;
import com.benbenlaw.casting.block.custom.CastingBlock;
import com.benbenlaw.casting.block.custom.MixerBlock;
import com.benbenlaw.casting.config.CastingConfig;
import com.benbenlaw.casting.item.CastingDataComponents;
import com.benbenlaw.casting.item.FluidMoverItem;
import com.benbenlaw.casting.item.util.FluidListComponent;
import com.benbenlaw.casting.recipe.custom.MixingRecipe;
import com.benbenlaw.casting.screen.MixerMenu;
import com.benbenlaw.core.block.entity.SyncableBlockEntity;
import com.benbenlaw.core.block.entity.handler.fluid.FilterFluidHandler;
import com.benbenlaw.core.block.entity.handler.fluid.SyncableFluidHandler;
import net.minecraft.core.BlockPos;
import net.minecraft.core.component.DataComponentGetter;
import net.minecraft.core.component.DataComponentMap;
import net.minecraft.network.chat.Component;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.MenuProvider;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.inventory.AbstractContainerMenu;
import net.minecraft.world.inventory.ContainerData;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.crafting.RecipeHolder;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.storage.ValueInput;
import net.minecraft.world.level.storage.ValueOutput;
import net.neoforged.neoforge.fluids.FluidStack;
import net.neoforged.neoforge.fluids.crafting.SizedFluidIngredient;
import net.neoforged.neoforge.transfer.fluid.FluidResource;
import net.neoforged.neoforge.transfer.fluid.FluidStacksResourceHandler;
import net.neoforged.neoforge.transfer.fluid.FluidUtil;
import net.neoforged.neoforge.transfer.transaction.Transaction;
import org.jetbrains.annotations.Nullable;
import org.jspecify.annotations.NonNull;

public class MixerBlockEntity extends SyncableBlockEntity implements MenuProvider, FluidSending, FluidAccepting {

    private final ContainerData data;
    private int maxProgress = CastingConfig.defaultMixerSpeed.get();
    private int progress = 0;

    private final SyncableFluidHandler fluidInventory = new SyncableFluidHandler(this, 5, 8000,
            (i, stack) -> i <= 3,
            i -> i == 4)
    {
        @Override
        public boolean isValid(int index, FluidResource resource) {
            if (!super.isValid(index, resource)) {
                return false;
            }

            if (resource.isEmpty() || index > 3) {
                return true;
            }

            if (isFluidInAnotherSlot(index, resource)) {
                return false;
            }

            return filterFluidHandler.matchesFluid(resource, true, false);
        }

        private boolean isFluidInAnotherSlot(int index, FluidResource resource) {
            for (int i = 0; i <= 3; i++) {
                if (i == index) continue;

                FluidResource other = getResource(i);
                if (!other.isEmpty() && other.equals(resource)) {
                    return true;
                }
            }

            return false;
        }
    };


    private final FilterFluidHandler filterFluidHandler = new FilterFluidHandler(this, 4);

    public MixerBlockEntity(BlockPos pos, BlockState state) {
        super(CastingBlockEntities.MIXER_BLOCK_ENTITY.get(), pos, state);
        this.data = new ContainerData() {
            @Override
            public int get(int index) {
                return switch (index) {
                    case 0 -> MixerBlockEntity.this.progress;
                    case 1 -> MixerBlockEntity.this.maxProgress;
                    default -> 0;
                };
            }

            @Override
            public void set(int index, int value) {
                switch (index) {
                    case 0 -> MixerBlockEntity.this.progress = value;
                    case 1 -> MixerBlockEntity.this.maxProgress = value;
                }
            }

            @Override
            public int getCount() {
                return 2;
            }
        };
    }

    public void tick() {
        if (level == null || level.isClientSide()) return;

        boolean isRunning = level.getBlockState(worldPosition).getValue(MixerBlock.RUNNING);

        if (!isRunning) {
            updateWorkingState(false);
            if (this.progress > 0) {
                this.progress = 0;
                setChanged();
                sync();
            }
            return;
        }

        RecipeHolder<MixingRecipe> recipeHolder = getRecipe();
        boolean changed = false;
        boolean isCurrentlyWorking = false;

        if (recipeHolder != null) {
            MixingRecipe recipe = recipeHolder.value();

            if (canFormOutput(recipe)) {
                isCurrentlyWorking = true;
                this.progress++;
                changed = true;

                if (this.progress >= this.maxProgress) {
                    executeMixing(recipe);
                    this.progress = 0;
                }
            } else {
                if (this.progress > 0) {
                    this.progress = 0;
                    changed = true;
                }
            }
        } else {
            if (this.progress > 0) {
                this.progress = 0;
                changed = true;
            }
        }

        updateWorkingState(isCurrentlyWorking);
        this.tickResourceSending(level, worldPosition);

        if (changed) {
            setChanged();
            sync();
        }
    }

    private void updateWorkingState(boolean working) {
        assert level != null;
        BlockState currentState = level.getBlockState(worldPosition);
        if (currentState.getValue(CastingBlock.WORKING) != working) {
            level.setBlock(worldPosition, currentState.setValue(CastingBlock.WORKING, working), 3);
        }
    }

    private boolean canFormOutput(MixingRecipe recipe) {
        FluidStack output = recipe.outputFluid().create();

        try (Transaction tx = Transaction.open(null)) {
            return fluidInventory.runInternal(() -> {
                int inserted = fluidInventory.insert(4, FluidResource.of(output), output.getAmount(), tx);
                return inserted == output.getAmount();
            });
        }
    }

    private void executeMixing(MixingRecipe recipe) {
        fluidInventory.runInternal(() -> {
            try (Transaction tx = Transaction.open(null)) {
                for (SizedFluidIngredient required : recipe.fluids()) {
                    int remainingToDrain = required.amount();
                    for (int i = 0; i < 4 && remainingToDrain > 0; i++) {
                        FluidStack inTank = FluidUtil.getStack(fluidInventory, i);

                        if (required.ingredient().test(inTank)) {
                            int drained = fluidInventory.extract(i, FluidResource.of(inTank), remainingToDrain, tx);
                            remainingToDrain -= drained;
                        }
                    }

                    if (remainingToDrain > 0) return;
                }

                FluidStack outputStack = recipe.outputFluid().create();
                fluidInventory.insert(
                        4,
                        FluidResource.of(outputStack),
                        outputStack.getAmount(),
                        tx
                );

                tx.commit();
            }
        });
    }

    private RecipeHolder<MixingRecipe> getRecipe() {
        if (level == null || level.getServer() == null) return null;

        return level.getServer().getRecipeManager()
                .recipeMap()
                .values()
                .stream()
                .filter(holder -> holder.value().getType() == MixingRecipe.TYPE)
                .map(holder -> (RecipeHolder<MixingRecipe>) holder)
                .filter(holder -> {
                    MixingRecipe recipe = holder.value();
                    for (SizedFluidIngredient required : recipe.fluids()) {
                        if (!hasFluidSatisfyingIngredient(required)) {
                            return false;
                        }
                    }
                    return true;
                })
                .findFirst()
                .orElse(null);
    }

    private boolean hasFluidSatisfyingIngredient(SizedFluidIngredient required) {
        int totalFound = 0;

        for (int i = 0; i < 4; i++) {
            FluidStack inTank = FluidUtil.getStack(fluidInventory, i);

            if (required.ingredient().test(inTank)) {
                totalFound += inTank.getAmount();
            }
        }

        return totalFound >= required.amount();
    }

    public boolean onPlayerUse(Player player, InteractionHand hand) {
        ItemStack stack = player.getItemInHand(hand);

        if (stack.getItem() instanceof FluidMoverItem) {
            return FluidMoverItem.onBlockInteract(stack, fluidInventory, filterFluidHandler, new int[]{4}, new int[]{0, 1, 2, 3});
        }

        try (Transaction tx = Transaction.open(null)) {
            boolean result = FluidUtil.interactWithFluidHandler(player, hand, this.worldPosition, fluidInventory, tx);
            if (result) {
                tx.commit();
            }
            return result;
        }
    }

    public FluidStacksResourceHandler getFluidHandler() {
        return fluidInventory;
    }

    public FilterFluidHandler getFilterFluidHandler() {
        return filterFluidHandler;
    }


    @Override
    public @Nullable AbstractContainerMenu createMenu(int container, @NonNull Inventory inventory, @NonNull Player player) {
        return new MixerMenu(container, inventory, this.worldPosition, data);
    }

    @Override
    public @NonNull Component getDisplayName() {
        return Component.translatable("block.casting.mixer");
    }

    @Override
    protected void saveAdditional(ValueOutput output) {

        fluidInventory.serialize(output.child("fluidInventory"));
        filterFluidHandler.serialize(output.child("filterFluids"));
        output.putInt("progress", progress);
        output.putInt("maxProgress", maxProgress);

        super.saveAdditional(output);
    }


    @Override
    protected void loadAdditional(ValueInput input) {

        fluidInventory.deserialize(input.childOrEmpty("fluidInventory"));
        filterFluidHandler.deserialize(input.childOrEmpty("filterFluids"));
        progress = input.getIntOr("progress", 0);
        maxProgress = input.getIntOr("maxProgress", CastingConfig.defaultMixerSpeed.get());

        super.loadAdditional(input);
    }

    @Override
    public void preRemoveSideEffects(@NonNull BlockPos pos, @NonNull BlockState state) {
    }

    @Override
    protected void collectImplicitComponents(DataComponentMap.@NonNull Builder builder) {
        super.collectImplicitComponents(builder);
        builder.set(CastingDataComponents.FLUIDS.get(), FluidListComponent.fromHandlers(fluidInventory));
    }

    @Override
    protected void applyImplicitComponents(@NonNull DataComponentGetter components) {
        super.applyImplicitComponents(components);
        FluidListComponent component = components.get(CastingDataComponents.FLUIDS.get());
        if (component != null) {
            System.out.println(component.fluids());
            component.applyToHandlers(fluidInventory);
        }
    }


    @Override
    public SyncableFluidHandler receivingHandler() {
        return fluidInventory;
    }

    @Override
    public int[] acceptingTanks() {
        return new int[] {0, 1, 2, 3};
    }

    @Override
    public SyncableFluidHandler fluidHandler() {
        return fluidInventory;
    }

    @Override
    public int[] sendingTanks() {
        return new int[] {4};
    }

    @Override
    public FilterFluidHandler getFilter() {
        return filterFluidHandler;
    }
}