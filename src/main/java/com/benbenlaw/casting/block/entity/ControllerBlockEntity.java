package com.benbenlaw.casting.block.entity;

import com.benbenlaw.casting.block.CastingBlockEntities;
import com.benbenlaw.casting.block.custom.CastingBlock;
import com.benbenlaw.casting.block.custom.ControllerBlock;
import com.benbenlaw.casting.config.CastingConfig;
import com.benbenlaw.casting.fluid.CastingFluids;
import com.benbenlaw.casting.item.CastingDataComponents;
import com.benbenlaw.casting.item.FluidMoverItem;
import com.benbenlaw.casting.item.util.FluidListComponent;
import com.benbenlaw.casting.recipe.MeltingRecipeInput;
import com.benbenlaw.casting.recipe.custom.MeltingRecipe;
import com.benbenlaw.casting.screen.ControllerMenu;
import com.benbenlaw.core.block.entity.SyncableBlockEntity;
import com.benbenlaw.core.block.entity.handler.fluid.OutputFluidHandler;
import com.benbenlaw.core.block.entity.handler.fluid.SyncableFluidHandler;
import com.benbenlaw.core.block.entity.handler.item.InputItemHandler;
import com.benbenlaw.core.block.entity.handler.item.SyncableItemHandler;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.core.NonNullList;
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
import net.minecraft.world.item.crafting.CraftingInput;
import net.minecraft.world.item.crafting.CraftingRecipe;
import net.minecraft.world.item.crafting.Ingredient;
import net.minecraft.world.item.crafting.RecipeHolder;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.entity.BlockEntity;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.storage.ValueInput;
import net.minecraft.world.level.storage.ValueOutput;
import net.neoforged.neoforge.common.crafting.IngredientType;
import net.neoforged.neoforge.fluids.FluidStack;
import net.neoforged.neoforge.fluids.FluidStackTemplate;
import net.neoforged.neoforge.transfer.ResourceHandler;
import net.neoforged.neoforge.transfer.fluid.FluidResource;
import net.neoforged.neoforge.transfer.fluid.FluidStacksResourceHandler;
import net.neoforged.neoforge.transfer.fluid.FluidUtil;
import net.neoforged.neoforge.transfer.item.ItemResource;
import net.neoforged.neoforge.transfer.item.ItemStackResourceHandler;
import net.neoforged.neoforge.transfer.item.ItemStacksResourceHandler;
import net.neoforged.neoforge.transfer.item.ItemUtil;
import net.neoforged.neoforge.transfer.transaction.Transaction;
import org.jetbrains.annotations.Nullable;
import org.jspecify.annotations.NonNull;

import java.util.ArrayList;
import java.util.List;
import java.util.OptionalInt;

public class ControllerBlockEntity extends SyncableBlockEntity implements MenuProvider, FluidSending {

    private final ContainerData data;

    private int[] progress = new int[15];
    private int[] maxProgress = new int[15];
    private OptionalInt temperature = OptionalInt.empty();

    private final SyncableItemHandler inventory = new SyncableItemHandler(this, 15,
            (i, stack) -> i >= 0 && i <= 14, i -> i == 15);

    private final SyncableFluidHandler fluidInventory = new SyncableFluidHandler(this, 4, 8000,
            (i, stack) -> false,
            i -> i >= 0 && i <= 3);

    public ControllerBlockEntity(BlockPos pos, BlockState state) {
        super(CastingBlockEntities.CONTROLLER_BLOCK_ENTITY.get(), pos, state);

        this.data = new ContainerData() {
            @Override
            public int get(int index) {
                if (index < 15) return progress[index];
                if (index < 30) return maxProgress[index - 15];
                if (index == 30) return temperature.orElse(Integer.MIN_VALUE);
                return 0;
            }

            @Override
            public void set(int index, int value) {
                if (index < 15) progress[index] = value;
                else if (index < 30) maxProgress[index - 15] = value;
                else if (index == 30) {
                    temperature = (value == Integer.MIN_VALUE) ? OptionalInt.empty() : OptionalInt.of(value);
                }
            }

            @Override
            public int getCount() {
                return 31;
            }
        };
    }

    public void tick() {
        if (level == null || level.isClientSide()) return;

        boolean isRunning = level.getBlockState(worldPosition).getValue(ControllerBlock.RUNNING);

        TankBlockEntity activeFuelTank = getActiveFuelTank(level, worldPosition);
        this.temperature = activeFuelTank != null ? activeFuelTank.getFuelTemp() : OptionalInt.empty();

        if (!isRunning || temperature.isEmpty()) {
            updateWorkingState(false);
            return;
        }


        int currentTemp = temperature.getAsInt();
        boolean changed = false;
        boolean isWorking = false;

        for (int i = 0; i < 15; i++) {
            ItemStack stack = ItemUtil.getStack(inventory, i);

            if (stack.isEmpty()) {
                if (progress[i] > 0) {
                    progress[i] = 0;
                    changed = true;
                }
                continue;
            }

            RecipeHolder<MeltingRecipe> recipeHolder = getRecipeForSlot(stack);

            if (recipeHolder != null) {
                MeltingRecipe recipe = recipeHolder.value();

                if (currentTemp >= recipe.meltingTemp()) {

                    int max = CastingConfig.defaultControllerSpeed.get();
                    int tempDiff = currentTemp - recipe.meltingTemp();
                    max -= (tempDiff / 100) * 10;
                    if (recipe.durationModifier().isPresent()) {
                        max = (int) (max * recipe.durationModifier().get());
                    }
                    max = Math.max(max, 20);
                    maxProgress[i] = max;

                    if (canFitFluids(recipe.output())) {
                        isWorking = true;
                        progress[i]++;
                        changed = true;

                        if (progress[i] >= maxProgress[i]) {
                            executeMelting(i, recipe, activeFuelTank);
                            progress[i] = 0;
                        }
                    }
                } else if (progress[i] > 0) {
                    progress[i] = 0;
                    changed = true;
                }
            }
        }

        updateWorkingState(isWorking);
        this.tickResourceSending(level, worldPosition);
        if (changed) {
            setChanged();
            sync();
        }
    }

    public static @Nullable TankBlockEntity getActiveFuelTank(Level level, BlockPos pos) {
        if (level == null) return null;
        for (var dir : Direction.values()) {
            BlockEntity neighbor = level.getBlockEntity(pos.relative(dir));
            if (neighbor instanceof TankBlockEntity tank) {
                if (!tank.getFluidHandler().getResource(0).isEmpty()) {
                    return tank;
                }
            }
        }
        return null;
    }

    private void executeMelting(int slot, MeltingRecipe recipe, TankBlockEntity fuelTank) {
        FluidStack fuelStack = FluidUtil.getStack(fuelTank.getFluidHandler(),0);

        if (fuelStack.isEmpty()) return;

        var fuelRecipe = TankBlockEntity.getFuel(level, fuelStack);
        if (fuelRecipe == null) return;

        int amountToConsume = fuelRecipe.value().fluid().amount();


        try (Transaction tx = Transaction.open(null)) {
            inventory.runInternal(() -> {
                        inventory.extract(slot, ItemResource.of(inventory.getResource(slot).toStack()), recipe.input().count(), tx);
            });

            fuelTank.getFluidHandler().extract(0, fuelTank.getFluidHandler().getResource(0), amountToConsume, tx);

            fluidInventory.runInternal(() -> {
                for (FluidStackTemplate fluid : recipe.output()) {
                    int remaining = fluid.amount();
                    for (int tank = 0; tank < 4 && remaining > 0; tank++) {
                        remaining -= fluidInventory.insert(tank, FluidResource.of(fluid), remaining, tx);
                    }
                }
            });

            tx.commit();
        }
        sync();
    }

    private void updateWorkingState(boolean working) {
        assert level != null;
        BlockState state = level.getBlockState(worldPosition);
        if (state.getValue(CastingBlock.WORKING) != working) {
            level.setBlock(worldPosition, state.setValue(CastingBlock.WORKING, working), 3);
        }
    }

    private boolean canFitFluids(List<FluidStackTemplate> outputs) {
        return fluidInventory.runInternal(() -> {
            for (FluidStackTemplate fluidToInsert : outputs) {
                int needed = fluidToInsert.amount();
                int tankToUse = -1;

                for (int i = 0; i < 4; i++) {
                    FluidStack existing = FluidUtil.getStack(fluidInventory, i);
                    if (!existing.isEmpty() && FluidStack.isSameFluidSameComponents(existing, fluidToInsert)) {
                        tankToUse = i;
                        break;
                    }
                }

                if (tankToUse == -1) {
                    for (int i = 0; i < 4; i++) {
                        if (FluidUtil.getStack(fluidInventory, i).isEmpty()) {
                            tankToUse = i;
                            break;
                        }
                    }
                }

                if (tankToUse != -1) {
                    FluidStack current = FluidUtil.getStack(fluidInventory, tankToUse);
                    int capacity = fluidInventory.getCapacityAsInt(tankToUse, FluidResource.of(fluidToInsert));
                    if (current.getAmount() + needed > capacity) return false;
                } else return false;
            }
            return true;
        });
    }

    public boolean onPlayerUse(Player player, InteractionHand hand) {
        ItemStack stack = player.getItemInHand(hand);

        if (stack.getItem() instanceof FluidMoverItem) {
            return FluidMoverItem.onBlockInteract(stack, fluidInventory, new int[]{0, 1, 2, 3}, new int[]{});
        }

        try (Transaction tx = Transaction.open(null)) {
            boolean result = FluidUtil.interactWithFluidHandler(player, hand, this.worldPosition, fluidInventory, tx);
            if (result) {
                tx.commit();
            }
            return result;
        }
    }

    private RecipeHolder<MeltingRecipe> getRecipeForSlot(ItemStack stack) {
        if (level == null || level.getServer() == null || stack.isEmpty()) return null;
        return level.getServer().getRecipeManager().recipeMap().values().stream()
                .filter(holder -> holder.value().getType() == MeltingRecipe.TYPE)
                .map(holder -> (RecipeHolder<MeltingRecipe>) holder)
                .filter(holder -> holder.value().input().test(stack))
                .findFirst().orElse(null);
    }

    @Override
    protected void saveAdditional(ValueOutput output) {
        inventory.serialize(output.child("inventory"));
        fluidInventory.serialize(output.child("fluidInventory"));
        output.putIntArray("progress", progress);
        output.putIntArray("maxProgress", maxProgress);

        output.putInt("temperature", temperature.orElse(Integer.MIN_VALUE));

        super.saveAdditional(output);
    }

    @Override
    protected void loadAdditional(ValueInput input) {
        inventory.deserialize(input.childOrEmpty("inventory"));
        fluidInventory.deserialize(input.childOrEmpty("fluidInventory"));
        this.progress = input.getIntArray("progress").orElse(new int[15]);
        this.maxProgress = input.getIntArray("maxProgress").orElse(new int[15]);

        int tempVal = input.getIntOr("temperature", Integer.MIN_VALUE);
        this.temperature = (tempVal == Integer.MIN_VALUE) ? OptionalInt.empty() : OptionalInt.of(tempVal);

        super.loadAdditional(input);
    }

    public ItemStacksResourceHandler getItemHandler() {
        return inventory;
    }

    public FluidStacksResourceHandler getFluidHandler() {
        return fluidInventory;
    }

    @Override public @Nullable AbstractContainerMenu createMenu(int id, @NonNull Inventory inv, @NonNull Player player) { return new ControllerMenu(id, inv, this.worldPosition, data); }
    @Override public @NonNull Component getDisplayName() { return Component.translatable("block.casting.controller"); }
    @Override public void preRemoveSideEffects(@NonNull BlockPos pos, @NonNull BlockState state) { dropInventoryContents(inventory); }

    @Override
    protected void collectImplicitComponents(DataComponentMap.@NonNull Builder builder) {
        super.collectImplicitComponents(builder);
        builder.set(CastingDataComponents.FLUIDS.get(), FluidListComponent.fromHandlers(fluidInventory));
    }

    @Override
    protected void applyImplicitComponents(@NonNull DataComponentGetter components) {
        super.applyImplicitComponents(components);
        FluidListComponent component = components.get(CastingDataComponents.FLUIDS.get());
        if (component != null) component.applyToHandlers(fluidInventory);
    }

    @Override
    public SyncableFluidHandler fluidHandler() {
        return fluidInventory;
    }

    @Override
    public int[] sendingTanks() {
        return new int[] {0, 1, 2, 3};
    }
}