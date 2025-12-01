package com.benbenlaw.casting.block.entity.multiblock;

import com.benbenlaw.casting.block.CastingBlocks;
import com.benbenlaw.casting.block.entity.CastingBlockEntities;
import com.benbenlaw.casting.block.multiblock.MultiblockControllerBlock;
import com.benbenlaw.casting.config.CastingConfig;
import com.benbenlaw.casting.fluid.FluidData;
import com.benbenlaw.casting.item.CastingDataComponents;
import com.benbenlaw.casting.item.util.FluidListComponent;
import com.benbenlaw.casting.multiblock.CoreMultiblockDetector;
import com.benbenlaw.casting.multiblock.MultiblockData;
import com.benbenlaw.casting.recipe.FuelRecipe;
import com.benbenlaw.casting.recipe.MeltingRecipe;
import com.benbenlaw.casting.screen.multiblock.MultiblockControllerMenu;
import com.benbenlaw.casting.util.CastingTags;
import com.benbenlaw.casting.util.FilteredItemHandler;
import com.benbenlaw.casting.util.MultiFluidTankSharedCapacity;
import com.benbenlaw.casting.util.SingleFluidTank;
import com.benbenlaw.core.block.entity.SyncableBlockEntity;
import com.benbenlaw.core.block.entity.handler.IInventoryHandlingBlockEntity;
import com.benbenlaw.core.block.entity.handler.InputOutputItemHandler;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.core.HolderLookup;
import net.minecraft.core.component.DataComponentMap;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.nbt.Tag;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.Containers;
import net.minecraft.world.MenuProvider;
import net.minecraft.world.SimpleContainer;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.ai.targeting.TargetingConditions;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.inventory.AbstractContainerMenu;
import net.minecraft.world.inventory.ContainerData;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.crafting.RecipeHolder;
import net.minecraft.world.level.block.entity.BlockEntity;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.block.state.properties.BlockStateProperties;
import net.minecraft.world.level.material.Fluid;
import net.minecraft.world.phys.AABB;
import net.neoforged.neoforge.fluids.FluidStack;
import net.neoforged.neoforge.fluids.capability.IFluidHandler;
import net.neoforged.neoforge.items.IItemHandler;
import net.neoforged.neoforge.items.ItemStackHandler;
import org.checkerframework.checker.units.qual.A;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.*;
import java.util.function.Consumer;
import java.util.function.Predicate;

import static com.benbenlaw.casting.block.multiblock.MultiblockSolidifierBlock.ENABLED;
import static com.benbenlaw.casting.block.multiblock.MultiblockSolidifierBlock.WORKING;
import static com.benbenlaw.casting.data.recipes.FluidStackHelper.getFluidStack;

public class MultiblockControllerBlockEntity extends SyncableBlockEntity implements MenuProvider, IInventoryHandlingBlockEntity {

    public final Map<Integer, Item> allowedItems = new HashMap<>();
    public final int EXPERIENCE_CREATED = CastingConfig.experienceGivenWhenMeltingAValidOre.get();

    public final FilteredItemHandler itemHandler = new FilteredItemHandler(CastingConfig.maxMultiblockControllerHeldItems.get(), allowedItems) {

        @Override
        protected void onContentsChanged(int slot) {
            setChanged();
            resetProgress(slot);
            sync();
        }

        @Override
        public int getSlotLimit(int slot) {
            return 1;
        }

        @Override
        protected int getStackLimit(int slot, ItemStack stack) {
            return 1;
        }
    };

    public void setAllowedItems(int slot, Item item) {
        if (item == null) {
            allowedItems.remove(slot);
        } else {
            allowedItems.put(slot, item);
        }
    }

    @Nullable
    public Item getAllowedItem(int slot) {
        return allowedItems.get(slot);
    }

    public int TOTAL_FLUID_TANK_CAPACITY = 1000000;

    public MultiFluidTankSharedCapacity fluidHandler = new MultiFluidTankSharedCapacity(TOTAL_FLUID_TANK_CAPACITY) {
        @Override
        protected void onContentsChanged() {
            setChanged();

        }
    };

    public IFluidHandler getFluidHandlerCapability(Direction side) {
        return fluidHandler;
    }

    public final ContainerData data;
    public int[] progress = new int[60];
    public int[] maxProgress = new int[60];
    public int CONTROLLER_MAX_PROGRESS = 1000;
    public String errorMessage = "";
    public int enabledSlots;
    private int tickCounter = 0;
    private int dropTimer = 0;
    private int fuelTemp = 0;
    public boolean structureValid;
    public boolean working;
    public MultiblockData cachedMultiblockData = null;
    public SingleFluidTank fuelTank;
    public SingleFluidTank coolantTank;
    public int regulatorCount = 0;
    public final IItemHandler controllerItemHandler = new InputOutputItemHandler(itemHandler,
            (i, stack) -> i < enabledSlots,
            i -> false
    );
    public final Map<Item, MeltingRecipe> recipeCache = new HashMap<>();
    public boolean recipeCacheInitialized = false;

    public Set<BlockPos> multiblockBlockPos = new HashSet<>();
    public boolean structureDirty = true;

    public @Nullable IItemHandler getItemHandlerCapability(@Nullable Direction side) {
        return controllerItemHandler;
    }


    @Override
    public void setHandler(ItemStackHandler itemStackHandler) {
        for (int i = 0; i < itemStackHandler.getSlots(); i++) {
            this.itemHandler.setStackInSlot(i, itemStackHandler.getStackInSlot(i));
        }
    }

    @Override
    public ItemStackHandler getItemStackHandler() {
        return itemHandler;
    }

    public MultiblockControllerBlockEntity(BlockPos pos, BlockState state) {
        super(CastingBlockEntities.MULTIBLOCK_CONTROLLER_BLOCK_ENTITY.get(), pos, state);

        this.fuelTank = new SingleFluidTank(1000);
        this.coolantTank = new SingleFluidTank(1000);

        this.data = new ContainerData() {
            public int get(int index) {
                if (index < 60) {
                    return MultiblockControllerBlockEntity.this.progress[index];
                } else if (index == 60) {
                    return MultiblockControllerBlockEntity.this.maxProgress[index - 60];
                } else {
                    return 0;
                }
            }

            public void set(int index, int value) {
                if (index < 60) {
                    MultiblockControllerBlockEntity.this.progress[index] = value;
                } else if (index == 60) {
                    MultiblockControllerBlockEntity.this.maxProgress[index - 60] = value;
                }
            }

            public int getCount() {
                return 61;
            }


        };
    }

    @Override
    public @NotNull Component getDisplayName() {
        return CastingBlocks.MULTIBLOCK_CONTROLLER.get().getName();
    }

    @Nullable
    @Override
    public AbstractContainerMenu createMenu(int container, @NotNull Inventory inventory, @NotNull Player player) {
        return new MultiblockControllerMenu(container, inventory, this.getBlockPos(), data);
    }

    public void drops() {
        SimpleContainer inventory = new SimpleContainer(itemHandler.getSlots());
        for (int i = 0; i < itemHandler.getSlots(); i++) {
            inventory.setItem(i, itemHandler.getStackInSlot(i));
        }
        assert this.level != null;
        Containers.dropContents(this.level, this.worldPosition, inventory);
    }

    public boolean isSlotEnabled(int index) {
        return index < enabledSlots;  // If index is less than enabledSlots, it's enabled
    }


    public void tick() {

        if (level.getGameTime() % CastingConfig.timeInTicksThatMultiblockControllerDoesAStructureCheck.get() == 0) {
            structureDirty = true;
        }

        //Validate multiblock structure if dirty
        if (structureDirty) {
            validateMultiblock();
            if (cachedMultiblockData != null) {

                multiblockBlockPos.clear();
                multiblockBlockPos.addAll(cachedMultiblockData.allBlockPositions());
            }
            structureDirty = false;
        }

        //Get fuelTank for client and server
        if (++tickCounter >= 20) {

            if (cachedMultiblockData != null) {
                findFuelTanks(cachedMultiblockData);
                findSolidifiers(cachedMultiblockData);
                findValves(cachedMultiblockData);
                findMixers(cachedMultiblockData);
                findRegulators(cachedMultiblockData);

                fluidHandler.setMaxFluidTypes(regulatorCount);
                fluidHandler.setRegulationEnabled(regulatorCount > 0);
                //System.out.println("Regulator count: " + regulatorCount);

                if (fluidHandler.getTankCapacity(1) != cachedMultiblockData.volume() * 1000) {
                    //System.out.println("Fluid handler capacity changed" + cachedMultiblockData.volume() * 1000);
                    fluidHandler.setEnabledCapacity(cachedMultiblockData.volume() * 1000);

                }
            } else {
                multiblockBlockPos.clear();
            }
        }

        assert level != null;
        if (!level.isClientSide()) {

            if (!recipeCacheInitialized) {
                updateRecipeCache();
                recipeCacheInitialized = true;
            }

            if (!this.getBlockState().getValue(ENABLED)) {
                level.setBlock(worldPosition, getBlockState().setValue(WORKING, false), 3);
                return;
            }

            updateWorkingState();
            sync();

            for (int i = 0; i < 60; i++) {
                if (!isSlotEnabled(i)) {
                    handleItemDrop(i);
                    continue;
                }

                if (cachedMultiblockData == null) {
                    resetProgress(i);
                    errorMessage = "multiblock_not_found";
                    continue;
                }

                ItemStack stack = itemHandler.getStackInSlot(i);

                if (stack.isEmpty()) {
                    errorMessage = "";
                    resetProgress(i);
                    continue;
                }

                processRecipeForItem(i, stack);
            }

            if (++tickCounter >= 20) {

                //Warning of players disabled until a good way of doing this is found
                if (cachedMultiblockData == null) {
                    structureValid = false;
                    if (++dropTimer >= 80) {
                        this.enabledSlots = 0;
                        dropTimer = 0;
                    }
                } else {
                    structureValid = true;
                    if (enabledSlots != cachedMultiblockData.volume()) {
                        this.enabledSlots = Math.min(cachedMultiblockData.volume(), 60);
                        dropTimer = 0;
                    }
                }
            }
        }
    }

    private void updateWorkingState() {

        working = false;
        for (int i = 0; i < enabledSlots; i++) {
            if (progress[i] > 0) {
                working = true;
                break;

            }
        }

        assert level != null;
        if (working) {
            level.setBlock(worldPosition, this.getBlockState().setValue(MultiblockControllerBlock.WORKING, true), 3);

        } else {
            level.setBlock(worldPosition, this.getBlockState().setValue(MultiblockControllerBlock.WORKING, false), 3);
        }

    }

    private void handleItemDrop(int slotIndex) {
        ItemStack stack = itemHandler.getStackInSlot(slotIndex);
        if (!stack.isEmpty()) {
            Direction facing = getBlockState().getValue(BlockStateProperties.HORIZONTAL_FACING);
            BlockPos dropPos = worldPosition.relative(facing);

            // Drop the item in front of the controller
            assert this.level != null;
            Containers.dropItemStack(this.level, dropPos.getX() + 0.5, dropPos.getY() + 0.5, dropPos.getZ() + 0.5, stack);
            resetProgress(slotIndex);
            itemHandler.setStackInSlot(slotIndex, ItemStack.EMPTY);
            sync();
        }
    }

    private void updateRecipeCache() {
        assert level != null;
        List<RecipeHolder<MeltingRecipe>> recipes = level.getRecipeManager().getAllRecipesFor(MeltingRecipe.Type.INSTANCE);
        recipeCache.clear();

        for (RecipeHolder<MeltingRecipe> recipeHolder : recipes) {
            MeltingRecipe recipe = recipeHolder.value();
            for (ItemStack itemStack : recipe.input().ingredient().getItems()) {
                Item inputItem = itemStack.getItem();
                recipeCache.put(inputItem, recipe);
            }
        }
    }

    private void processRecipeForItem(int slotIndex, ItemStack stack) {
        assert level != null;
        MeltingRecipe selectedRecipe = recipeCache.get(stack.getItem());

        if (selectedRecipe != null) {
            int fillAmount = selectedRecipe.output().getAmount();

            boolean producesExperience = selectedRecipe.input().ingredient().getItems()[0].is(CastingTags.Items.MELTING_PRODUCES_EXPERIENCE);

            if (selectedRecipe.input().ingredient().getItems()[0].is(CastingTags.Items.MELTING_OUTPUT_AMOUNT_EFFECTED)) {
                fillAmount = (int) (fillAmount * CastingConfig.oreMultiplier.get());
            }

            FluidStack fluidStack = new FluidStack(selectedRecipe.output().getFluid(), fillAmount);
            Fluid fluidString = BuiltInRegistries.FLUID.get(ResourceLocation.parse(CastingConfig.experienceFluid.get()));
            FluidStack experienceFluidStack = new FluidStack(fluidString, EXPERIENCE_CREATED);

            if (fluidHandler.fill(fluidStack, IFluidHandler.FluidAction.SIMULATE) < fillAmount) {
                resetProgress(slotIndex);
                errorMessage = "not_enough_space";
                return;
            }

            if (hasEnoughFuel(selectedRecipe.meltingTemp())) {
                maxProgress[slotIndex] = setNewMaxProgress();
                progress[slotIndex]++;
                level.setBlock(worldPosition, this.getBlockState().setValue(MultiblockControllerBlock.WORKING, true), 3);
                errorMessage = "";

                if (progress[slotIndex] >= maxProgress[slotIndex]) {
                    resetProgress(slotIndex);
                    fluidHandler.fill(fluidStack, IFluidHandler.FluidAction.EXECUTE);

                    if (producesExperience) {
                        fluidHandler.fill(experienceFluidStack, IFluidHandler.FluidAction.EXECUTE);
                    }

                    itemHandler.extractItem(slotIndex, 1, false);
                    useFuel();
                    setChanged();
                    sync();
                }
            } else {
                resetProgress(slotIndex);
            }
        } else {
            resetProgress(slotIndex);
        }
    }


    private boolean hasEnoughFuel(int temp) {

        fuelTemp = 0;

        if (fuelTank == null) {
            System.out.println("Fuel tank is null");
            return false;
        }
        if (level != null) {
            List<RecipeHolder<FuelRecipe>> allFuels = level.getRecipeManager().getAllRecipesFor(FuelRecipe.Type.INSTANCE);

            for (RecipeHolder<FuelRecipe> recipeHolder : allFuels) {
                FuelRecipe recipe = recipeHolder.value();
                if (recipe.fluid().getFluid() == fuelTank.getFluid().getFluid() && fuelTank.getFluidAmount() >= recipe.fluid().getAmount()) {
                    if (recipe.temp() >= temp) {
                        fuelTemp = recipe.temp();
                        return true;
                    } else {
                        errorMessage = "not_hot_enough";
                        return false;
                    }
                } else {
                    errorMessage = "no_fuel";
                }
            }
        }

        return false;
    }

    private int setNewMaxProgress() {

        if (fuelTank == null) {
            System.out.println("Fuel tank is null");
            return CONTROLLER_MAX_PROGRESS;
        }
        if (level != null) {
            List<RecipeHolder<FuelRecipe>> allFuels = level.getRecipeManager().getAllRecipesFor(FuelRecipe.Type.INSTANCE);

            for (RecipeHolder<FuelRecipe> recipeHolder : allFuels) {
                FuelRecipe recipe = recipeHolder.value();
                if (recipe.fluid().getFluid() == fuelTank.getFluid().getFluid()) {
                    return recipe.duration();
                }
            }
        }
        return CONTROLLER_MAX_PROGRESS;
    }

    private void useFuel() {
        if (fuelTank == null) {
            System.out.println("Fuel tank is null");
            return;
        }
        if (level != null) {
            List<RecipeHolder<FuelRecipe>> allFuels = level.getRecipeManager().getAllRecipesFor(FuelRecipe.Type.INSTANCE);

            for (RecipeHolder<FuelRecipe> recipeHolder : allFuels) {
                FuelRecipe recipe = recipeHolder.value();
                if (recipe.fluid().getFluid() == fuelTank.getFluid().getFluid()) {
                    if (fuelTank.getFluidAmount() >= recipe.fluid().getAmount()) {
                        fuelTank.drain(recipe.fluid().getAmount(), IFluidHandler.FluidAction.EXECUTE);

                    }
                }
            }
        }
    }

    private void identifyMultiblockExtraBlocks(MultiblockData data, Predicate<BlockState> filter, Consumer<BlockEntity> entityConsumer) {

        if (level == null) return;

        data.extraBlocks().forEach(pos -> {
            BlockEntity entity = level.getBlockEntity(pos);
            if (entity != null) {
                BlockState blockState = level.getBlockState(pos);

                if (filter.test(blockState)) {
                    entityConsumer.accept(entity);
                }

            }
        });

    }

    public final List<IItemHandler> solidifierItemHandlers = new ArrayList<>();

    public void findSolidifiers(MultiblockData cachedMultiblockData) {

        solidifierItemHandlers.clear();

        identifyMultiblockExtraBlocks(cachedMultiblockData, state -> state.is(CastingBlocks.MULTIBLOCK_SOLIDIFIER.get()),
                entity -> {
                    if (entity instanceof MultiblockSolidifierBlockEntity solidifier) {
                        solidifier.setControllerBlockEntity(this);
                        solidifier.setControllerPos(this.worldPosition);
                        solidifier.setCoolantTank(coolantTank);
                        solidifierItemHandlers.add(solidifier.getItemStackHandler());
                    }
                });
    }

    public void findValves(MultiblockData cachedMultiblockData) {
        identifyMultiblockExtraBlocks(cachedMultiblockData, state -> state.is(CastingBlocks.MULTIBLOCK_VALVE.get()),
                entity -> {
                    if (entity instanceof MultiblockValveBlockEntity valve) {
                        valve.setControllerBlockEntity(this);
                        valve.setControllerPos(this.worldPosition);
                    }
                });
    }

    public void findMixers(MultiblockData cachedMultiblockData) {
        identifyMultiblockExtraBlocks(cachedMultiblockData, state -> state.is(CastingBlocks.MULTIBLOCK_MIXER.get()),
                entity -> {
                    if (entity instanceof MultiblockMixerBlockEntity mixer) {
                        mixer.setControllerBlockEntity(this);
                        mixer.setControllerPos(this.worldPosition);
                    }
                });
    }

    public void findRegulators(MultiblockData cachedMultiblockData) {

        regulatorCount = 0;

        cachedMultiblockData.extraBlocks().stream()
                .filter(pos -> level.getBlockState(pos).is(CastingBlocks.MULTIBLOCK_REGULATOR.get()))
                .forEach(pos -> {
                    regulatorCount += 1;
                });
    }


    public void findFuelTanks(MultiblockData cachedMultiblockData) {
        fuelTank = null;
        coolantTank = null;

        identifyMultiblockExtraBlocks(cachedMultiblockData, state -> true,
                entity -> {
                    if (entity instanceof MultiblockFuelTankBlockEntity fuelTankEntity) {
                        fuelTank = fuelTankEntity.fluidHandler;
                    } else if (entity instanceof MultiblockCoolantTankBlockEntity coolantTankEntity) {
                        coolantTank = coolantTankEntity.fluidHandler;
                    }
                });

    }

    private void validateMultiblock() {
        assert level != null;
        cachedMultiblockData = CoreMultiblockDetector.findMultiblock(level, worldPosition, this.getBlockState().getBlock(), wallState -> wallState.is(CastingTags.Blocks.CONTROLLER_WALLS),
                floorState -> floorState.is(CastingTags.Blocks.CONTROLLER_FLOORS), extraValidBlocks -> extraValidBlocks.is(CastingTags.Blocks.CONTROLLER_EXTRA_BLOCKS), true, true, 1024, 128, 64);

    }

    private void resetProgress(int slot) {
        progress[slot] = 0;
        maxProgress[slot] = 0;
    }

    @Override
    protected void saveAdditional(@NotNull CompoundTag compoundTag, HolderLookup.@NotNull Provider provider) {
        super.saveAdditional(compoundTag, provider);
        compoundTag.put("inventory", this.itemHandler.serializeNBT(provider));
        compoundTag.putIntArray("progress", progress);
        compoundTag.putIntArray("maxProgress", maxProgress);
        compoundTag.putBoolean("structureValid", structureValid);
        compoundTag.putInt("enabledSlots", enabledSlots);
        compoundTag.put("fluidTank", fluidHandler.writeToNBT(provider));
        compoundTag.putString("errorMessage", errorMessage);
        compoundTag.putInt("regulatorCount", regulatorCount);


        if (fuelTank != null) {
            compoundTag.put("fuelTank", fuelTank.writeToNBT(provider, new CompoundTag()));
            compoundTag.putInt("fuelTemp", fuelTemp);
        }

        if (coolantTank != null) {
            compoundTag.put("coolantTank", coolantTank.writeToNBT(provider, new CompoundTag()));
        }

        if (cachedMultiblockData != null) {
            compoundTag.put("multiblockData", cachedMultiblockData.serializeNBT(provider));
        }

        ListTag allowedItemsList = new ListTag();
        for (Map.Entry<Integer, Item> entry : allowedItems.entrySet()) {
            CompoundTag entryTag = new CompoundTag();
            entryTag.putInt("slot", entry.getKey());
            ResourceLocation itemID = BuiltInRegistries.ITEM.getKey(entry.getValue());
            entryTag.putString("item", itemID.toString());
            allowedItemsList.add(entryTag);
        }
        compoundTag.put("allowedItems", allowedItemsList);

    }

    @Override
    protected void loadAdditional(CompoundTag compoundTag, HolderLookup.@NotNull Provider provider) {
        this.itemHandler.deserializeNBT(provider, compoundTag.getCompound("inventory"));
        progress = compoundTag.getIntArray("progress");
        maxProgress = compoundTag.getIntArray("maxProgress");
        structureValid = compoundTag.getBoolean("structureValid");
        enabledSlots = compoundTag.getInt("enabledSlots");
        fluidHandler.readFromNBT(provider, compoundTag.get("fluidTank"));
        errorMessage = compoundTag.getString("errorMessage");
        regulatorCount = compoundTag.getInt("regulatorCount");

        if (compoundTag.contains("fuelTank") && fuelTank != null) {
            fuelTank.readFromNBT(provider, compoundTag.getCompound("fuelTank"));
            fuelTemp = compoundTag.getInt("fuelTemp");
        }

        if (compoundTag.contains("coolantTank") && coolantTank != null) {
            coolantTank.readFromNBT(provider, compoundTag.getCompound("coolantTank"));
        }

        if (compoundTag.contains("multiblockData") && cachedMultiblockData != null) {
            cachedMultiblockData.deserializeNBT(provider, Objects.requireNonNull(compoundTag.get("multiblockData")));
        }

        allowedItems.clear();
        if (compoundTag.contains("allowedItems")) {
            ListTag allowedItemsList = compoundTag.getList("allowedItems", Tag.TAG_COMPOUND);
            for (int i = 0; i < allowedItemsList.size(); i++) {
                CompoundTag entryTag = allowedItemsList.getCompound(i);
                int slot = entryTag.getInt("slot");
                ResourceLocation itemID = ResourceLocation.parse(entryTag.getString("item"));
                Item item = BuiltInRegistries.ITEM.get(itemID);
                allowedItems.put(slot, item);
            }
        }

        super.loadAdditional(compoundTag, provider);
    }

    @Override
    protected void collectImplicitComponents(DataComponentMap.Builder builder) {
        super.collectImplicitComponents(builder);

        List<FluidStack> fluids = this.fluidHandler.getFluids();
        if (!fluids.isEmpty()) {
            builder.set(CastingDataComponents.FLUIDS, new FluidListComponent(fluids));
        }
    }

    @Override
    protected void applyImplicitComponents(DataComponentInput input) {
        super.applyImplicitComponents(input);
        FluidListComponent component = input.get(CastingDataComponents.FLUIDS);
        if (component != null && !component.fluids().isEmpty()) {
            this.fluidHandler.setFluids(component.fluids());
        }
    }

}




