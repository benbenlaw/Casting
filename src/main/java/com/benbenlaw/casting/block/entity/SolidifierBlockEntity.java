package com.benbenlaw.casting.block.entity;

import com.benbenlaw.casting.item.CastingDataComponents;
import com.benbenlaw.casting.recipe.CoolantRecipe;
import com.benbenlaw.casting.recipe.SolidifierRecipe;
import com.benbenlaw.casting.screen.SolidifierMenu;
import com.benbenlaw.casting.util.CastingTags;
import com.benbenlaw.core.block.entity.handler.IInventoryHandlingBlockEntity;
import com.benbenlaw.core.block.entity.handler.InputOutputItemHandler;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.core.HolderLookup;
import net.minecraft.core.component.DataComponentMap;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.Connection;
import net.minecraft.network.chat.Component;
import net.minecraft.network.protocol.Packet;
import net.minecraft.network.protocol.game.ClientGamePacketListener;
import net.minecraft.network.protocol.game.ClientboundBlockEntityDataPacket;
import net.minecraft.server.level.ServerChunkCache;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.Containers;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.MenuProvider;
import net.minecraft.world.SimpleContainer;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.inventory.AbstractContainerMenu;
import net.minecraft.world.inventory.ContainerData;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.crafting.Ingredient;
import net.minecraft.world.item.crafting.RecipeHolder;
import net.minecraft.world.item.crafting.RecipeInput;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.entity.BlockEntity;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.chunk.LevelChunk;
import net.neoforged.neoforge.common.crafting.SizedIngredient;
import net.neoforged.neoforge.fluids.FluidStack;
import net.neoforged.neoforge.fluids.FluidUtil;
import net.neoforged.neoforge.fluids.capability.IFluidHandler;
import net.neoforged.neoforge.fluids.capability.templates.FluidTank;
import net.neoforged.neoforge.items.IItemHandler;
import net.neoforged.neoforge.items.ItemStackHandler;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;
import java.util.Objects;

public class SolidifierBlockEntity extends BlockEntity implements MenuProvider, IInventoryHandlingBlockEntity {

    private final ItemStackHandler itemHandler = new ItemStackHandler(2) {
        @Override
        protected void onContentsChanged(int slot) {
            setChanged();
            sync();
        }

        @Override
        protected int getStackLimit(int slot, ItemStack stack) {
            if(slot == 0 && stack.is(CastingTags.Items.MOLDS)) {
                return 1;
            }
            return 64;
        }
    };

    public final FluidTank TANK = new FluidTank(16000) {
        @Override
        protected void onContentsChanged() {
            setChanged();
            sync();
        }
    };

    private final IFluidHandler fluidHandler = new IFluidHandler() {
        @Override
        public int getTanks() {
            return 1;
        }

        @Override
        public FluidStack getFluidInTank(int tank) {
            return TANK.getFluid();
        }

        @Override
        public int getTankCapacity(int tank) {
            return TANK.getCapacity();
        }

        @Override
        public boolean isFluidValid(int tank, FluidStack stack) {
            return TANK.isFluidValid(stack);
        }


        @Override
        public int fill(FluidStack resource, FluidAction action) {
            if (resource.getFluid() == TANK.getFluid().getFluid() || TANK.isEmpty()) {
                return TANK.fill(resource, action);
            }
            return 0;
        }

        @Override
        public FluidStack drain(FluidStack resource, FluidAction action) {
            if (resource.getFluid() == TANK.getFluid().getFluid()) {
                return TANK.drain(resource.getAmount(), action);
            }
            return FluidStack.EMPTY;
        }

        @Override
        public FluidStack drain(int maxDrain, FluidAction action) {
            if (TANK.getFluidAmount() > 0) {
                return TANK.drain(maxDrain, action);
            }
            return FluidStack.EMPTY;
        }
    };

    public boolean onPlayerUse(Player player, InteractionHand hand) {
        return FluidUtil.interactWithFluidHandler(player, hand, TANK);
    }

    public IFluidHandler getFluidHandlerCapability(Direction side) {
        return fluidHandler;
    }
    public void sync() {
        if (level instanceof ServerLevel serverLevel) {
            LevelChunk chunk = serverLevel.getChunkAt(getBlockPos());
            if (Objects.requireNonNull(chunk.getLevel()).getChunkSource() instanceof ServerChunkCache chunkCache) {
                chunkCache.chunkMap.getPlayers(chunk.getPos(), false).forEach(this::syncContents);
            }
        }
    }

    public void setFluid(FluidStack stack) {
        this.TANK.setFluid(stack);
    }

    public void getFluid(FluidStack stack) {
        TANK.setFluid(stack);
    }

    public FluidStack getFluidStack() {
        return this.TANK.getFluid();
    }

    public void syncContents(ServerPlayer player) {
        player.connection.send(Objects.requireNonNull(getUpdatePacket()));
    }

    public final ContainerData data;
    public int progress = 0;
    public int maxProgress;
    public int fuelTemp = 0;
    public int storedTankFluidAmount = 0;
    public int storedTankFluidAmountUsedInRecipe = 0;
    public boolean isLimitMode = false;
    private final IItemHandler solidifierItemHandler = new InputOutputItemHandler(itemHandler,
            (i, stack) -> i == 0 ,  //
            i -> i == 1
    );

    public RecipeHolder<SolidifierRecipe> cachedRecipe = null;

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

    public SolidifierBlockEntity(BlockPos pos, BlockState state) {
        super(CastingBlockEntities.SOLIDIFIER_BLOCK_ENTITY.get(), pos, state);
        this.data = new ContainerData() {
            public int get(int index) {
                return switch (index) {
                    case 0 -> SolidifierBlockEntity.this.progress;
                    case 1 -> SolidifierBlockEntity.this.maxProgress;
                    default -> 0;
                };
            }

            public void set(int index, int value) {
                switch (index) {
                    case 0 -> SolidifierBlockEntity.this.progress = value;
                    case 1 -> SolidifierBlockEntity.this.maxProgress = value;
                }
            }

            public int getCount() {
                return 2;
            }
        };
    }

    private boolean isRecipeStillValid(@NotNull RecipeHolder<SolidifierRecipe> holder) {
        SolidifierRecipe recipe = holder.value();
        return recipe.mold().test(itemHandler.getStackInSlot(0))
                && hasEnoughFluid(recipe.fluid())
                && hasCorrectInputAmount(recipe.mold())
                && hasOutputSpaceMaking(this, recipe);
    }

    private void handleRecipeTick(SolidifierRecipe recipe) {
        FluidStack output = recipe.fluid();
        progress++;

        if (progress >= maxProgress) {
            extractFluid(output, output.getAmount());

            if (!itemHandler.getStackInSlot(0).is(CastingTags.Items.MOLDS)) {
                itemHandler.getStackInSlot(0).shrink(recipe.mold().count());
            }

            ItemStack currentOutput = itemHandler.getStackInSlot(1);
            ItemStack result = new ItemStack(recipe.output().getItems()[0].getItem(), recipe.output().count());
            if (currentOutput.isEmpty()) {
                itemHandler.setStackInSlot(1, result);
            } else {
                currentOutput.grow(result.getCount());
            }

            setChanged();
            useFuel(this);
            resetProgress();
            sync();
        }
    }


    @Override
    public Component getDisplayName() {
        return Component.translatable("block.casting.solidifier");
    }


    @Nullable
    @Override
    public AbstractContainerMenu createMenu(int container, @NotNull Inventory inventory, @NotNull Player player) {
        return new SolidifierMenu(container, inventory, this.getBlockPos(), data);
    }

    @Override
    public void onLoad() {
        super.onLoad();
        this.setChanged();
    }

    @Nullable
    public Packet<ClientGamePacketListener> getUpdatePacket() {
        return ClientboundBlockEntityDataPacket.create(this);
    }

    @Override
    public void handleUpdateTag(@NotNull CompoundTag compoundTag, HolderLookup.@NotNull Provider provider) {
        super.loadAdditional(compoundTag, provider);
    }

    @Override
    public @NotNull CompoundTag getUpdateTag(HolderLookup.@NotNull Provider provider) {
        CompoundTag compoundTag = new CompoundTag();
        saveAdditional(compoundTag, provider);
        return compoundTag;
    }

    @Override
    public void onDataPacket(@NotNull Connection connection, @NotNull ClientboundBlockEntityDataPacket clientboundBlockEntityDataPacket,
                             HolderLookup.@NotNull Provider provider) {
        super.onDataPacket(connection, clientboundBlockEntityDataPacket, provider);
    }

    @Override
    protected void saveAdditional(@NotNull CompoundTag compoundTag, HolderLookup.@NotNull Provider provider) {
        super.saveAdditional(compoundTag, provider);
        compoundTag.put("inventory", this.itemHandler.serializeNBT(provider));
        compoundTag.putInt("progress", progress);
        compoundTag.putInt("maxProgress", maxProgress);
        compoundTag.put("tank", TANK.writeToNBT(provider, new CompoundTag()));
        compoundTag.putInt("fuelTemp", fuelTemp);
        compoundTag.putInt("storedTankFluidAmount", storedTankFluidAmount);
        compoundTag.putInt("storedTankFluidAmountUsedInRecipe", storedTankFluidAmountUsedInRecipe);
        compoundTag.putBoolean("limitMode", isLimitMode);

    }

    @Override
    protected void loadAdditional(CompoundTag compoundTag, HolderLookup.@NotNull Provider provider) {
        this.itemHandler.deserializeNBT(provider, compoundTag.getCompound("inventory"));
        progress = compoundTag.getInt("progress");
        maxProgress = compoundTag.getInt("maxProgress");
        TANK.readFromNBT(provider, compoundTag.getCompound("tank"));
        fuelTemp = compoundTag.getInt("fuelTemp");
        storedTankFluidAmount = compoundTag.getInt("storedTankFluidAmount");
        storedTankFluidAmountUsedInRecipe = compoundTag.getInt("storedTankFluidAmountUsedInRecipe");
        isLimitMode = compoundTag.getBoolean("limitMode");
        super.loadAdditional(compoundTag, provider);
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
        if (level == null || level.isClientSide()) return;

        sync();
        fuelInformation(level.getBlockEntity(this.worldPosition));

        if (itemHandler.getStackInSlot(0).isEmpty()) {
            resetProgress();
            cachedRecipe = null;
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

        // === Use cache if valid ===
        if (cachedRecipe != null && isRecipeStillValid(cachedRecipe)) {
            handleRecipeTick(cachedRecipe.value());
            return;
        }

        // === Else, search for recipe ===
        for (RecipeHolder<SolidifierRecipe> recipeHolder :
                level.getRecipeManager().getRecipesFor(SolidifierRecipe.Type.INSTANCE, inventory, level)) {
            if (isRecipeStillValid(recipeHolder)) {
                cachedRecipe = recipeHolder;
                handleRecipeTick(recipeHolder.value());
                return;
            }
        }

        // No match found
        resetProgress();
        cachedRecipe = null;
    }

    private boolean hasCorrectInputAmount(SizedIngredient mold) {
        return itemHandler.getStackInSlot(0).getCount() >= mold.count();
    }


    private void resetProgress() {
        progress = 0;
        sync();
    }

    private void extractFluid(FluidStack output, int amount) {
        if (TANK.getFluidAmount() >= amount && TANK.getFluid().getFluid() == output.getFluid()) {
            TANK.drain(amount, IFluidHandler.FluidAction.EXECUTE);
        }
    }


    private boolean hasEnoughFluid(FluidStack output) {
        int tankAmount = TANK.getFluidAmount();
        if (isLimitMode) {
            tankAmount = tankAmount - 100;
        }
        return tankAmount >= output.getAmount() && TANK.getFluid().getFluid() == output.getFluid();
    }

    private boolean isRecipeSlotsValidForTanks(SolidifierRecipe recipe) {
        FluidStack recipeFluid = recipe.fluid();
        return TANK.getFluid().is(recipeFluid.getFluidType()) && (tankIsValidForSlot(recipeFluid, 0) || tankIsValidForSlot(recipeFluid, 1));
    }

    private boolean tankIsValidForSlot(FluidStack stack, int slot) {

        return stack.getFluid() == TANK.getFluid().getFluid();
    }

    private boolean hasOutputSpaceMaking(SolidifierBlockEntity entity, SolidifierRecipe recipe) {
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

    private void fuelInformation(BlockEntity entity) {
        if (entity == null) {
            return;
        }
        Level level = entity.getLevel();
        if (level != null && !level.isClientSide()) {
            boolean foundFuel = false;
            for (Direction direction : Direction.values()) {
                BlockEntity adjacentEntity = level.getBlockEntity(entity.getBlockPos().relative(direction));

                if (adjacentEntity instanceof TankBlockEntity tankBlockEntity) {
                    List<RecipeHolder<CoolantRecipe>> allFuels = level.getRecipeManager().getAllRecipesFor(CoolantRecipe.Type.INSTANCE);

                    for (RecipeHolder<CoolantRecipe> recipeHolder : allFuels) {
                        CoolantRecipe recipe = recipeHolder.value();
                        if (recipe.fluid().getFluid() == tankBlockEntity.FLUID_TANK.getFluid().getFluid() &&
                                tankBlockEntity.FLUID_TANK.getFluidAmount() >= recipe.fluid().getAmount()) {
                            maxProgress = recipe.duration();
                            storedTankFluidAmount = tankBlockEntity.FLUID_TANK.getFluidAmount();
                            storedTankFluidAmountUsedInRecipe = recipe.fluid().getAmount();
                            foundFuel = true;
                            break;
                        }
                    }

                    if (foundFuel) {
                        break;
                    }
                }
            }
            if (!foundFuel) {
                maxProgress = 220;  // Default max progress if no fuel found
                fuelTemp = 1000;  // Set the default temperature
            }
        }
    }


    private void useFuel(BlockEntity entity) {
        if (entity == null) {
            return;
        }
        Level level = entity.getLevel();
        if (level != null) {
            for (Direction direction : Direction.values()) {
                BlockEntity adjacentEntity = level.getBlockEntity(entity.getBlockPos().relative(direction));
                if (adjacentEntity instanceof TankBlockEntity tankBlockEntity) {
                    List<RecipeHolder<CoolantRecipe>> allFuels = level.getRecipeManager().getAllRecipesFor(CoolantRecipe.Type.INSTANCE);

                    for (RecipeHolder<CoolantRecipe> recipeHolder : allFuels) {
                        CoolantRecipe recipe = recipeHolder.value();
                        if (recipe.fluid().getFluid() == tankBlockEntity.FLUID_TANK.getFluid().getFluid()) {
                            if (tankBlockEntity.FLUID_TANK.getFluidAmount() >= recipe.fluid().getAmount()) {
                                tankBlockEntity.FLUID_TANK.drain(recipe.fluid().getAmount(), IFluidHandler.FluidAction.EXECUTE);
                            }
                        }
                    }
                }
            }
        }
    }

    public boolean getLimitMode() {
        return isLimitMode;
    }

    public boolean toggleLimitMode() {
        isLimitMode = !isLimitMode;
        return isLimitMode;
    }

    @Override
    protected void collectImplicitComponents(DataComponentMap.Builder builder) {
        super.collectImplicitComponents(builder);

        FluidStack fluid = this.TANK.getFluid();
        if (!fluid.isEmpty()) {
            builder.set(CastingDataComponents.FLUIDS, List.of(fluid.copy()));
        }
    }

    @Override
    protected void applyImplicitComponents(DataComponentInput input) {
        super.applyImplicitComponents(input);
        List<FluidStack> fluids = input.get(CastingDataComponents.FLUIDS);
        if (fluids != null && !fluids.isEmpty()) {
            this.TANK.setFluid(fluids.getFirst().copy());
        }
    }

}