package com.benbenlaw.casting.block.entity;

import com.benbenlaw.casting.block.custom.ControllerBlock;
import com.benbenlaw.casting.recipe.FuelRecipe;
import com.benbenlaw.casting.recipe.MeltingRecipe;
import com.benbenlaw.casting.screen.SmelterMenu;
import com.benbenlaw.core.block.entity.handler.IInventoryHandlingBlockEntity;
import com.benbenlaw.core.block.entity.handler.InputOutputItemHandler;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.core.HolderLookup;
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
import net.minecraft.world.MenuProvider;
import net.minecraft.world.SimpleContainer;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.inventory.AbstractContainerMenu;
import net.minecraft.world.inventory.ContainerData;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.crafting.RecipeHolder;
import net.minecraft.world.item.crafting.RecipeInput;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.entity.BlockEntity;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.chunk.LevelChunk;
import net.neoforged.neoforge.fluids.FluidStack;
import net.neoforged.neoforge.fluids.IFluidTank;
import net.neoforged.neoforge.fluids.capability.IFluidHandler;
import net.neoforged.neoforge.fluids.capability.templates.FluidTank;
import net.neoforged.neoforge.items.IItemHandler;
import net.neoforged.neoforge.items.ItemStackHandler;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;
import java.util.Objects;

import static net.neoforged.neoforge.fluids.FluidStack.isSameFluidSameComponents;

public class ControllerBlockEntity extends BlockEntity implements MenuProvider, IInventoryHandlingBlockEntity {

    private final ItemStackHandler itemHandler = new ItemStackHandler(16) {
        @Override
        protected void onContentsChanged(int slot) {
            setChanged();
            sync();
        }

        @Override
        protected int getStackLimit(int slot, ItemStack stack) {
            if (slot >= 0 && slot <= 14) {
                return 1;
            } else {
                return 64;
            }
        }

        @Override
        public int getSlotLimit(int slot) {
            if (slot >= 0 && slot <= 14) {
                return 1;
            } else {
                return 64;
            }
        }
    };

    public final FluidTank TANK_1 = new FluidTank(16000) {
        @Override
        protected void onContentsChanged() {
            setChanged();
            sync();
        }
    };

    public final FluidTank TANK_2 = new FluidTank(16000) {
        @Override
        protected void onContentsChanged() {
            setChanged();
            sync();
        }
    };

    public final FluidTank TANK_3 = new FluidTank(16000) {
        @Override
        protected void onContentsChanged() {
            setChanged();
            sync();
        }
    };

    public final FluidTank TANK_4 = new FluidTank(16000) {
        @Override
        protected void onContentsChanged() {
            setChanged();
            sync();
        }
    };

    private final IFluidHandler fluidHandler = new IFluidHandler() {
        @Override
        public int getTanks() {
            return 4;
        }

        @Override
        public @NotNull FluidStack getFluidInTank(int tank) {
            return switch (tank) {
                case 0 -> TANK_1.getFluid();
                case 1 -> TANK_2.getFluid();
                case 2 -> TANK_3.getFluid();
                case 3 -> TANK_4.getFluid();
                default -> FluidStack.EMPTY;
            };
        }

        @Override
        public int getTankCapacity(int tank) {
            if (tank == 0)
                return TANK_1.getCapacity();
            if (tank == 1)
                return TANK_2.getCapacity();
            if (tank == 2)
                return TANK_3.getCapacity();
            if (tank == 3)
                return TANK_4.getCapacity();
            return 0;
        }

        @Override
        public boolean isFluidValid(int tank, @NotNull FluidStack stack) {
            if (tank == 0)
                return TANK_1.isFluidValid(stack);
            if (tank == 1)
                return TANK_2.isFluidValid(stack);
            if (tank == 2)
                return TANK_3.isFluidValid(stack);
            if (tank == 3)
                return TANK_4.isFluidValid(stack);
            return false;
        }

        @Override
        public int fill(FluidStack resource, @NotNull FluidAction action) {
            if (resource.getFluid() == TANK_1.getFluid().getFluid()) {
                return TANK_1.fill(resource, action);
            }
            if (resource.getFluid() == TANK_2.getFluid().getFluid()) {
                return TANK_2.fill(resource, action);
            }
            if (resource.getFluid() == TANK_3.getFluid().getFluid()) {
                return TANK_3.fill(resource, action);
            }
            if (resource.getFluid() == TANK_4.getFluid().getFluid()) {
                return TANK_4.fill(resource, action);
            }
            return 0;
        }


        @Override
        public @NotNull FluidStack drain(FluidStack resource, @NotNull FluidAction action) {

            if (resource.getFluid() == TANK_1.getFluid().getFluid()) {
                return TANK_1.drain(resource.getAmount(), action);
            }
            if (resource.getFluid() == TANK_2.getFluid().getFluid()) {
                return TANK_2.drain(resource.getAmount(), action);
            }
            if (resource.getFluid() == TANK_3.getFluid().getFluid()) {
                return TANK_3.drain(resource.getAmount(), action);
            }
            if (resource.getFluid() == TANK_4.getFluid().getFluid()) {
                return TANK_4.drain(resource.getAmount(), action);
            }
            return FluidStack.EMPTY;
        }

        @Override
        public @NotNull FluidStack drain(int maxDrain, @NotNull FluidAction action) {
            assert level != null;

            if (TANK_1.getFluidAmount() > 0) {
                return TANK_1.drain(maxDrain, action);
            }
            if (TANK_2.getFluidAmount() > 0) {
                return TANK_2.drain(maxDrain, action);
            }
            if (TANK_3.getFluidAmount() > 0) {
                return TANK_3.drain(maxDrain, action);
            }
            if (TANK_4.getFluidAmount() > 0) {
                return TANK_4.drain(maxDrain, action);
            }
            return FluidStack.EMPTY;
        }
    };

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

    public void syncContents(ServerPlayer player) {
        player.connection.send(Objects.requireNonNull(getUpdatePacket()));
    }

    public void getOutputFluidStack1(FluidStack stack) {
        this.TANK_1.setFluid(stack);
    }
    public void setOutputFluid1(FluidStack stack) {
        TANK_1.setFluid(stack);
    }
    public FluidStack getOutputFluid1() {
        return this.TANK_1.getFluid();
    }

    public void getOutputFluidStack2(FluidStack stack) {
        this.TANK_2.setFluid(stack);
    }
    public void setOutputFluid2(FluidStack stack) {
        TANK_2.setFluid(stack);
    }
    public FluidStack getOutputFluid2() {
        return this.TANK_2.getFluid();
    }

    public void getOutputFluidStack3(FluidStack stack) {
        this.TANK_3.setFluid(stack);
    }
    public void setOutputFluid3(FluidStack stack) {
        TANK_3.setFluid(stack);
    }
    public FluidStack getOutputFluid3() {
        return this.TANK_3.getFluid();
    }

    public void getOutputFluidStack4(FluidStack stack) {
        this.TANK_4.setFluid(stack);
    }
    public void setOutputFluid4(FluidStack stack) {
        TANK_4.setFluid(stack);
    }
    public FluidStack getOutputFluid4() {
        return this.TANK_4.getFluid();
    }

    public final ContainerData data;
    public int[] progress = new int[15];
    public int[] maxProgress = new int[15];
    public int fuelTemp = 0;
    private final IItemHandler controllerItemHandler = new InputOutputItemHandler(itemHandler,
            (i, stack) -> i != 15,
            i -> i == 1
    );


    public @Nullable IItemHandler getItemHandlerCapability(@Nullable Direction side) {
        return controllerItemHandler;
    }

    public void setHandler(ItemStackHandler handler) {
        for (int i = 0; i < handler.getSlots(); i++) {
            this.itemHandler.setStackInSlot(i, handler.getStackInSlot(i));
        }
    }

    public ItemStackHandler getItemStackHandler() {
        return this.itemHandler;
    }

    public ControllerBlockEntity(BlockPos pos, BlockState state) {
        super(CastingBlockEntities.CONTROLLER_BLOCK_ENTITY.get(), pos, state);
        this.data = new ContainerData() {
            public int get(int index) {
                if (index < 15) {
                    return ControllerBlockEntity.this.progress[index];
                } else if (index == 15) {
                    return ControllerBlockEntity.this.maxProgress[index - 15];
                } else {
                    return 0;
                }
            }

            public void set(int index, int value) {
                if (index < 15) {
                    ControllerBlockEntity.this.progress[index] = value;
                } else if (index == 15) {
                    ControllerBlockEntity.this.maxProgress[index - 15] = value;
                }
            }

            public int getCount() {
                return 16;
            }


        };
    }


    @Override
    public Component getDisplayName() {
        return Component.translatable("block.casting.controller");
    }


    @Nullable
    @Override
    public AbstractContainerMenu createMenu(int container, @NotNull Inventory inventory, @NotNull Player player) {
        return new SmelterMenu(container, inventory, this.getBlockPos(), data);
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
        compoundTag.putIntArray("progress", progress);
        compoundTag.putIntArray("maxProgress", maxProgress);
        compoundTag.putInt("fuelTemp", fuelTemp);
        compoundTag.put("tank1", TANK_1.writeToNBT(provider, new CompoundTag()));
        compoundTag.put("tank2", TANK_2.writeToNBT(provider, new CompoundTag()));
        compoundTag.put("tank3", TANK_3.writeToNBT(provider, new CompoundTag()));
        compoundTag.put("tank4", TANK_4.writeToNBT(provider, new CompoundTag()));


    }

    @Override
    protected void loadAdditional(CompoundTag compoundTag, HolderLookup.@NotNull Provider provider) {
        this.itemHandler.deserializeNBT(provider, compoundTag.getCompound("inventory"));

        progress = compoundTag.getIntArray("progress");
        maxProgress = compoundTag.getIntArray("maxProgress");
        fuelTemp = compoundTag.getInt("fuelTemp");

        TANK_1.readFromNBT(provider, compoundTag.getCompound("tank1"));
        TANK_2.readFromNBT(provider, compoundTag.getCompound("tank2"));
        TANK_3.readFromNBT(provider, compoundTag.getCompound("tank3"));
        TANK_4.readFromNBT(provider, compoundTag.getCompound("tank4"));

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
        assert level != null;
        if (level.isClientSide()) return;

        RecipeInput inventory = new RecipeInput() {
            @Override
            public ItemStack getItem(int index) {
                return itemHandler.getStackInSlot(index);
            }

            @Override
            public int size() {
                return itemHandler.getSlots();
            }
        };


        if (level.getGameTime() % 20 == 0) {
            drainTanksIntoValidBlocks();
            fuelInformation(level.getBlockEntity(worldPosition));
        }

        boolean isPowered = false;
        boolean powerStateChanged = false;

        List<RecipeHolder<MeltingRecipe>> recipes =
                level.getRecipeManager().getRecipesFor(MeltingRecipe.Type.INSTANCE, inventory, level);

        for (int i = 0; i < 15; i++) {
            ItemStack stack = itemHandler.getStackInSlot(i);
            if (stack.isEmpty()) {
                resetProgress(i);
                continue;
            }

            RecipeHolder<MeltingRecipe> match = recipes.stream()
                    .filter(r -> r.value().input().test(stack))
                    .findFirst()
                    .orElse(null);

            if (match == null) {
                resetProgress(i);
                continue;
            }

            FluidStack output = match.value().output();

            if (!canFitFluidInAnyTank(output) || !hasEnoughFuel(level.getBlockEntity(worldPosition), match.value().meltingTemp())) {
                resetProgress(i);
                continue;
            }

            isPowered = true;
            maxProgress[i] = setNewMaxProgress(fuelTemp, match.value().meltingTemp());
            progress[i]++;

            if (progress[i] >= maxProgress[i]) {
                int outputAmount = output.getAmount();

                if (stack.isDamageableItem()) {
                    float modifier = (float)(stack.getMaxDamage() - stack.getDamageValue()) / stack.getMaxDamage();
                    outputAmount = Math.round(outputAmount * modifier);
                }

                stack.shrink(1);
                addFluidToTank(output, outputAmount);
                useFuel(this);
                resetProgress(i);
                setChanged();
                sync();
            }
        }

        BlockState currentState = level.getBlockState(worldPosition);
        boolean currentPowered = currentState.getValue(ControllerBlock.POWERED);

        if (currentPowered != isPowered) {
            level.setBlockAndUpdate(worldPosition, currentState.setValue(ControllerBlock.POWERED, isPowered));
            sync();
        }
    }


    private void drainTanksIntoValidBlocks() {

        //Drain to adjacent solidifier

        for (Direction direction : Direction.values()) {
            BlockEntity entity = level.getBlockEntity(this.worldPosition.relative(direction));
            if (entity instanceof SolidifierBlockEntity solidifierBlockEntity) {
                transferFluidToTank(TANK_1, solidifierBlockEntity.TANK);
                transferFluidToTank(TANK_2, solidifierBlockEntity.TANK);
                transferFluidToTank(TANK_3, solidifierBlockEntity.TANK);
                transferFluidToTank(TANK_4, solidifierBlockEntity.TANK);
            }
        }

        // Drain to adjacent tool modifier
        for (Direction direction : Direction.values()) {
            BlockEntity entity = level.getBlockEntity(this.worldPosition.relative(direction));
            if (entity instanceof EquipmentModifierBlockEntity toolModifierBlockEntity) {
                transferFluidToTank(TANK_1, toolModifierBlockEntity.TANK);
                transferFluidToTank(TANK_2, toolModifierBlockEntity.TANK);
                transferFluidToTank(TANK_3, toolModifierBlockEntity.TANK);
                transferFluidToTank(TANK_4, toolModifierBlockEntity.TANK);
            }
        }


        // Drain to adjacent mixer
        for (Direction direction : Direction.values()) {
            BlockEntity entity = level.getBlockEntity(this.worldPosition.relative(direction));
            if (entity instanceof MixerBlockEntity mixerBlockEntity) {
                transferFluidToMixer(TANK_1, mixerBlockEntity);
                transferFluidToMixer(TANK_2, mixerBlockEntity);
                transferFluidToMixer(TANK_3, mixerBlockEntity);
                transferFluidToMixer(TANK_4, mixerBlockEntity);
            }
        }

    }


    private void resetProgress(int slot) {
        progress[slot] = 0;

    }
    private void addFluidToTank(FluidStack output, int amount) {

        if((TANK_1.getFluid().getFluid() == output.getFluid() && (TANK_1.getCapacity() - TANK_1.getFluidAmount() >= output.getAmount()))) {
            TANK_1.fill(new FluidStack(output.getFluid(), amount), IFluidHandler.FluidAction.EXECUTE);
        }
        else if((TANK_2.getFluid().getFluid() == output.getFluid() && (TANK_2.getCapacity() - TANK_2.getFluidAmount() >= output.getAmount()))) {
            TANK_2.fill(new FluidStack(output.getFluid(), amount), IFluidHandler.FluidAction.EXECUTE);
        }
        else if((TANK_3.getFluid().getFluid() == output.getFluid() && (TANK_3.getCapacity() - TANK_3.getFluidAmount() >= output.getAmount()))) {
            TANK_3.fill(new FluidStack(output.getFluid(), amount), IFluidHandler.FluidAction.EXECUTE);
        }
        else if((TANK_4.getFluid().getFluid() == output.getFluid() && (TANK_4.getCapacity() - TANK_4.getFluidAmount() >= output.getAmount()))) {
            TANK_4.fill(new FluidStack(output.getFluid(), amount), IFluidHandler.FluidAction.EXECUTE);
        }

        else if(TANK_1.getFluid().isEmpty()) {
            TANK_1.fill(new FluidStack(output.getFluid(), amount), IFluidHandler.FluidAction.EXECUTE);
        }
        else if(TANK_2.getFluid().isEmpty()) {
            TANK_2.fill(new FluidStack(output.getFluid(), amount), IFluidHandler.FluidAction.EXECUTE);
        }
        else if(TANK_3.getFluid().isEmpty()) {
            TANK_3.fill(new FluidStack(output.getFluid(), amount), IFluidHandler.FluidAction.EXECUTE);
        }
        else if(TANK_4.getFluid().isEmpty()) {
            TANK_4.fill(new FluidStack(output.getFluid(), amount), IFluidHandler.FluidAction.EXECUTE);
        }
    }

    private boolean canFitFluidInAnyTank(FluidStack output) {
        // Check if the fluid is already in any tank
        boolean fluidAlreadyInTank = TANK_1.getFluid().getFluid() == output.getFluid() ||
                TANK_2.getFluid().getFluid() == output.getFluid() ||
                TANK_3.getFluid().getFluid() == output.getFluid() ||
                TANK_4.getFluid().getFluid() == output.getFluid();

        // If the fluid is already in a tank, we must ensure no other tank takes the same fluid
        if (fluidAlreadyInTank) {
            return (TANK_1.getFluid().getFluid() == output.getFluid() &&
                    (TANK_1.getCapacity() - TANK_1.getFluidAmount() >= output.getAmount())) ||
                    (TANK_2.getFluid().getFluid() == output.getFluid() &&
                            (TANK_2.getCapacity() - TANK_2.getFluidAmount() >= output.getAmount())) ||
                    (TANK_3.getFluid().getFluid() == output.getFluid() &&
                            (TANK_3.getCapacity() - TANK_3.getFluidAmount() >= output.getAmount())) ||
                    (TANK_4.getFluid().getFluid() == output.getFluid() &&
                            (TANK_4.getCapacity() - TANK_4.getFluidAmount() >= output.getAmount()));
        }

        // If the fluid is not in any tank, allow it to fit in any tank that can accommodate it
        return (TANK_1.getFluid().isEmpty() ||
                (TANK_1.getFluid().getFluid() == output.getFluid() &&
                        (TANK_1.getCapacity() - TANK_1.getFluidAmount() >= output.getAmount())))
                || (TANK_2.getFluid().isEmpty() ||
                (TANK_2.getFluid().getFluid() == output.getFluid() &&
                        (TANK_2.getCapacity() - TANK_2.getFluidAmount() >= output.getAmount())))
                || (TANK_3.getFluid().isEmpty() ||
                (TANK_3.getFluid().getFluid() == output.getFluid() &&
                        (TANK_3.getCapacity() - TANK_3.getFluidAmount() >= output.getAmount())))
                || (TANK_4.getFluid().isEmpty() ||
                (TANK_4.getFluid().getFluid() == output.getFluid() &&
                        (TANK_4.getCapacity() - TANK_4.getFluidAmount() >= output.getAmount())));
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
                    List<RecipeHolder<FuelRecipe>> allFuels = level.getRecipeManager().getAllRecipesFor(FuelRecipe.Type.INSTANCE);

                    for (RecipeHolder<FuelRecipe> recipeHolder : allFuels) {
                        FuelRecipe recipe = recipeHolder.value();
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


    private boolean hasEnoughFuel(BlockEntity entity, int temp) {
        if (entity == null) {
            return false;
        }
        Level level = entity.getLevel();
        if (level != null) {
            for (Direction direction : Direction.values()) {
                BlockEntity adjacentEntity = level.getBlockEntity(entity.getBlockPos().relative(direction));
                if (adjacentEntity instanceof TankBlockEntity tankBlockEntity) {
                    List<RecipeHolder<FuelRecipe>> allFuels = level.getRecipeManager().getAllRecipesFor(FuelRecipe.Type.INSTANCE);

                    for (RecipeHolder<FuelRecipe> recipeHolder : allFuels) {
                        FuelRecipe recipe = recipeHolder.value();
                        if (recipe.fluid().getFluid() == tankBlockEntity.FLUID_TANK.getFluid().getFluid() && tankBlockEntity.FLUID_TANK.getFluid().getAmount() >= recipe.fluid().getAmount()) {
                            if (recipe.temp() >= temp) {
                                return true;
                            }
                        }
                    }
                }
            }
        }
        return false;
    }

    private void transferFluidToTank(IFluidTank sourceTank, IFluidTank targetTank) {
        if (sourceTank.getFluidAmount() > 0) {
            FluidStack fluidInSource = sourceTank.getFluid();

            // Check if the target tank is empty and already has a defined fluid type
            if (targetTank.getFluidAmount() > 0) {
                // Ensure the fluid in the target tank is the same as the source
                if (!isSameFluidSameComponents(targetTank.getFluid(), fluidInSource)) {
                    return; // Abort if fluids are not compatible
                }
            } else {
                // Ensure the target tank can accept this type of fluid (some tanks may define specific constraints)
                if (!targetTank.isFluidValid(fluidInSource)) {
                    return; // Abort if target tank cannot accept this fluid type
                }
            }

            // Proceed with the transfer
            int drainAmount = sourceTank.getFluidAmount();
            FluidStack drained = sourceTank.drain(drainAmount, IFluidHandler.FluidAction.SIMULATE);
            int filled = targetTank.fill(drained, IFluidHandler.FluidAction.EXECUTE);
            sourceTank.drain(filled, IFluidHandler.FluidAction.EXECUTE);
        }
    }


    private void transferFluidToMixer(IFluidTank sourceTank, MixerBlockEntity mixerBlockEntity) {
        if (sourceTank.getFluidAmount() > 0) {
            FluidStack fluidInSource = sourceTank.getFluid();
            IFluidTank[] mixerTanks = {
                    mixerBlockEntity.TANK_1,
                    mixerBlockEntity.TANK_2,
                    mixerBlockEntity.TANK_3,
                    mixerBlockEntity.TANK_4,
                    mixerBlockEntity.TANK_5,
                    mixerBlockEntity.TANK_6
            };

            // First check if there's already a tank with this fluid
            for (IFluidTank mixerTank : mixerTanks) {
                if (isSameFluidSameComponents(mixerTank.getFluid(), fluidInSource)) {
                    // If a tank contains the same fluid, attempt to fill it
                    if (mixerTank.getCapacity() > mixerTank.getFluidAmount()) {
                        int spaceLeftInTank = mixerTank.getCapacity() - mixerTank.getFluidAmount();
                        int drainAmount = Math.min(sourceTank.getFluidAmount(), spaceLeftInTank);

                        // Drain from the source tank and fill into the mixer tank
                        FluidStack drained = sourceTank.drain(drainAmount, IFluidHandler.FluidAction.SIMULATE);
                        if (isSameFluidSameComponents(drained, fluidInSource)) {
                            int filled = mixerTank.fill(drained, IFluidHandler.FluidAction.EXECUTE);
                            sourceTank.drain(filled, IFluidHandler.FluidAction.EXECUTE);
                        }
                    }
                    // Stop here since we only want one tank to be filled with this fluid
                    return;
                }
            }

            // If no tank currently contains the fluid, fill the first available empty tank
            for (IFluidTank mixerTank : mixerTanks) {
                if (mixerTank.getFluidAmount() == 0) {
                    int drainAmount = Math.min(sourceTank.getFluidAmount(), mixerTank.getCapacity());
                    FluidStack drained = sourceTank.drain(drainAmount, IFluidHandler.FluidAction.SIMULATE);

                    // Only proceed if the fluid types match
                    if (isSameFluidSameComponents(drained, fluidInSource)) {
                        int filled = mixerTank.fill(drained, IFluidHandler.FluidAction.EXECUTE);
                        sourceTank.drain(filled, IFluidHandler.FluidAction.EXECUTE);
                    }
                    // Stop after filling the first empty tank
                    return;
                }
            }
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
                    List<RecipeHolder<FuelRecipe>> allFuels = level.getRecipeManager().getAllRecipesFor(FuelRecipe.Type.INSTANCE);

                    for (RecipeHolder<FuelRecipe> recipeHolder : allFuels) {
                        FuelRecipe recipe = recipeHolder.value();
                        if (recipe.fluid().getFluid() == tankBlockEntity.FLUID_TANK.getFluid().getFluid()) {
                            fuelTemp = recipe.temp();
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
                fuelTemp = 0;  // Set the default temperature
            }
        }
    }


    private int setNewMaxProgress(int fuelInTankTemp, int recipeTemp) {
        if (recipeTemp <= 0) {
            throw new IllegalArgumentException("recipeTemp must be greater than zero");
        }

        if (fuelInTankTemp < recipeTemp) {
            return 10000;
        }

        double temperatureRatio = (double) fuelInTankTemp / recipeTemp;
        int newMaxProgress = (int) (240 / temperatureRatio);

        return Math.max(newMaxProgress, 1); // Ensure it's at least 1
    }
}