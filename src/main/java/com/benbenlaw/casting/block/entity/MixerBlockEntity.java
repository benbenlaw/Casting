package com.benbenlaw.casting.block.entity;

import com.benbenlaw.casting.block.CastingBlocks;
import com.benbenlaw.casting.recipe.FuelRecipe;
import com.benbenlaw.casting.recipe.MixingRecipe;
import com.benbenlaw.casting.screen.MixerMenu;
import com.benbenlaw.core.recipe.NoInventoryRecipe;
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
import net.minecraft.world.InteractionHand;
import net.minecraft.world.MenuProvider;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.inventory.AbstractContainerMenu;
import net.minecraft.world.inventory.ContainerData;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.crafting.RecipeHolder;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.entity.BlockEntity;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.chunk.LevelChunk;
import net.neoforged.neoforge.fluids.FluidActionResult;
import net.neoforged.neoforge.fluids.FluidStack;
import net.neoforged.neoforge.fluids.FluidUtil;
import net.neoforged.neoforge.fluids.IFluidTank;
import net.neoforged.neoforge.fluids.capability.IFluidHandler;
import net.neoforged.neoforge.fluids.capability.templates.FluidTank;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.*;

import static net.neoforged.neoforge.fluids.FluidStack.isSameFluidSameComponents;

public class MixerBlockEntity extends BlockEntity implements MenuProvider {

    public final FluidTank TANK_1 = new FluidTank(8000) {
        @Override
        protected void onContentsChanged() {
            setChanged();
            sync();
        }
    };

    public final FluidTank TANK_2 = new FluidTank(8000) {
        @Override
        protected void onContentsChanged() {
            setChanged();
            sync();
        }
    };

    public final FluidTank TANK_3 = new FluidTank(8000) {
        @Override
        protected void onContentsChanged() {
            setChanged();
            sync();
        }
    };

    public final FluidTank TANK_4 = new FluidTank(8000) {
        @Override
        protected void onContentsChanged() {
            setChanged();
            sync();
        }
    };

    public final FluidTank TANK_5 = new FluidTank(8000) {
        @Override
        protected void onContentsChanged() {
            setChanged();
            sync();
        }
    };

    public final FluidTank TANK_6 = new FluidTank(8000) {
        @Override
        protected void onContentsChanged() {
            setChanged();
            sync();
        }
    };

    public final FluidTank OUTPUT_TANK = new FluidTank(16000) {
        @Override
        protected void onContentsChanged() {
            setChanged();
            sync();
        }
    };

    public final FluidTank[] tanks = new FluidTank[]{TANK_1, TANK_2, TANK_3, TANK_4, TANK_5, TANK_6};

    public final IFluidHandler fluidHandler = new IFluidHandler() {
        @Override
        public int getTanks() {
            return 7;
        }

        @Override
        public FluidStack getFluidInTank(int tank) {
            return switch (tank) {
                case 0 -> TANK_1.getFluid();
                case 1 -> TANK_2.getFluid();
                case 2 -> TANK_3.getFluid();
                case 3 -> TANK_4.getFluid();
                case 4 -> TANK_5.getFluid();
                case 5 -> TANK_6.getFluid();
                case 6 -> OUTPUT_TANK.getFluid();
                default -> FluidStack.EMPTY;
            };
        }

        @Override
        public int getTankCapacity(int tank) {
            return switch (tank) {
                case 0 -> TANK_1.getCapacity();
                case 1 -> TANK_2.getCapacity();
                case 2 -> TANK_3.getCapacity();
                case 3 -> TANK_4.getCapacity();
                case 4 -> TANK_5.getCapacity();
                case 5 -> TANK_6.getCapacity();
                case 6 -> OUTPUT_TANK.getCapacity();
                default -> 0;
            };
        }

        @Override
        public boolean isFluidValid(int tank, FluidStack stack) {
            return switch (tank) {
                case 0 -> TANK_1.isFluidValid(stack);
                case 1 -> TANK_2.isFluidValid(stack);
                case 2 -> TANK_3.isFluidValid(stack);
                case 3 -> TANK_4.isFluidValid(stack);
                case 4 -> TANK_5.isFluidValid(stack);
                case 5 -> TANK_6.isFluidValid(stack);
                case 6 -> OUTPUT_TANK.isFluidValid(stack);
                default -> false;
            };
        }

        @Override
        public int fill(@NotNull FluidStack resource, @NotNull FluidAction action) {
            // Check if any tank already contains the same type of fluid
            for (FluidTank tank : new FluidTank[]{TANK_1, TANK_2, TANK_3, TANK_4, TANK_5, TANK_6}) {
                if (!tank.getFluid().isEmpty() && tank.getFluid().getFluid().isSame(resource.getFluid())) {
                    // Calculate how much more of the fluid can be added to this tank
                    int spaceLeft = tank.getCapacity() - tank.getFluidAmount();
                    int amountToFill = Math.min(resource.getAmount(), spaceLeft);

                    if (amountToFill > 0) {
                        FluidStack resourceToFill = resource.copy();
                        resourceToFill.setAmount(amountToFill);
                        return tank.fill(resourceToFill, action);
                    } else {
                        return 0; // Tank is already full
                    }
                }
            }

            // Try to fill the first empty tank
            for (FluidTank tank : new FluidTank[]{TANK_1, TANK_2, TANK_3, TANK_4, TANK_5, TANK_6}) {
                if (tank.isFluidValid(resource) && tank.getFluid().isEmpty()) {
                    int amountToFill = Math.min(resource.getAmount(), tank.getCapacity());
                    if (amountToFill > 0) {
                        FluidStack resourceToFill = resource.copy();
                        resourceToFill.setAmount(amountToFill);
                        return tank.fill(resourceToFill, action);
                    }
                }
            }

            return 0; // Couldn't fill any tank
        }

        @Override
        public FluidStack drain(FluidStack resource, FluidAction action) {
            if (resource.getFluid().isSame(OUTPUT_TANK.getFluid().getFluid())) {
                return OUTPUT_TANK.drain(resource.getAmount(), action);
            }
            return FluidStack.EMPTY;
        }

        @Override
        public FluidStack drain(int maxDrain, FluidAction action) {
            FluidStack drained = OUTPUT_TANK.drain(maxDrain, action);
            if (!drained.isEmpty()) {
                return drained;
            }
            return FluidStack.EMPTY;
        }
    };


    public boolean onPlayerUse(Player player, InteractionHand hand) {
        ItemStack heldItem = player.getItemInHand(hand);

        // Check if the held item is a valid container
        if (heldItem.isEmpty()) {
            return false;
        }

        // Attempt to fill the container from the output tank
        FluidActionResult result = FluidUtil.tryFillContainer(heldItem, OUTPUT_TANK, 1000, null, true);
        if (result.isSuccess()) {
            ItemStack filledContainer = result.getResult();

            // Check if the filled container is different from the held item
            if (!ItemStack.matches(heldItem, filledContainer)) {
                // Replace the held item with the filled container
                player.setItemInHand(hand, filledContainer);
                return true;
            }
        }

        FluidTank[] inputTanks = new FluidTank[]{TANK_1, TANK_2, TANK_3, TANK_4, TANK_5, TANK_6};

        // Check if the held item contains a fluid that's already full in one of the tanks
        if (FluidUtil.getFluidContained(heldItem).isPresent()) {
            FluidStack fluidInBucket = FluidUtil.getFluidContained(heldItem).get();

            for (FluidTank tank : inputTanks) {
                if (isSameFluidSameComponents(tank.getFluid(), fluidInBucket)) {
                    // Check if the tank is full
                    if (tank.getFluidAmount() >= tank.getCapacity()) {
                        // Prevent the operation if the tank is already full of the same fluid
                        return false;
                    }
                }
            }
        }

        // Attempt to interact with the fluid handlers if the tank was not full
        for (FluidTank tank : inputTanks) {
            if (FluidUtil.interactWithFluidHandler(player, hand, tank)) {
                return true;
            }
        }

        // If no interaction was successful, return false
        return false;
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

    public void syncContents(ServerPlayer player) {
        player.connection.send(Objects.requireNonNull(getUpdatePacket()));
    }



    public void setFluid1(FluidStack stack) {
        this.TANK_1.setFluid(stack);
    }

    public void getFluid1(FluidStack stack) {
        TANK_1.setFluid(stack);
    }

    public FluidStack getFluidStack1() {
        return this.TANK_1.getFluid();
    }

    public void setFluid2(FluidStack stack) {
        this.TANK_2.setFluid(stack);
    }

    public void getFluid2(FluidStack stack) {
        TANK_2.setFluid(stack);
    }

    public FluidStack getFluidStack2() {
        return this.TANK_2.getFluid();
    }

    public void setFluid3(FluidStack stack) {
        this.TANK_3.setFluid(stack);
    }

    public void getFluid3(FluidStack stack) {
        TANK_3.setFluid(stack);
    }

    public FluidStack getFluidStack3() {
        return this.TANK_3.getFluid();
    }

    public void setFluid4(FluidStack stack) {
        this.TANK_4.setFluid(stack);
    }

    public void getFluid4(FluidStack stack) {
        TANK_4.setFluid(stack);
    }

    public FluidStack getFluidStack4() {
        return this.TANK_4.getFluid();
    }

    public void setFluid5(FluidStack stack) {
        this.TANK_5.setFluid(stack);
    }

    public void getFluid5(FluidStack stack) {
        TANK_5.setFluid(stack);
    }

    public FluidStack getFluidStack5() {
        return this.TANK_5.getFluid();
    }

    public void setFluid6(FluidStack stack) {
        this.TANK_6.setFluid(stack);
    }

    public void getFluid6(FluidStack stack) {
        TANK_6.setFluid(stack);
    }

    public FluidStack getFluidStack6() {
        return this.TANK_6.getFluid();
    }

    public void setOutputFluid(FluidStack stack) {
        this.OUTPUT_TANK.setFluid(stack);
    }

    public void getOutputFluid(FluidStack stack) {
        OUTPUT_TANK.setFluid(stack);
    }

    public FluidStack getOutputFluid() {
        return this.OUTPUT_TANK.getFluid();
    }



    public final ContainerData data;
    public int progress = 0;
    public int maxProgress = 220;
    public int fuelTemp = 0;
    public int totalWhisksAbove = 0;

    public MixerBlockEntity(BlockPos pos, BlockState state) {
        super(CastingBlockEntities.MIXER_BLOCK_ENTITY.get(), pos, state);
        this.data = new ContainerData() {
            public int get(int index) {
                return switch (index) {
                    case 0 -> MixerBlockEntity.this.progress;
                    case 1 -> MixerBlockEntity.this.maxProgress;
                    default -> 0;
                };
            }

            public void set(int index, int value) {
                switch (index) {
                    case 0 -> MixerBlockEntity.this.progress = value;
                    case 1 -> MixerBlockEntity.this.maxProgress = value;
                }
            }

            public int getCount() {
                return 2;
            }
        };
    }


    @Override
    public Component getDisplayName() {
        return Component.translatable("block.casting.mixer");
    }


    @Nullable
    @Override
    public AbstractContainerMenu createMenu(int container, @NotNull Inventory inventory, @NotNull Player player) {
        return new MixerMenu(container, inventory, this.getBlockPos(), data);
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
        compoundTag.putInt("progress", progress);
        compoundTag.putInt("maxProgress", maxProgress);
        compoundTag.putInt("totalWhisksAbove", totalWhisksAbove);
        compoundTag.put("tank1", TANK_1.writeToNBT(provider, new CompoundTag()));
        compoundTag.put("tank2", TANK_2.writeToNBT(provider, new CompoundTag()));
        compoundTag.put("tank3", TANK_3.writeToNBT(provider, new CompoundTag()));
        compoundTag.put("tank4", TANK_4.writeToNBT(provider, new CompoundTag()));
        compoundTag.put("tank5", TANK_5.writeToNBT(provider, new CompoundTag()));
        compoundTag.put("tank6", TANK_6.writeToNBT(provider, new CompoundTag()));
        compoundTag.put("output_tank", OUTPUT_TANK.writeToNBT(provider, new CompoundTag()));
        compoundTag.putInt("fuelTemp", fuelTemp);



    }

    @Override
    protected void loadAdditional(CompoundTag compoundTag, HolderLookup.@NotNull Provider provider) {
        progress = compoundTag.getInt("progress");
        maxProgress = compoundTag.getInt("maxProgress");
        totalWhisksAbove = compoundTag.getInt("totalWhisksAbove");
        TANK_1.readFromNBT(provider, compoundTag.getCompound("tank1"));
        TANK_2.readFromNBT(provider, compoundTag.getCompound("tank2"));
        TANK_3.readFromNBT(provider, compoundTag.getCompound("tank3"));
        TANK_4.readFromNBT(provider, compoundTag.getCompound("tank4"));
        TANK_5.readFromNBT(provider, compoundTag.getCompound("tank5"));
        TANK_6.readFromNBT(provider, compoundTag.getCompound("tank6"));
        OUTPUT_TANK.readFromNBT(provider, compoundTag.getCompound("output_tank"));
        fuelTemp = compoundTag.getInt("fuelTemp");

        super.loadAdditional(compoundTag, provider);
    }


    public void drops() {
    }

    public void tick() {
        assert level != null;
        if (!level.isClientSide()) {


            //Drain to adjacent solidifer

            for (Direction direction : Direction.values()) {
                BlockEntity entity = level.getBlockEntity(this.worldPosition.relative(direction));
                if (entity instanceof SolidifierBlockEntity solidifierBlockEntity) {
                    transferFluid(OUTPUT_TANK, solidifierBlockEntity.TANK);

                }
            }

            //Drain to adjacent tool modifier
            for (Direction direction : Direction.values()) {
                BlockEntity entity = level.getBlockEntity(this.worldPosition.relative(direction));
                if (entity instanceof EquipmentModifierBlockEntity toolModifierBlockEntity) {
                    transferFluid(OUTPUT_TANK, toolModifierBlockEntity.TANK);
                }
            }

            //Check for Whisk

            whiskCheck();

            drainTanksIntoValidBlocks();
            fuelInformation(level.getBlockEntity(this.worldPosition));

            for (RecipeHolder<MixingRecipe> recipeHolder : level.getRecipeManager().getRecipesFor(MixingRecipe.Type.INSTANCE, NoInventoryRecipe.INSTANCE, level)) {
                MixingRecipe recipe = recipeHolder.value();
                List<FluidStack> requiredFluids = recipe.getAllFluids();

                requiredFluids.removeIf(FluidStack::isEmpty);

                Map<FluidStack, Integer> matchedTanks = checkFluidCombinations(requiredFluids);
                if (matchedTanks != null && hasOutputSpaceMaking(this, recipe)) {
                    progress++;
                    if (progress >= maxProgress) {
                        addOutputFluid(recipe.outputFluid());
                        removeTankFluids(matchedTanks);
                        setChanged();
                        resetProgress();
                        sync();
                    }
                }
            }
        }
    }

    public void whiskCheck() {

        maxProgress = 220;
        int whiskEffectAmount = 40;

        BlockPos aboveMixer = this.getBlockPos().above();
        totalWhisksAbove = 0;

        while (true) {
            assert level != null;
            if (!level.getBlockState(aboveMixer).is(CastingBlocks.MIXER_WHISK.get())) break;
            totalWhisksAbove++;
            aboveMixer = aboveMixer.above();
        }

        maxProgress -= totalWhisksAbove * whiskEffectAmount;

        if (maxProgress < 20) {
            maxProgress = 20;
        }
    }

    private void drainTanksIntoValidBlocks() {

        //Drain to adjacent solidifier
        for (Direction direction : Direction.values()) {
            BlockEntity entity = level.getBlockEntity(this.worldPosition.relative(direction));
            if (entity instanceof SolidifierBlockEntity solidifierBlockEntity) {
                transferFluid(OUTPUT_TANK, solidifierBlockEntity.TANK);
            }
        }
    }

    private void removeTankFluids(Map<FluidStack, Integer> matchedTanks) {
        for (Map.Entry<FluidStack, Integer> entry : matchedTanks.entrySet()) {
            FluidStack requiredFluid = entry.getKey();
            int tankIndex = entry.getValue();

            FluidStack tankFluid = tanks[tankIndex].getFluid();
            tankFluid.shrink(requiredFluid.getAmount());
            tanks[tankIndex].setFluid(tankFluid);
        }
    }




    private Map<FluidStack, Integer> checkFluidCombinations(List<FluidStack> requiredFluids) {
        List<List<Integer>> combinations = generateCombinations(6);

        for (List<Integer> combination : combinations) {
            Map<FluidStack, Integer> matchedTanks = checkFluidsInCombination(requiredFluids, combination);
            if (matchedTanks != null) {
                return matchedTanks; // Return the matched tank indices
            }
        }

        return null; // No combination of tanks matched all required fluids
    }



    private List<List<Integer>> generateCombinations(int n) {
        List<List<Integer>> combinations = new ArrayList<>();
        int[] currentCombination = new int[n];
        generateCombinationsHelper(combinations, currentCombination, 0, 0, n);
        return combinations;
    }

    private void generateCombinationsHelper(List<List<Integer>> combinations, int[] currentCombination, int index, int start, int n) {
        if (index == n) {
            List<Integer> combination = new ArrayList<>();
            for (int i = 0; i < n; i++) {
                combination.add(currentCombination[i]);
            }
            combinations.add(combination);
            return;
        }

        for (int i = start; i < 6; i++) { // Assuming you have 6 tanks
            currentCombination[index] = i;
            generateCombinationsHelper(combinations, currentCombination, index + 1, i + 1, n);
        }
    }

    private Map<FluidStack, Integer> checkFluidsInCombination(List<FluidStack> requiredFluids, List<Integer> combination) {
        Map<FluidStack, Integer> matchedTanks = new HashMap<>();

        for (FluidStack fluid : requiredFluids) {
            boolean fluidFound = false;
            for (int tankIndex : combination) {
                FluidStack tankFluid = tanks[tankIndex].getFluid();
                int tankAmount = tanks[tankIndex].getFluidAmount();
                if (fluid.getFluid().isSame(tankFluid.getFluid()) && fluid.getAmount() <= tankAmount) {
                    matchedTanks.put(fluid, tankIndex);
                    fluidFound = true;
                    break;
                }
            }
            if (!fluidFound) {
                return null; // Required fluid not found in this combination of tanks
            }
        }
        return matchedTanks; // All required fluids are found in this combination of tanks
    }






    private void addOutputFluid(FluidStack output) {
        OUTPUT_TANK.fill(output, IFluidHandler.FluidAction.EXECUTE);
    }

    private boolean hasOutputSpaceMaking(MixerBlockEntity entity, MixingRecipe recipe) {
        FluidStack output = recipe.outputFluid();
        FluidTank outputTank = entity.OUTPUT_TANK;
        return outputTank.getFluid().isEmpty() || (outputTank.getFluid().getFluid().isSame(output.getFluid()) &&
                outputTank.getFluidAmount() + output.getAmount() <= outputTank.getCapacity());
    }


    private void resetProgress() {
        progress = 0;
    }

    private void transferFluid(IFluidTank sourceTank, IFluidTank targetTank) {
        if (sourceTank.getFluidAmount() > 0) {
            int drainAmount = sourceTank.getFluidAmount();
            FluidStack drained = sourceTank.drain(drainAmount, IFluidHandler.FluidAction.SIMULATE);
            int filled = targetTank.fill(drained, IFluidHandler.FluidAction.EXECUTE);
            sourceTank.drain(filled, IFluidHandler.FluidAction.EXECUTE);
        }
    }

    private boolean fuelInformation(BlockEntity entity) {
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
                        if (recipe.fluid().getFluid() == tankBlockEntity.FLUID_TANK.getFluid().getFluid()) {

                            //maxProgress = recipe.smeltTime();
                            fuelTemp = recipe.temp();

                        }
                    }
                }
            }
        }
        return false;
    }
}