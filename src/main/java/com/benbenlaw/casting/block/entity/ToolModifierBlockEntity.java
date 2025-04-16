package com.benbenlaw.casting.block.entity;

import com.benbenlaw.casting.config.ToolModifierConfig;
import com.benbenlaw.casting.item.CastingDataComponents;
import com.benbenlaw.casting.recipe.SolidifierRecipe;
import com.benbenlaw.casting.recipe.ToolModifierRecipe;
import com.benbenlaw.casting.screen.ToolModifierMenu;
import com.benbenlaw.casting.util.CastingTags;
import com.benbenlaw.casting.util.ValidToolTypesForToolModifiers;
import com.benbenlaw.opolisutilities.block.entity.custom.handler.InputOutputItemHandler;
import com.benbenlaw.opolisutilities.util.inventory.IInventoryHandlingBlockEntity;
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
import net.minecraft.world.InteractionHand;
import net.minecraft.world.MenuProvider;
import net.minecraft.world.SimpleContainer;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.inventory.AbstractContainerMenu;
import net.minecraft.world.inventory.ContainerData;
import net.minecraft.world.item.*;
import net.minecraft.world.item.crafting.RecipeHolder;
import net.minecraft.world.item.crafting.RecipeInput;
import net.minecraft.world.level.block.entity.BlockEntity;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.chunk.LevelChunk;
import net.neoforged.fml.ModList;
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

import static com.benbenlaw.casting.util.ValidToolTypesForToolModifiers.*;

public class ToolModifierBlockEntity extends BlockEntity implements MenuProvider, IInventoryHandlingBlockEntity {

    private final ItemStackHandler itemHandler = new ItemStackHandler(3) {
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

    public final FluidTank TANK = new FluidTank(64000) {
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
    public int maxProgress = 100;
    public int fuelTemp = 0;
    public int storedTankFluidAmount = 0;
    public int storedTankFluidAmountUsedInRecipe = 0;
    public int TOOL_SLOT = 0;
    public int UPGRADE_ITEM_SLOT = 1;
    public int OUTPUT_SLOT = 2;
    public int MAX_FORTUNE_LEVEL = ToolModifierConfig.maxFortuneAmount.get();
    public int MAX_EFFICIENCY_LEVEL = ToolModifierConfig.maxEfficiencyAmount.get();
    public int MAX_UNBREAKING_LEVEL = ToolModifierConfig.maxUnbreakingAmount.get();
    public int MAX_REPAIRING_LEVEL = ToolModifierConfig.maxRepairingAmount.get();
    public boolean errorMaxLevel;
    public boolean isLimitMode = false;
    private final IItemHandler toolModifierItemHandler = new InputOutputItemHandler(itemHandler,
            (i, stack) -> i == 0 ,  //
            i -> i == 1
    );

    public @Nullable IItemHandler getItemHandlerCapability(@Nullable Direction side) {
        return toolModifierItemHandler;
    }

    public void setHandler(ItemStackHandler handler) {
        for (int i = 0; i < handler.getSlots(); i++) {
            this.itemHandler.setStackInSlot(i, handler.getStackInSlot(i));
        }
    }

    public ItemStackHandler getItemStackHandler() {
        return this.itemHandler;
    }

    public ToolModifierBlockEntity(BlockPos pos, BlockState state) {
        super(ModBlockEntities.TOOL_MODIFIER_BLOCK_ENTITY.get(), pos, state);
        this.data = new ContainerData() {
            public int get(int index) {
                return switch (index) {
                    case 0 -> ToolModifierBlockEntity.this.progress;
                    case 1 -> ToolModifierBlockEntity.this.maxProgress;
                    default -> 0;
                };
            }

            public void set(int index, int value) {
                switch (index) {
                    case 0 -> ToolModifierBlockEntity.this.progress = value;
                    case 1 -> ToolModifierBlockEntity.this.maxProgress = value;
                }
            }

            public int getCount() {
                return 2;
            }
        };
    }


    @Override
    public Component getDisplayName() {
        return Component.translatable("block.casting.tool_modifier");
    }


    @Nullable
    @Override
    public AbstractContainerMenu createMenu(int container, @NotNull Inventory inventory, @NotNull Player player) {
        return new ToolModifierMenu(container, inventory, this.getBlockPos(), data);
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
        compoundTag.putBoolean("errorMaxLevel", errorMaxLevel);

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
        errorMaxLevel = compoundTag.getBoolean("errorMaxLevel");
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

        if (!level.isClientSide()) {
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
            boolean foundMatch = false;
            boolean blockedByMax = false;




            if (itemHandler.getStackInSlot(TOOL_SLOT).getItem() instanceof TieredItem || itemHandler.getStackInSlot(TOOL_SLOT).getItem() instanceof ShearsItem) {
                ItemStack toolStack = itemHandler.getStackInSlot(TOOL_SLOT);
                String toolType = getToolType(toolStack);

                List<String> validModifiers = ValidToolTypesForToolModifiers.TOOL_TYPE_VALID_MODIFIERS.get(toolType);

                for (RecipeHolder<ToolModifierRecipe> recipeHolder :
                        level.getRecipeManager().getRecipesFor(ToolModifierRecipe.Type.INSTANCE, inventory, level)) {

                    ToolModifierRecipe recipe = recipeHolder.value();
                    String effect = recipe.effect();

                    //Valid effect for the tool type
                    if (!validModifiers.isEmpty()) {
                        if (!validModifiers.contains(effect)) {
                            continue;
                        }
                    }

                    boolean validRecipe = false;
                    boolean bothRequired = false;
                    boolean fluidOnly = false;
                    boolean itemOnly = false;

                    if (recipe.requiresBothItemAndFluid()) {
                        if (hasEnoughFluid(recipe.upgradeFluid()) &&
                                itemHandler.getStackInSlot(UPGRADE_ITEM_SLOT).getCount() >= recipe.upgradeItem().count() && recipe.upgradeItem().test(itemHandler.getStackInSlot(UPGRADE_ITEM_SLOT))) {
                            validRecipe = true;
                            bothRequired = true;
                        }
                    }

                    if (!recipe.requiresBothItemAndFluid()) {
                        if (itemHandler.getStackInSlot(UPGRADE_ITEM_SLOT).getCount() >= recipe.upgradeItem().count() && recipe.upgradeItem().test(itemHandler.getStackInSlot(UPGRADE_ITEM_SLOT))) {

                            validRecipe = true;
                            itemOnly = true;
                        }
                        else if (hasEnoughFluid(recipe.upgradeFluid())) {
                            validRecipe = true;
                            fluidOnly = true;
                        }

                    }

                    boolean effectMaxed = isEffectAtMax(toolStack, recipe.effect());

                    if (validRecipe && !effectMaxed) {
                        if (itemHandler.getStackInSlot(OUTPUT_SLOT).isEmpty()) {
                            progress++;
                            if (progress >= maxProgress) {
                                if (bothRequired || fluidOnly) {
                                    extractFluid(recipe.upgradeFluid(), recipe.upgradeFluid().getAmount());
                                }

                                if (bothRequired || itemOnly) {
                                    itemHandler.extractItem(UPGRADE_ITEM_SLOT, recipe.upgradeItem().count(), false);
                                }

                                itemHandler.setStackInSlot(OUTPUT_SLOT, copyAndApplyEffect(toolStack, effect));
                                itemHandler.setStackInSlot(TOOL_SLOT, ItemStack.EMPTY);
                                resetProgress();
                            }
                        }

                        foundMatch = true;
                        errorMaxLevel = false;
                    } else if (effectMaxed) {
                        blockedByMax = true;
                    }
                }
            }

            if (!foundMatch) {
                errorMaxLevel = blockedByMax;
                resetProgress();
            }
        }
    }


    private static Class<?> mekanismPaxelClass;

    private String getToolType(ItemStack stack){

        if (ModList.get().isLoaded("mekanismtools") && mekanismPaxelClass == null) {
            try {
                mekanismPaxelClass = Class.forName("mekanism.tools.common.item.ItemMekanismPaxel");
            } catch (ClassNotFoundException e) {
                throw new RuntimeException(e);
            }
        }

        Item item = stack.getItem();

        if (mekanismPaxelClass != null && mekanismPaxelClass.isInstance(item)) {
            return ValidToolTypesForToolModifiers.PAXEL_MODIFIERS;
        }

        return switch (item) {
            case PickaxeItem pickaxeItem -> ValidToolTypesForToolModifiers.PICKAXE_MODIFIERS;
            case AxeItem axeItem -> ValidToolTypesForToolModifiers.AXE_MODIFIERS;
            case ShovelItem shovelItem -> ValidToolTypesForToolModifiers.SHOVEL_MODIFIERS;
            case HoeItem hoeItem -> ValidToolTypesForToolModifiers.HOE_MODIFIERS;
            case SwordItem swordItem -> ValidToolTypesForToolModifiers.SWORD_MODIFIERS;
            case ShearsItem shearItem -> ValidToolTypesForToolModifiers.SHEAR_MODIFIERS;
            default -> "";
        };
    }

    private boolean isEffectAtMax(ItemStack stack, String effect) {
        boolean hasSilkTouch = stack.getOrDefault(CastingDataComponents.SILK_TOUCH, false);
        int currentFortune = stack.getOrDefault(CastingDataComponents.FORTUNE, 0);

        if (effect.contains(FORTUNE)) {
            return hasSilkTouch || currentFortune >= MAX_FORTUNE_LEVEL;
        }
        if (effect.contains(EFFICIENCY)) {
            int currentEfficiency = stack.getOrDefault(CastingDataComponents.EFFICIENCY, 0);
            return currentEfficiency >= MAX_EFFICIENCY_LEVEL;
        }
        if (effect.contains(SILK_TOUCH)) {
            return currentFortune > 0 || hasSilkTouch;
        }
        if (effect.contains(UNBREAKING)) {
            int currentUnbreaking = stack.getOrDefault(CastingDataComponents.UNBREAKING, 0);
            return currentUnbreaking >= MAX_UNBREAKING_LEVEL;
        }
        if (effect.contains(REPAIRING)) {
            int currentRepairing = stack.getOrDefault(CastingDataComponents.REPAIRING, 0);
            return currentRepairing >= MAX_REPAIRING_LEVEL;
        }
        if (effect.contains(TORCH_PLACING)) {
            return stack.getOrDefault(CastingDataComponents.TORCH_PLACING, false);
        }
        if (effect.contains(AUTO_SMELT)) {
            return stack.getOrDefault(CastingDataComponents.AUTO_SMELT, false);
        }

        return false;
    }

    public ItemStack copyAndApplyEffect(ItemStack stack, String effect) {
        ItemStack copy = stack.copy();

        if (effect.contains(FORTUNE)) {
            int currentFortune = copy.getOrDefault(CastingDataComponents.FORTUNE, 0);
            int newFortune = Math.min(currentFortune + 1, MAX_FORTUNE_LEVEL);
            copy.set(CastingDataComponents.FORTUNE, newFortune);
        }
        if (effect.contains(EFFICIENCY)) {
            int currentEfficiency = copy.getOrDefault(CastingDataComponents.EFFICIENCY, 0);
            int newEfficiency = Math.min(currentEfficiency + 1, MAX_EFFICIENCY_LEVEL);
            copy.set(CastingDataComponents.EFFICIENCY, newEfficiency);
        }
        if (effect.contains(SILK_TOUCH)) {
            boolean isSilkTouch = copy.getOrDefault(CastingDataComponents.SILK_TOUCH, false);
            copy.set(CastingDataComponents.SILK_TOUCH, !isSilkTouch);
        }
        if (effect.contains(UNBREAKING)) {
            int currentUnbreaking = copy.getOrDefault(CastingDataComponents.UNBREAKING, 0);
            int newUnbreaking = Math.min(currentUnbreaking + 1, ToolModifierConfig.maxUnbreakingAmount.get());
            copy.set(CastingDataComponents.UNBREAKING, newUnbreaking);
        }
        if (effect.contains(REPAIRING)) {
            int currentRepairing = copy.getOrDefault(CastingDataComponents.REPAIRING, 0);
            int newRepairing = Math.min(currentRepairing + 1, ToolModifierConfig.maxRepairingAmount.get());
            copy.set(CastingDataComponents.REPAIRING, newRepairing);
        }
        if (effect.contains(TORCH_PLACING)) {
            boolean isTorchPlacing = copy.getOrDefault(CastingDataComponents.TORCH_PLACING, false);
            copy.set(CastingDataComponents.TORCH_PLACING, !isTorchPlacing);
        }
        if (effect.contains(AUTO_SMELT)) {
            boolean isAutoSmelt = copy.getOrDefault(CastingDataComponents.AUTO_SMELT, false);
            copy.set(CastingDataComponents.AUTO_SMELT, !isAutoSmelt);
        }

        return copy;

    }

    private void resetProgress() {
        progress = 0;
    }


    private void extractFluid(FluidStack output, int amount) {
        if (TANK.getFluidAmount() >= amount && TANK.getFluid().getFluid() == output.getFluid()) {
            TANK.drain(amount, IFluidHandler.FluidAction.EXECUTE);
        }
    }


    private boolean hasEnoughFluid(FluidStack output) {
        if (output == null || output.isEmpty()) {
            return false;
        }

        int tankAmount = TANK.getFluidAmount();
        if (isLimitMode) {
            tankAmount = tankAmount - 100;
        }

        return tankAmount >= output.getAmount() &&
                TANK.getFluid().getFluid() == output.getFluid();
    }


    private boolean isRecipeSlotsValidForTanks(SolidifierRecipe recipe) {
        FluidStack recipeFluid = recipe.fluid();
        return TANK.getFluid().is(recipeFluid.getFluidType()) && (tankIsValidForSlot(recipeFluid, 0) || tankIsValidForSlot(recipeFluid, 1));
    }

    private boolean tankIsValidForSlot(FluidStack stack, int slot) {

        return stack.getFluid() == TANK.getFluid().getFluid();
    }

    private boolean hasOutputSpaceMaking(ToolModifierBlockEntity entity, SolidifierRecipe recipe) {
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

}