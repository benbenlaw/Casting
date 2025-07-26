package com.benbenlaw.casting.block.entity;

import com.benbenlaw.casting.item.CastingItems;
import com.benbenlaw.casting.item.EquipmentModifier;
import com.benbenlaw.casting.recipe.EquipmentModifierRecipe;
import com.benbenlaw.casting.recipe.MeltingRecipe;
import com.benbenlaw.casting.screen.EquipmentModifierMenu;
import com.benbenlaw.casting.util.CastingTags;
import com.benbenlaw.casting.util.EquipmentModifierUtils;
import com.benbenlaw.core.block.entity.handler.IInventoryHandlingBlockEntity;
import com.benbenlaw.core.block.entity.handler.InputOutputItemHandler;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.core.HolderLookup;
import net.minecraft.core.component.DataComponentType;
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
import net.minecraft.world.item.crafting.Ingredient;
import net.minecraft.world.item.crafting.RecipeHolder;
import net.minecraft.world.item.crafting.RecipeInput;
import net.minecraft.world.level.block.entity.BlockEntity;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.chunk.LevelChunk;
import net.neoforged.fml.ModList;
import net.neoforged.neoforge.fluids.FluidStack;
import net.neoforged.neoforge.fluids.FluidUtil;
import net.neoforged.neoforge.fluids.capability.IFluidHandler;
import net.neoforged.neoforge.fluids.capability.templates.FluidTank;
import net.neoforged.neoforge.items.IItemHandler;
import net.neoforged.neoforge.items.ItemStackHandler;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;
import java.util.Locale;
import java.util.Objects;

import static com.benbenlaw.casting.item.EquipmentModifier.EQUIPMENT_EXPERIENCE;
import static com.benbenlaw.casting.item.EquipmentModifier.EQUIPMENT_LEVEL;
import static com.benbenlaw.casting.util.ValidToolTypesForToolModifiers.*;

public class EquipmentModifierBlockEntity extends BlockEntity implements MenuProvider, IInventoryHandlingBlockEntity {

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
        public @NotNull FluidStack getFluidInTank(int tank) {
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
        public @NotNull FluidStack drain(FluidStack resource, FluidAction action) {
            if (resource.getFluid() == TANK.getFluid().getFluid()) {
                return TANK.drain(resource.getAmount(), action);
            }
            return FluidStack.EMPTY;
        }

        @Override
        public @NotNull FluidStack drain(int maxDrain, FluidAction action) {
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
    public String errorMessage = "";
    public boolean isLimitMode = false;
    private final IItemHandler equipmentModifierItemHandler = new InputOutputItemHandler(itemHandler,
            (i, stack) -> i == 1 ,  //
            i -> i == 2
    );

    public @Nullable IItemHandler getItemHandlerCapability(@Nullable Direction side) {
        return equipmentModifierItemHandler;
    }

    public void setHandler(ItemStackHandler handler) {
        for (int i = 0; i < handler.getSlots(); i++) {
            this.itemHandler.setStackInSlot(i, handler.getStackInSlot(i));
        }
    }

    public ItemStackHandler getItemStackHandler() {
        return this.itemHandler;
    }

    public EquipmentModifierBlockEntity(BlockPos pos, BlockState state) {
        super(CastingBlockEntities.EQUIPMENT_MODIFIER_BLOCK_ENTITY.get(), pos, state);
        this.data = new ContainerData() {
            public int get(int index) {
                return switch (index) {
                    case 0 -> EquipmentModifierBlockEntity.this.progress;
                    case 1 -> EquipmentModifierBlockEntity.this.maxProgress;
                    default -> 0;
                };
            }

            public void set(int index, int value) {
                switch (index) {
                    case 0 -> EquipmentModifierBlockEntity.this.progress = value;
                    case 1 -> EquipmentModifierBlockEntity.this.maxProgress = value;
                }
            }

            public int getCount() {
                return 2;
            }
        };
    }


    @Override
    public @NotNull Component getDisplayName() {
        return Component.translatable("block.casting.equipment_modifier");
    }


    @Nullable
    @Override
    public AbstractContainerMenu createMenu(int container, @NotNull Inventory inventory, @NotNull Player player) {
        return new EquipmentModifierMenu(container, inventory, this.getBlockPos(), data);
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
        compoundTag.putString("errorMessage", errorMessage);

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
        errorMessage = compoundTag.getString("errorMessage");
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

            boolean foundMatch = false;

            if (itemHandler.getStackInSlot(TOOL_SLOT).getItem() instanceof TieredItem ||
                    itemHandler.getStackInSlot(TOOL_SLOT).getItem() instanceof ShearsItem ||
                    itemHandler.getStackInSlot(TOOL_SLOT).getItem() instanceof ArmorItem) {

                ItemStack toolStack = itemHandler.getStackInSlot(TOOL_SLOT);
                String toolType = getToolType(toolStack);

                List<EquipmentModifier> validModifiers = VALID_MODIFIERS.get(toolType);

                if (validModifiers == null) {
                    validModifiers = List.of();
                }

                EquipmentModifierRecipe matchedRecipe = null;
                boolean bothRequired = false;
                boolean itemOnly = false;
                boolean fluidOnly = false;

                boolean repairMode = false;

                //new
                if (itemHandler.getStackInSlot(UPGRADE_ITEM_SLOT).is(CastingItems.REPAIRING_MOLD)) {

                    if (itemHandler.getStackInSlot(TOOL_SLOT).isEmpty()) {
                        resetProgress();
                        return;
                    }

                    ItemStack equipmentStack = itemHandler.getStackInSlot(TOOL_SLOT);

                    Ingredient getRepairItem = null;

                    if (equipmentStack.getItem() instanceof TieredItem tieredItem) {
                        getRepairItem = tieredItem.getTier().getRepairIngredient();
                    }

                    else if (equipmentStack.getItem() instanceof ArmorItem armorItem) {
                        getRepairItem = armorItem.getMaterial().value().repairIngredient().get();
                    }

                    assert getRepairItem != null;

                    if (itemHandler.getStackInSlot(OUTPUT_SLOT).isEmpty()) {

                        for (RecipeHolder<MeltingRecipe> recipeHolder : level.getRecipeManager().getAllRecipesFor(MeltingRecipe.Type.INSTANCE)) {
                            MeltingRecipe recipe = recipeHolder.value();

                            for (Ingredient recipeIngredient : recipe.getIngredients()) {
                                for (ItemStack repairStack : getRepairItem.getItems()) {

                                    if (recipeIngredient.test(repairStack)) {
                                        if (recipeIngredient.test(getRepairItem.getItems()[0])) {
                                            if (hasEnoughRepairFluid(equipmentStack, getResourceCount(equipmentStack), recipe.output())) {
                                                progress++;
                                                repairMode = true;

                                                if (progress >= maxProgress) {
                                                    extractFluid(recipe.output(), getFluidNeededForRepair(equipmentStack, getResourceCount(equipmentStack), recipe.output()));
                                                    itemHandler.setStackInSlot(TOOL_SLOT, ItemStack.EMPTY);

                                                    ItemStack restoredItem = equipmentStack.copy();
                                                    restoredItem.setDamageValue(0);
                                                    itemHandler.setStackInSlot(OUTPUT_SLOT, restoredItem);
                                                    sync();

                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                //end new

                if (!repairMode) {

                    // First pass: check 2-input recipes
                    for (RecipeHolder<EquipmentModifierRecipe> recipeHolder :
                            level.getRecipeManager().getRecipesFor(EquipmentModifierRecipe.Type.INSTANCE, inventory, level)) {

                        EquipmentModifierRecipe recipe = recipeHolder.value();
                        EquipmentModifier modifier = EquipmentModifier.valueOf(recipe.effect().toUpperCase(Locale.ROOT));


                        if (!validModifiers.isEmpty() && !validModifiers.contains(modifier)) {
                            continue;
                        }

                        if (!EquipmentModifierUtils.hasEnoughFreeModifiers(toolStack, modifier)) {
                            errorMessage = "not_high_enough_level"; // Not enough modifiers for the effect
                            continue; // Skip this recipe if not enough modifiers
                        }

                        if (recipe.requiresBothItemAndFluid()) {
                            if (hasEnoughFluid(recipe.upgradeFluid()) &&
                                    itemHandler.getStackInSlot(UPGRADE_ITEM_SLOT).getCount() >= recipe.upgradeItem().count() &&
                                    recipe.upgradeItem().test(itemHandler.getStackInSlot(UPGRADE_ITEM_SLOT))) {
                                matchedRecipe = recipe;
                                bothRequired = true;
                                sync();
                                break;
                            }
                        }
                    }

                    // Second pass: check 1-input recipes only if no 2-input match found
                    if (matchedRecipe == null) {
                        for (RecipeHolder<EquipmentModifierRecipe> recipeHolder :
                                level.getRecipeManager().getRecipesFor(EquipmentModifierRecipe.Type.INSTANCE, inventory, level)) {

                            EquipmentModifierRecipe recipe = recipeHolder.value();
                            EquipmentModifier modifier = EquipmentModifier.valueOf(recipe.effect().toUpperCase(Locale.ROOT));

                            if (!validModifiers.isEmpty() && !validModifiers.contains(modifier)) {
                                continue;
                            }

                            if (!EquipmentModifierUtils.hasEnoughFreeModifiers(toolStack, modifier)) {
                                errorMessage = "not_high_enough_level"; // Not enough modifiers for the effect
                                sync();
                                continue; // Skip this recipe if not enough modifiers
                            }

                            if (!recipe.requiresBothItemAndFluid()) {
                                if (itemHandler.getStackInSlot(UPGRADE_ITEM_SLOT).getCount() >= recipe.upgradeItem().count() &&
                                        recipe.upgradeItem().test(itemHandler.getStackInSlot(UPGRADE_ITEM_SLOT))) {
                                    matchedRecipe = recipe;
                                    itemOnly = true;
                                    break;
                                } else if (hasEnoughFluid(recipe.upgradeFluid())) {
                                    matchedRecipe = recipe;
                                    fluidOnly = true;
                                    break;
                                }
                            }
                        }
                    }

                    if (matchedRecipe != null) {
                        EquipmentModifier modifier = EquipmentModifier.valueOf(matchedRecipe.effect().toUpperCase(Locale.ROOT));
                        boolean effectMaxed = EquipmentModifierUtils.isEffectAtMax(toolStack, modifier);

                        if (effectMaxed) {
                            // Tool or effect is at max level
                            errorMessage = "at_max_level"; // If the effect is maxed, don't apply more modifiers
                            sync();
                        } else {
                            if (itemHandler.getStackInSlot(OUTPUT_SLOT).isEmpty()) {
                                progress++;
                                if (progress >= maxProgress) {
                                    if (bothRequired || fluidOnly) {
                                        extractFluid(matchedRecipe.upgradeFluid(), matchedRecipe.upgradeFluid().getAmount());
                                    }
                                    if (bothRequired || itemOnly) {
                                        itemHandler.extractItem(UPGRADE_ITEM_SLOT, matchedRecipe.upgradeItem().count(), false);
                                    }

                                    itemHandler.setStackInSlot(OUTPUT_SLOT, EquipmentModifierUtils.copyAndApplyEffect(toolStack, modifier));

                                    // Add Tool Level to confirm the tool is modified
                                    if (!itemHandler.getStackInSlot(OUTPUT_SLOT).has(EQUIPMENT_LEVEL.dataComponent.get())) {
                                        itemHandler.getStackInSlot(OUTPUT_SLOT).set((DataComponentType<Integer>) EQUIPMENT_LEVEL.dataComponent.value(), 1);
                                        itemHandler.getStackInSlot(OUTPUT_SLOT).set(EQUIPMENT_EXPERIENCE, 0);
                                    }

                                    itemHandler.setStackInSlot(TOOL_SLOT, ItemStack.EMPTY);
                                    resetProgress();
                                }
                            }

                            foundMatch = true;
                            errorMessage = ""; // Clear the error message when a match is found and operation is in progress
                            sync();
                        }
                    }
                }

                // If no valid match was found and errorMessage is still empty, set the "at_max_level" message
                if (!foundMatch && errorMessage.isEmpty() && !repairMode) {
                    errorMessage = "at_max_level"; // Tool has reached max level, no valid recipe found
                    resetProgress();

                }
            } else {
                resetProgress();
            }
        }
    }

    private static int getResourceCount(ItemStack item) {

        Item equipmentItem = item.getItem();

        if (mekanismPaxelClass != null && mekanismPaxelClass.isInstance(equipmentItem)) {
            return 7;
        }

        return switch (equipmentItem) {
            case PickaxeItem ignored -> 3;
            case AxeItem ignored -> 3;
            case ShovelItem ignored -> 1;
            case HoeItem ignored -> 2;
            case SwordItem ignored -> 2;
            case ArmorItem armorItem -> switch (armorItem.getType()) {
                case HELMET -> 5;
                case CHESTPLATE -> 8;
                case LEGGINGS -> 7;
                case BOOTS -> 4;
                case BODY -> 10;
            };
            default -> 10000;
        };
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
            return PAXEL_MODIFIERS;
        }

        return switch (item) {
            case PickaxeItem ignored -> PICKAXE_MODIFIERS;
            case AxeItem ignored -> AXE_MODIFIERS;
            case ShovelItem ignored -> SHOVEL_MODIFIERS;
            case HoeItem ignored -> HOE_MODIFIERS;
            case SwordItem ignored -> SWORD_MODIFIERS;
            case ShearsItem ignored -> SHEAR_MODIFIERS;
            case ArmorItem armorItem -> switch (armorItem.getType()) {
                case HELMET -> HELMET_MODIFIERS;
                case CHESTPLATE -> CHESTPLATE_MODIFIERS;
                case LEGGINGS -> LEGGINGS_MODIFIERS;
                case BOOTS -> BOOTS_MODIFIERS;
                case BODY -> BODY_MODIFIERS;
            };

            default -> "";
        };
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


    private int getFluidNeededForRepair(ItemStack equipmentStack, int resourceCount, FluidStack repairFluid) {
        int repairAmount = equipmentStack.getDamageValue();
        int fluidAmountPerResourceMelted = repairFluid.getAmount();
        int totalFluidCount = fluidAmountPerResourceMelted * resourceCount;

        float fluidPerDurability = (float) totalFluidCount / (float) equipmentStack.getMaxDamage();

        return Math.round(fluidPerDurability * repairAmount);
    }

    private boolean hasEnoughRepairFluid(ItemStack equipmentStack, int resourceCount, FluidStack repairFluid) {
        int totalFluidNeeded = getFluidNeededForRepair(equipmentStack, resourceCount, repairFluid);

        if (totalFluidNeeded == 0) {
            errorMessage = "";
            return true;
        }

        if (TANK.getFluid().getFluid() != repairFluid.getFluid()) {
            errorMessage = "wrong_fluid";
            return false;
        }

        if (TANK.getFluidAmount() >= totalFluidNeeded) {
            errorMessage = "";
            return true;
        } else {
            errorMessage = "not_enough_fluid";
            return false;
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

}