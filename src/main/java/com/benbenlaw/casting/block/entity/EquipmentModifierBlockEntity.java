package com.benbenlaw.casting.block.entity;

import com.benbenlaw.casting.config.EquipmentModifierConfig;
import com.benbenlaw.casting.item.CastingDataComponents;
import com.benbenlaw.casting.item.ModItems;
import com.benbenlaw.casting.recipe.EquipmentModifierRecipe;
import com.benbenlaw.casting.recipe.MeltingRecipe;
import com.benbenlaw.casting.recipe.SolidifierRecipe;
import com.benbenlaw.casting.screen.EquipmentModifierMenu;
import com.benbenlaw.casting.util.CastingTags;
import com.benbenlaw.casting.util.EquipmentModifierUtils;
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
import net.minecraft.world.item.crafting.*;
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

    private ItemStack lastToolStack = ItemStack.EMPTY.copy();
    public String errorMessage = "";
    public boolean isLimitMode = false;
    private final IItemHandler equipmentModifierItemHandler = new InputOutputItemHandler(itemHandler,
            (i, stack) -> i == 0 ,  //
            i -> i == 1
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
        super(ModBlockEntities.EQUIPMENT_MODIFIER_BLOCK_ENTITY.get(), pos, state);
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
    public Component getDisplayName() {
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
        if (level.isClientSide()) return;

        if (level.getGameTime() % 20 == 0) {
            sync();
        }

        ItemStack currentToolStack = itemHandler.getStackInSlot(TOOL_SLOT);

        if (ItemStack.matches(currentToolStack, lastToolStack)) {
            return;
        }

        lastToolStack = currentToolStack.copy();

        if (!isToolValid()) {
            resetProgress();
            return;
        }

        if (currentToolStack.isEmpty()) {
            resetProgress();
            return;
        }

        if (isRepairing(currentToolStack)) {
            handleRepair(currentToolStack);
            return;
        }

        RecipeInput inventory = createInventory();

        String toolType = getToolType(currentToolStack);
        List<String> validModifiers = VALID_MODIFIERS.getOrDefault(toolType, List.of());

        handleModifierApplication(currentToolStack, inventory, validModifiers);
    }

    private RecipeInput createInventory() {
        return new RecipeInput() {
            @Override
            public @NotNull ItemStack getItem(int index) {
                return itemHandler.getStackInSlot(index);
            }

            @Override
            public int size() {
                return itemHandler.getSlots();
            }
        };
    }

    private boolean isToolValid() {
        Item item = itemHandler.getStackInSlot(TOOL_SLOT).getItem();
        return item instanceof TieredItem || item instanceof ShearsItem || item instanceof ArmorItem;
    }

    private boolean isRepairing(ItemStack toolStack) {
        return itemHandler.getStackInSlot(UPGRADE_ITEM_SLOT).is(ModItems.REPAIRING_MOLD) && !toolStack.isEmpty();
    }

    private void handleRepair(ItemStack toolStack) {
        Ingredient repairIngredient = getRepairIngredient(toolStack.getItem());
        if (repairIngredient == null || !itemHandler.getStackInSlot(OUTPUT_SLOT).isEmpty()) {
            resetProgress();
            return;
        }

        for (RecipeHolder<MeltingRecipe> holder : level.getRecipeManager().getAllRecipesFor(MeltingRecipe.Type.INSTANCE)) {
            MeltingRecipe recipe = holder.value();

            for (ItemStack repairStack : repairIngredient.getItems()) {
                if (!recipe.getIngredients().stream().anyMatch(ing -> ing.test(repairStack))) continue;
                if (!hasEnoughRepairFluid(toolStack, getResourceCount(toolStack), recipe.output())) continue;

                progress++;
                if (progress >= maxProgress) {
                    extractFluid(recipe.output(), getFluidNeededForRepair(toolStack, getResourceCount(toolStack), recipe.output()));
                    itemHandler.setStackInSlot(TOOL_SLOT, ItemStack.EMPTY);

                    ItemStack restoredItem = toolStack.copy();
                    restoredItem.setDamageValue(0);
                    itemHandler.setStackInSlot(OUTPUT_SLOT, restoredItem);
                    resetProgress();
                }
                return;
            }
        }
    }

    private Ingredient getRepairIngredient(Item item) {
        if (item instanceof TieredItem tiered) {
            return tiered.getTier().getRepairIngredient();
        } else if (item instanceof ArmorItem armor) {
            return armor.getMaterial().value().repairIngredient().get();
        }
        return null;
    }

    private void handleModifierApplication(ItemStack toolStack, RecipeInput inventory, List<String> validModifiers) {

        if (validModifiers.isEmpty()) {
            errorMessage = "no_valid_modifiers";
            resetProgress();
            return;
        }

        EquipmentModifierRecipe matchedRecipe = null;
        boolean bothRequired = false, itemOnly = false, fluidOnly = false;

        // First pass: recipes that require both item and fluid
        for (RecipeHolder<EquipmentModifierRecipe> holder : level.getRecipeManager()
                .getRecipesFor(EquipmentModifierRecipe.Type.INSTANCE, inventory, level)) {

            EquipmentModifierRecipe recipe = holder.value();
            if (!isValidRecipe(recipe, toolStack, validModifiers)) continue;

            if (recipe.requiresBothItemAndFluid() && canApplyBoth(recipe)) {
                matchedRecipe = recipe;
                bothRequired = true;
                break;
            }
        }

        // Second pass: item-only or fluid-only if no match
        if (matchedRecipe == null) {
            for (RecipeHolder<EquipmentModifierRecipe> holder : level.getRecipeManager()
                    .getRecipesFor(EquipmentModifierRecipe.Type.INSTANCE, inventory, level)) {

                EquipmentModifierRecipe recipe = holder.value();
                if (!isValidRecipe(recipe, toolStack, validModifiers)) continue;

                if (!recipe.requiresBothItemAndFluid()) {
                    if (canApplyItemOnly(recipe)) {
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
            applyModifier(toolStack, matchedRecipe, bothRequired, itemOnly, fluidOnly);
        } else {
            errorMessage = "at_max_level";
            resetProgress();
        }
    }

    private boolean isValidRecipe(EquipmentModifierRecipe recipe, ItemStack toolStack, List<String> validModifiers) {
        String effect = recipe.effect();
        if (!validModifiers.isEmpty() && !validModifiers.contains(effect)) return false;
        if (!EquipmentModifierUtils.hasEnoughFreeModifiers(toolStack, effect)) {
            errorMessage = "not_high_enough_level";
            return false;
        }
        return true;
    }

    private boolean canApplyBoth(EquipmentModifierRecipe recipe) {
        return hasEnoughFluid(recipe.upgradeFluid())
                && itemHandler.getStackInSlot(UPGRADE_ITEM_SLOT).getCount() >= recipe.upgradeItem().count()
                && recipe.upgradeItem().test(itemHandler.getStackInSlot(UPGRADE_ITEM_SLOT));
    }

    private boolean canApplyItemOnly(EquipmentModifierRecipe recipe) {
        return itemHandler.getStackInSlot(UPGRADE_ITEM_SLOT).getCount() >= recipe.upgradeItem().count()
                && recipe.upgradeItem().test(itemHandler.getStackInSlot(UPGRADE_ITEM_SLOT));
    }

    private void applyModifier(ItemStack toolStack, EquipmentModifierRecipe recipe, boolean both, boolean itemOnly, boolean fluidOnly) {
        if (EquipmentModifierUtils.isEffectAtMax(toolStack, recipe.effect())) {
            errorMessage = "at_max_level";
            return;
        }

        if (!itemHandler.getStackInSlot(OUTPUT_SLOT).isEmpty()) return;

        progress++;
        if (progress < maxProgress) return;

        if (both || fluidOnly) {
            extractFluid(recipe.upgradeFluid(), recipe.upgradeFluid().getAmount());
        }

        if (both || itemOnly) {
            itemHandler.extractItem(UPGRADE_ITEM_SLOT, recipe.upgradeItem().count(), false);
        }

        ItemStack result = EquipmentModifierUtils.copyAndApplyEffect(toolStack, recipe.effect());
        if (!result.has(CastingDataComponents.EQUIPMENT_LEVEL)) {
            result.set(CastingDataComponents.EQUIPMENT_LEVEL, 1);
            result.set(CastingDataComponents.EQUIPMENT_EXPERIENCE, 1);
        }

        itemHandler.setStackInSlot(OUTPUT_SLOT, result);
        itemHandler.setStackInSlot(TOOL_SLOT, ItemStack.EMPTY);
        resetProgress();
        errorMessage = ""; // clear error if successful
    }
    private static int getResourceCount(ItemStack item) {

        Item equipmentItem = item.getItem();

        if (mekanismPaxelClass != null && mekanismPaxelClass.isInstance(equipmentItem)) {
            return 7;
        }

        return switch (equipmentItem) {
            case PickaxeItem pickaxeItem -> 3;
            case AxeItem axeItem -> 3;
            case ShovelItem shovelItem -> 1;
            case HoeItem hoeItem -> 2;
            case SwordItem swordItem -> 2;
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
            case PickaxeItem pickaxeItem -> PICKAXE_MODIFIERS;
            case AxeItem axeItem -> AXE_MODIFIERS;
            case ShovelItem shovelItem -> SHOVEL_MODIFIERS;
            case HoeItem hoeItem -> HOE_MODIFIERS;
            case SwordItem swordItem -> SWORD_MODIFIERS;
            case ShearsItem shearItem -> SHEAR_MODIFIERS;
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

        //System.out.println("total fluid fro full repair: " + totalFluidCount);
        //System.out.println("Fluid per durability: " + fluidPerDurability);
        //System.out.println("Repair amount: " + repairAmount);
        //System.out.println("Fluid needed for repair: " + (fluidPerDurability * repairAmount));
        //System.out.println("Fluid needed for repair: " + Math.round(fluidPerDurability * repairAmount));

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


    private boolean isRecipeSlotsValidForTanks(SolidifierRecipe recipe) {
        FluidStack recipeFluid = recipe.fluid();
        return TANK.getFluid().is(recipeFluid.getFluidType()) && (tankIsValidForSlot(recipeFluid, 0) || tankIsValidForSlot(recipeFluid, 1));
    }

    private boolean tankIsValidForSlot(FluidStack stack, int slot) {

        return stack.getFluid() == TANK.getFluid().getFluid();
    }

    private boolean hasOutputSpaceMaking(EquipmentModifierBlockEntity entity, SolidifierRecipe recipe) {
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