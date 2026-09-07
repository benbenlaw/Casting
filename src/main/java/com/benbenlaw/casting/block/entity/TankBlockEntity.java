package com.benbenlaw.casting.block.entity;

import com.benbenlaw.casting.block.CastingBlockEntities;
import com.benbenlaw.casting.block.custom.TankBlock;
import com.benbenlaw.casting.item.CastingDataComponents;
import com.benbenlaw.casting.item.FluidMoverItem;
import com.benbenlaw.casting.item.util.FluidListComponent;
import com.benbenlaw.casting.recipe.custom.FuelRecipe;
import com.benbenlaw.core.block.entity.SyncableBlockEntity;
import com.benbenlaw.core.block.entity.handler.fluid.SyncableFluidHandler;
import net.minecraft.core.BlockPos;
import net.minecraft.core.component.DataComponentGetter;
import net.minecraft.core.component.DataComponentMap;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.crafting.RecipeHolder;
import net.minecraft.world.item.crafting.RecipeManager;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.storage.ValueInput;
import net.minecraft.world.level.storage.ValueOutput;
import net.neoforged.neoforge.fluids.FluidStack;
import net.neoforged.neoforge.transfer.fluid.FluidStacksResourceHandler;
import net.neoforged.neoforge.transfer.fluid.FluidUtil;
import net.neoforged.neoforge.transfer.transaction.Transaction;
import org.jspecify.annotations.NonNull;

import java.util.OptionalInt;

public class TankBlockEntity extends SyncableBlockEntity {

    private final SyncableFluidHandler fluidInventory = new SyncableFluidHandler(this, 1, 4000, (i, stack) -> i == 0, i -> i == 0) {
        @Override
        protected void onContentsChanged(int index, FluidStack previousContents) {
            FluidStack current = FluidUtil.getStack(this, index);
            if (!FluidStack.isSameFluidSameComponents(current, previousContents)) {
                fuelRecipeDirty = true;
            }
            super.onContentsChanged(index, previousContents);
        }
    };

    private RecipeHolder<FuelRecipe> cachedFuelRecipe = null;
    private boolean fuelRecipeDirty = true;
    private RecipeManager lastRecipeManager = null;

    public TankBlockEntity(BlockPos pos, BlockState state) {
        super(CastingBlockEntities.TANK_BLOCK_ENTITY.get(), pos, state);
    }

    public void tick() {
        assert level != null;
        if (!level.isClientSide()) {
            if (!level.getBlockState(worldPosition).getValue(TankBlock.RUNNING)) return;
        }
    }

    public OptionalInt getFuelTemp() {
        FluidStack stack = FluidUtil.getStack(fluidInventory, 0);
        if (stack.isEmpty()) {
            cachedFuelRecipe = null;
            fuelRecipeDirty = false;
            return OptionalInt.empty();
        }

        if (level != null && level.getServer() != null) {
            RecipeManager current = level.getServer().getRecipeManager();
            if (current != lastRecipeManager) {
                fuelRecipeDirty = true;
                lastRecipeManager = current;
            }
        }

        if (fuelRecipeDirty) {
            cachedFuelRecipe = getFuel(level, stack);
            fuelRecipeDirty = false;
        }

        if (cachedFuelRecipe == null) return OptionalInt.empty();

        return OptionalInt.of(cachedFuelRecipe.value().temp());
    }

    public FluidStacksResourceHandler getFluidHandler() {
        return fluidInventory;
    }

    public static RecipeHolder<FuelRecipe> getFuel(Level level, FluidStack stack) {
        if (level == null || level.getServer() == null || stack.isEmpty()) return null;

        for (RecipeHolder<FuelRecipe> holder : level.getServer().getRecipeManager().recipeMap().byType(FuelRecipe.TYPE)) {
            if (holder.value().fluid().ingredient().test(stack)) return holder;
        }
        return null;
    }

    @Override
    protected void saveAdditional(ValueOutput output) {
        fluidInventory.serialize(output.child("fluidInventory"));
        super.saveAdditional(output);
    }


    @Override
    protected void loadAdditional(ValueInput input) {
        fluidInventory.deserialize(input.childOrEmpty("fluidInventory"));
        super.loadAdditional(input);
    }

    @Override
    public void preRemoveSideEffects(BlockPos pos, BlockState state) {

    }

    public boolean onPlayerUse(Player player, InteractionHand hand) {
        ItemStack stack = player.getItemInHand(hand);

        if (stack.getItem() instanceof FluidMoverItem) {
            return FluidMoverItem.onBlockInteract(stack, fluidInventory, new int[]{0}, new int[]{0});
        }

        try (Transaction tx = Transaction.open(null)) {
            boolean result = FluidUtil.interactWithFluidHandler(player, hand, this.worldPosition, fluidInventory, tx);
            if (result) {
                tx.commit();
            }
            return result;
        }
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
            component.applyToHandlers(fluidInventory);
        }
    }
}