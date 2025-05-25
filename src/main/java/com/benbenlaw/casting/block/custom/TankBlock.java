package com.benbenlaw.casting.block.custom;

import com.benbenlaw.casting.block.entity.CastingBlockEntities;
import com.benbenlaw.casting.block.entity.TankBlockEntity;
import com.benbenlaw.casting.item.CastingDataComponents;
import com.benbenlaw.casting.item.CastingItems;
import com.mojang.serialization.MapCodec;
import net.minecraft.ChatFormatting;
import net.minecraft.client.gui.screens.Screen;
import net.minecraft.core.BlockPos;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.ItemInteractionResult;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.TooltipFlag;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.BaseEntityBlock;
import net.minecraft.world.level.block.RenderShape;
import net.minecraft.world.level.block.entity.BlockEntity;
import net.minecraft.world.level.block.entity.BlockEntityTicker;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.material.Fluid;
import net.minecraft.world.level.material.Fluids;
import net.minecraft.world.phys.BlockHitResult;
import net.neoforged.neoforge.fluids.FluidStack;
import net.neoforged.neoforge.fluids.FluidType;
import net.neoforged.neoforge.fluids.capability.IFluidHandler;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;
import java.util.Objects;

public class TankBlock extends BaseEntityBlock {

    public static final MapCodec<TankBlock> CODEC = simpleCodec(TankBlock::new);

    public TankBlock(Properties properties) {
        super(properties);
    }

    @Override
    protected @NotNull MapCodec<? extends BaseEntityBlock> codec() {
        return CODEC;
    }

    @Override
    protected @NotNull ItemInteractionResult useItemOn(ItemStack itemStack, BlockState blockState, Level level, BlockPos blockPos, Player player, InteractionHand hand, BlockHitResult hitResult) {

        if (itemStack.is(CastingItems.FLUID_MOVER.asItem())) {
            TankBlockEntity tankEntity = (TankBlockEntity) level.getBlockEntity(blockPos);
            assert tankEntity != null;
            Fluid tankFluid = tankEntity.getFluidStack().getFluid();
            int tankFluidAmount = tankEntity.getFluidStack().getAmount();

            if (itemStack.get(CastingDataComponents.FLUID_TYPE) != null && itemStack.get(CastingDataComponents.FLUID_AMOUNT) != null) {
                Fluid itemFluid = BuiltInRegistries.FLUID.get(ResourceLocation.tryParse(Objects.requireNonNull(itemStack.get(CastingDataComponents.FLUID_TYPE))));
                int fluidAmount = itemStack.get(CastingDataComponents.FLUID_AMOUNT);
                if (tankFluid.isSame(itemFluid) || tankFluid == Fluids.EMPTY) {
                    int space = tankEntity.FLUID_TANK.getSpace();
                    int amountToTransfer = Math.min(space, fluidAmount); // Ensure we don't transfer more than the tank can hold
                    tankEntity.FLUID_TANK.fill(new FluidStack(itemFluid, amountToTransfer), IFluidHandler.FluidAction.EXECUTE);
                    itemStack.set(CastingDataComponents.FLUID_AMOUNT, fluidAmount - amountToTransfer);
                    if (fluidAmount - amountToTransfer == 0) {
                        itemStack.remove(CastingDataComponents.FLUID_TYPE);
                        itemStack.remove(CastingDataComponents.FLUID_AMOUNT);
                    }
                    return ItemInteractionResult.SUCCESS;
                }
            }

            if (itemStack.get(CastingDataComponents.FLUID_TYPE) == null || itemStack.get(CastingDataComponents.FLUID_AMOUNT) == null) {
                if (tankFluid != Fluids.EMPTY) {
                    int currentFluidAmountInItem = itemStack.get(CastingDataComponents.FLUID_AMOUNT) != null ? itemStack.get(CastingDataComponents.FLUID_AMOUNT) : 0;
                    int remainingSpaceInItem = 8000 - currentFluidAmountInItem;
                    int amountToExtract = Math.min(tankFluidAmount, remainingSpaceInItem);
                    if (amountToExtract > 0) {
                        FluidStack extractedFluid = tankEntity.FLUID_TANK.drain(amountToExtract, IFluidHandler.FluidAction.EXECUTE);
                        itemStack.set(CastingDataComponents.FLUID_TYPE, BuiltInRegistries.FLUID.getKey(extractedFluid.getFluid()).toString());
                        itemStack.set(CastingDataComponents.FLUID_AMOUNT, extractedFluid.getAmount());
                        return ItemInteractionResult.SUCCESS;
                    }
                }
            }

        }
        else {
            TankBlockEntity tankBlockEntity = (TankBlockEntity) level.getBlockEntity(blockPos);
            if (tankBlockEntity != null && tankBlockEntity.onPlayerUse(player, InteractionHand.MAIN_HAND)) {
                return ItemInteractionResult.SUCCESS;
            }
        }

        return ItemInteractionResult.FAIL;
    }

    @Override
    public void setPlacedBy(Level level, BlockPos blockPos, BlockState blockState, @Nullable LivingEntity entity, ItemStack itemStack) {
        super.setPlacedBy(level, blockPos, blockState, entity, itemStack);

        if (itemStack.has(CastingDataComponents.FLUID_TYPE) && itemStack.has(CastingDataComponents.FLUID_AMOUNT)) {
            String fluidAsString = itemStack.get(CastingDataComponents.FLUID_TYPE);
            assert fluidAsString != null;
            Fluid fluid = BuiltInRegistries.FLUID.get(ResourceLocation.tryParse(fluidAsString));
            int fluidAmount = itemStack.get(CastingDataComponents.FLUID_AMOUNT);
            TankBlockEntity tankEntity = (TankBlockEntity) level.getBlockEntity(blockPos);
            tankEntity.setFluid(new FluidStack(fluid, fluidAmount));
        }
    }


    @Override
    public void playerDestroy(Level level, Player player, BlockPos pos, BlockState state, @Nullable BlockEntity entity, ItemStack stack) {
        if (entity instanceof TankBlockEntity tankEntity) {

            if (tankEntity.getFluidStack().getFluid() != Fluids.EMPTY && tankEntity.getFluidStack().getAmount() > 0 ){
                ItemStack itemStackWithFluid = new ItemStack(this);
                itemStackWithFluid.set(CastingDataComponents.FLUID_TYPE, tankEntity.getFluidStack().getFluid().getFluidType().toString());
                itemStackWithFluid.set(CastingDataComponents.FLUID_AMOUNT, tankEntity.getFluidStack().getAmount());
                popResource(level, pos, itemStackWithFluid);
            } else {
                popResource(level, pos, this.asItem().getDefaultInstance());
            }
        }
        super.playerDestroy(level, player, pos, state, entity, stack);
    }

    @Override
    public void appendHoverText(ItemStack itemStack, Item.@NotNull TooltipContext context, @NotNull List<Component> components, @NotNull TooltipFlag flag) {

        if (Screen.hasShiftDown()) {
            if (itemStack.has(CastingDataComponents.FLUID_TYPE)) {
                String fluidAsString = itemStack.get(CastingDataComponents.FLUID_TYPE);
                int fluidAmount = itemStack.get(CastingDataComponents.FLUID_AMOUNT);
                assert fluidAsString != null;
                FluidType fluid = BuiltInRegistries.FLUID.get(ResourceLocation.tryParse(fluidAsString)).getFluidType();
                components.add(Component.literal("Contains: ").append(fluidAmount + "mb ").append(Component.translatable(fluid.getDescriptionId())).withStyle(ChatFormatting.GREEN));
            }
        }

        else if (itemStack.has(CastingDataComponents.FLUID_TYPE)) {
            components.add(Component.translatable("tooltips.bblcore.shift").withStyle(ChatFormatting.YELLOW));
        }

        super.appendHoverText(itemStack, context, components, flag);

    }

    //BLOCK ENTITY


    @SuppressWarnings("deprecation")
    @Override
    public @NotNull RenderShape getRenderShape(@NotNull BlockState blockState) {
        return RenderShape.MODEL;
    }

    @Override
    public void onRemove(BlockState blockState, @NotNull Level level, @NotNull BlockPos blockPos, BlockState newBlockState, boolean isMoving) {
        blockState.getBlock();
        newBlockState.getBlock();
        super.onRemove(blockState, level, blockPos, newBlockState, isMoving);
    }

    @Nullable
    @Override
    public BlockEntity newBlockEntity(@NotNull BlockPos blockPos, @NotNull BlockState blockState) {
        return new TankBlockEntity(blockPos, blockState);
    }

    @Nullable
    @Override
    public <T extends BlockEntity> BlockEntityTicker<T> getTicker(@NotNull Level level, @NotNull BlockState blockState, @NotNull BlockEntityType<T> blockEntityType) {
        return createTickerHelper(blockEntityType, CastingBlockEntities.TANK_BLOCK_ENTITY.get(),
                (world, blockPos, thisBlockState, blockEntity) -> blockEntity.tick());
    }
}
