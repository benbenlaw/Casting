package com.benbenlaw.casting.block.custom;

import com.benbenlaw.casting.block.entity.CastingBlockEntities;
import com.benbenlaw.casting.block.entity.MixerBlockEntity;
import com.benbenlaw.casting.item.CastingDataComponents;
import com.benbenlaw.casting.screen.MixerMenu;
import com.mojang.serialization.MapCodec;
import net.minecraft.ChatFormatting;
import net.minecraft.client.gui.screens.Screen;
import net.minecraft.core.BlockPos;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.InteractionResult;
import net.minecraft.world.SimpleMenuProvider;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.inventory.ContainerData;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.TooltipFlag;
import net.minecraft.world.item.context.BlockPlaceContext;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.*;
import net.minecraft.world.level.block.entity.BlockEntity;
import net.minecraft.world.level.block.entity.BlockEntityTicker;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.block.state.StateDefinition;
import net.minecraft.world.level.block.state.properties.BlockStateProperties;
import net.minecraft.world.level.block.state.properties.DirectionProperty;
import net.minecraft.world.level.material.Fluid;
import net.minecraft.world.level.material.Fluids;
import net.minecraft.world.phys.BlockHitResult;
import net.neoforged.neoforge.fluids.FluidStack;
import net.neoforged.neoforge.fluids.FluidType;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;

public class MixerBlock extends BaseEntityBlock {

    public static final MapCodec<MixerBlock> CODEC = simpleCodec(MixerBlock::new);

    @Override
    protected @NotNull MapCodec<? extends BaseEntityBlock> codec() {
        return CODEC;
    }

    public static final DirectionProperty FACING = BlockStateProperties.HORIZONTAL_FACING;

    /* FACING */
    @Override
    public BlockState getStateForPlacement(BlockPlaceContext pContext) {
        return this.defaultBlockState().setValue(FACING, pContext.getHorizontalDirection().getOpposite());
    }

    @Override
    public @NotNull BlockState rotate(BlockState pState, Rotation pRotation) {
        return pState.setValue(FACING, pRotation.rotate(pState.getValue(FACING)));
    }

    @Override
    public @NotNull BlockState mirror(BlockState pState, Mirror pMirror) {
        return pState.rotate(pMirror.getRotation(pState.getValue(FACING)));
    }

    @Override
    protected void createBlockStateDefinition(StateDefinition.Builder<Block, BlockState> pBuilder) {
        pBuilder.add(FACING);
    }

    public MixerBlock(Properties properties) {
        super(properties);
    }

    @Override
    public RenderShape getRenderShape(BlockState pState) {
        return RenderShape.MODEL;
    }

    @Override
    public void onRemove(BlockState pState, Level pLevel, BlockPos pPos, BlockState pNewState, boolean pIsMoving) {
        if (pState.getBlock() != pNewState.getBlock()) {
            BlockEntity blockEntity = pLevel.getBlockEntity(pPos);
            if (blockEntity instanceof MixerBlockEntity) {
                ((MixerBlockEntity) blockEntity).drops();
            }
        }
        super.onRemove(pState, pLevel, pPos, pNewState, pIsMoving);
    }

    @Override
    public @NotNull InteractionResult useWithoutItem(@NotNull BlockState blockState, Level level, @NotNull BlockPos blockPos, @NotNull Player player, @NotNull BlockHitResult hit) {

        if (level.isClientSide()) {
            return InteractionResult.SUCCESS;
        }

        if (!level.isClientSide()) {

            MixerBlockEntity mixerBlockEntity = (MixerBlockEntity) level.getBlockEntity(blockPos);


            if (mixerBlockEntity instanceof MixerBlockEntity) {

                //Use Bucket First else open menu not both

                if (mixerBlockEntity.onPlayerUse(player, InteractionHand.MAIN_HAND)) {
                    return InteractionResult.SUCCESS;
                }

                else {
                    ContainerData data = mixerBlockEntity.data;
                    player.openMenu(new SimpleMenuProvider(
                            (windowId, playerInventory, playerEntity) -> new MixerMenu(windowId, playerInventory, blockPos, data),
                            Component.translatable("block.casting.mixer")), (buf -> buf.writeBlockPos(blockPos)));
                }
            }


            return InteractionResult.SUCCESS;
        }
        return InteractionResult.FAIL;
    }

    @Override
    public void setPlacedBy(Level level, BlockPos blockPos, BlockState blockState, @Nullable LivingEntity entity, ItemStack itemStack) {
        super.setPlacedBy(level, blockPos, blockState, entity, itemStack);

        if (level.getBlockEntity(blockPos) instanceof MixerBlockEntity mixerBlockEntity) {

            if (itemStack.has(CastingDataComponents.FLUID_TYPE_1) && itemStack.has(CastingDataComponents.FLUID_AMOUNT_1)) {
                String fluidAsString1 = itemStack.get(CastingDataComponents.FLUID_TYPE_1);
                Fluid fluid1 = BuiltInRegistries.FLUID.get(ResourceLocation.tryParse(fluidAsString1));
                int fluidAmount1 = itemStack.get(CastingDataComponents.FLUID_AMOUNT_1);
                mixerBlockEntity.setFluid1(new FluidStack(fluid1, fluidAmount1));
            }

            if (itemStack.has(CastingDataComponents.FLUID_TYPE_2) && itemStack.has(CastingDataComponents.FLUID_AMOUNT_2)) {
                String fluidAsString2 = itemStack.get(CastingDataComponents.FLUID_TYPE_2);
                Fluid fluid2 = BuiltInRegistries.FLUID.get(ResourceLocation.tryParse(fluidAsString2));
                int fluidAmount2 = itemStack.get(CastingDataComponents.FLUID_AMOUNT_2);
                mixerBlockEntity.setFluid2(new FluidStack(fluid2, fluidAmount2));
            }

            if (itemStack.has(CastingDataComponents.FLUID_TYPE_3) && itemStack.has(CastingDataComponents.FLUID_AMOUNT_3)) {
                String fluidAsString3 = itemStack.get(CastingDataComponents.FLUID_TYPE_3);
                Fluid fluid3 = BuiltInRegistries.FLUID.get(ResourceLocation.tryParse(fluidAsString3));
                int fluidAmount3 = itemStack.get(CastingDataComponents.FLUID_AMOUNT_3);
                mixerBlockEntity.setFluid3(new FluidStack(fluid3, fluidAmount3));
            }

            if (itemStack.has(CastingDataComponents.FLUID_TYPE_4) && itemStack.has(CastingDataComponents.FLUID_AMOUNT_4)) {
                String fluidAsString4 = itemStack.get(CastingDataComponents.FLUID_TYPE_4);
                Fluid fluid4 = BuiltInRegistries.FLUID.get(ResourceLocation.tryParse(fluidAsString4));
                int fluidAmount4 = itemStack.get(CastingDataComponents.FLUID_AMOUNT_4);
                mixerBlockEntity.setFluid4(new FluidStack(fluid4, fluidAmount4));
            }

            if (itemStack.has(CastingDataComponents.FLUID_TYPE_5) && itemStack.has(CastingDataComponents.FLUID_AMOUNT_5)) {
                String fluidAsString5 = itemStack.get(CastingDataComponents.FLUID_TYPE_5);
                Fluid fluid5 = BuiltInRegistries.FLUID.get(ResourceLocation.tryParse(fluidAsString5));
                int fluidAmount5 = itemStack.get(CastingDataComponents.FLUID_AMOUNT_5);
                mixerBlockEntity.setFluid5(new FluidStack(fluid5, fluidAmount5));
            }

            if (itemStack.has(CastingDataComponents.FLUID_TYPE_6) && itemStack.has(CastingDataComponents.FLUID_AMOUNT_6)) {
                String fluidAsString6 = itemStack.get(CastingDataComponents.FLUID_TYPE_6);
                Fluid fluid6 = BuiltInRegistries.FLUID.get(ResourceLocation.tryParse(fluidAsString6));
                int fluidAmount6 = itemStack.get(CastingDataComponents.FLUID_AMOUNT_6);
                mixerBlockEntity.setFluid6(new FluidStack(fluid6, fluidAmount6));
            }

            if (itemStack.has(CastingDataComponents.OUTPUT_FLUID_1) && itemStack.has(CastingDataComponents.OUTPUT_FLUID_AMOUNT_1)) {
                String outputFluidAsString = itemStack.get(CastingDataComponents.OUTPUT_FLUID_1);
                Fluid outputFluid = BuiltInRegistries.FLUID.get(ResourceLocation.tryParse(outputFluidAsString));
                int outputFluidAmount = itemStack.get(CastingDataComponents.OUTPUT_FLUID_AMOUNT_1);
                mixerBlockEntity.setOutputFluid(new FluidStack(outputFluid, outputFluidAmount));
            }
        }
    }



    @Override
    public void playerDestroy(Level level, Player player, BlockPos pos, BlockState state, @Nullable BlockEntity entity, ItemStack stack) {
        if (entity instanceof MixerBlockEntity mixerBlockEntity) {

            ItemStack itemStackWithFluids = new ItemStack(this);

            if (mixerBlockEntity.getFluidStack1().getFluid() != Fluids.EMPTY && mixerBlockEntity.getFluidStack1().getAmount() > 0) {
                itemStackWithFluids.set(CastingDataComponents.FLUID_TYPE_1, mixerBlockEntity.getFluidStack1().getFluid().getFluidType().toString());
                itemStackWithFluids.set(CastingDataComponents.FLUID_AMOUNT_1, mixerBlockEntity.getFluidStack1().getAmount());
            }
            if (mixerBlockEntity.getFluidStack2().getFluid() != Fluids.EMPTY && mixerBlockEntity.getFluidStack2().getAmount() > 0) {
                itemStackWithFluids.set(CastingDataComponents.FLUID_TYPE_2, mixerBlockEntity.getFluidStack2().getFluid().getFluidType().toString());
                itemStackWithFluids.set(CastingDataComponents.FLUID_AMOUNT_2, mixerBlockEntity.getFluidStack2().getAmount());
            }
            if (mixerBlockEntity.getFluidStack3().getFluid() != Fluids.EMPTY && mixerBlockEntity.getFluidStack3().getAmount() > 0) {
                itemStackWithFluids.set(CastingDataComponents.FLUID_TYPE_3, mixerBlockEntity.getFluidStack3().getFluid().getFluidType().toString());
                itemStackWithFluids.set(CastingDataComponents.FLUID_AMOUNT_3, mixerBlockEntity.getFluidStack3().getAmount());
            }
            if (mixerBlockEntity.getFluidStack4().getFluid() != Fluids.EMPTY && mixerBlockEntity.getFluidStack4().getAmount() > 0) {
                itemStackWithFluids.set(CastingDataComponents.FLUID_TYPE_4, mixerBlockEntity.getFluidStack4().getFluid().getFluidType().toString());
                itemStackWithFluids.set(CastingDataComponents.FLUID_AMOUNT_4, mixerBlockEntity.getFluidStack4().getAmount());
            }
            if (mixerBlockEntity.getFluidStack5().getFluid() != Fluids.EMPTY && mixerBlockEntity.getFluidStack5().getAmount() > 0) {
                itemStackWithFluids.set(CastingDataComponents.FLUID_TYPE_5, mixerBlockEntity.getFluidStack5().getFluid().getFluidType().toString());
                itemStackWithFluids.set(CastingDataComponents.FLUID_AMOUNT_5, mixerBlockEntity.getFluidStack5().getAmount());
            }
            if (mixerBlockEntity.getFluidStack6().getFluid() != Fluids.EMPTY && mixerBlockEntity.getFluidStack6().getAmount() > 0) {
                itemStackWithFluids.set(CastingDataComponents.FLUID_TYPE_6, mixerBlockEntity.getFluidStack6().getFluid().getFluidType().toString());
                itemStackWithFluids.set(CastingDataComponents.FLUID_AMOUNT_6, mixerBlockEntity.getFluidStack6().getAmount());
            }
            if (mixerBlockEntity.getOutputFluid().getFluid() != Fluids.EMPTY && mixerBlockEntity.getOutputFluid().getAmount() > 0) {
                itemStackWithFluids.set(CastingDataComponents.OUTPUT_FLUID_1, mixerBlockEntity.getOutputFluid().getFluid().getFluidType().toString());
                itemStackWithFluids.set(CastingDataComponents.OUTPUT_FLUID_AMOUNT_1, mixerBlockEntity.getOutputFluid().getAmount());
            }
            popResource(level, pos, itemStackWithFluids);

        } else {
            popResource(level, pos, this.asItem().getDefaultInstance());
        }

        super.playerDestroy(level, player, pos, state, entity, stack);
    }


    @Override
    public void appendHoverText(ItemStack itemStack, @NotNull Item.TooltipContext context, @NotNull List<Component> components, @NotNull TooltipFlag flag) {

        if(Screen.hasShiftDown()) {

            if (itemStack.has(CastingDataComponents.FLUID_TYPE_1) && itemStack.has(CastingDataComponents.FLUID_AMOUNT_1)) {
                String fluidAsString1 = itemStack.get(CastingDataComponents.FLUID_TYPE_1);
                int fluidAmount1 = itemStack.get(CastingDataComponents.FLUID_AMOUNT_1);
                FluidType fluid1 = BuiltInRegistries.FLUID.get(ResourceLocation.tryParse(fluidAsString1)).getFluidType();
                components.add(Component.literal("Tank 1 Contains: ").append(fluidAmount1 + "mb ").append(Component.translatable(fluid1.getDescriptionId())).withStyle(ChatFormatting.GREEN));
            }

            if (itemStack.has(CastingDataComponents.FLUID_TYPE_2) && itemStack.has(CastingDataComponents.FLUID_AMOUNT_2)) {
                String fluidAsString2 = itemStack.get(CastingDataComponents.FLUID_TYPE_2);
                int fluidAmount2 = itemStack.get(CastingDataComponents.FLUID_AMOUNT_2);
                FluidType fluid2 = BuiltInRegistries.FLUID.get(ResourceLocation.tryParse(fluidAsString2)).getFluidType();
                components.add(Component.literal("Tank 2 Contains: ").append(fluidAmount2 + "mb ").append(Component.translatable(fluid2.getDescriptionId())).withStyle(ChatFormatting.GREEN));
            }

            if (itemStack.has(CastingDataComponents.FLUID_TYPE_3) && itemStack.has(CastingDataComponents.FLUID_AMOUNT_3)) {
                String fluidAsString3 = itemStack.get(CastingDataComponents.FLUID_TYPE_3);
                int fluidAmount3 = itemStack.get(CastingDataComponents.FLUID_AMOUNT_3);
                FluidType fluid3 = BuiltInRegistries.FLUID.get(ResourceLocation.tryParse(fluidAsString3)).getFluidType();
                components.add(Component.literal("Tank 3 Contains: ").append(fluidAmount3 + "mb ").append(Component.translatable(fluid3.getDescriptionId())).withStyle(ChatFormatting.GREEN));
            }

            if (itemStack.has(CastingDataComponents.FLUID_TYPE_4) && itemStack.has(CastingDataComponents.FLUID_AMOUNT_4)) {
                String fluidAsString4 = itemStack.get(CastingDataComponents.FLUID_TYPE_4);
                int fluidAmount4 = itemStack.get(CastingDataComponents.FLUID_AMOUNT_4);
                FluidType fluid4 = BuiltInRegistries.FLUID.get(ResourceLocation.tryParse(fluidAsString4)).getFluidType();
                components.add(Component.literal("Tank 4 Contains: ").append(fluidAmount4 + "mb ").append(Component.translatable(fluid4.getDescriptionId())).withStyle(ChatFormatting.GREEN));
            }

            if (itemStack.has(CastingDataComponents.FLUID_TYPE_5) && itemStack.has(CastingDataComponents.FLUID_AMOUNT_5)) {
                String fluidAsString5 = itemStack.get(CastingDataComponents.FLUID_TYPE_5);
                int fluidAmount5 = itemStack.get(CastingDataComponents.FLUID_AMOUNT_5);
                FluidType fluid5 = BuiltInRegistries.FLUID.get(ResourceLocation.tryParse(fluidAsString5)).getFluidType();
                components.add(Component.literal("Tank 5 Contains: ").append(fluidAmount5 + "mb ").append(Component.translatable(fluid5.getDescriptionId())).withStyle(ChatFormatting.GREEN));
            }

            if (itemStack.has(CastingDataComponents.FLUID_TYPE_6) && itemStack.has(CastingDataComponents.FLUID_AMOUNT_6)) {
                String fluidAsString6 = itemStack.get(CastingDataComponents.FLUID_TYPE_6);
                int fluidAmount6 = itemStack.get(CastingDataComponents.FLUID_AMOUNT_6);
                FluidType fluid6 = BuiltInRegistries.FLUID.get(ResourceLocation.tryParse(fluidAsString6)).getFluidType();
                components.add(Component.literal("Tank 6 Contains: ").append(fluidAmount6 + "mb ").append(Component.translatable(fluid6.getDescriptionId())).withStyle(ChatFormatting.GREEN));
            }

            if (itemStack.has(CastingDataComponents.OUTPUT_FLUID_1) && itemStack.has(CastingDataComponents.OUTPUT_FLUID_AMOUNT_1)) {
                String outputFluidAsString = itemStack.get(CastingDataComponents.OUTPUT_FLUID_1);
                int outputFluidAmount = itemStack.get(CastingDataComponents.OUTPUT_FLUID_AMOUNT_1);
                FluidType outputFluid = BuiltInRegistries.FLUID.get(ResourceLocation.tryParse(outputFluidAsString)).getFluidType();
                components.add(Component.literal("Output Tank Contains: ").append(outputFluidAmount + "mb ").append(Component.translatable(outputFluid.getDescriptionId())).withStyle(ChatFormatting.GREEN));
            }
        }
        else if (itemStack.has(CastingDataComponents.FLUID_TYPE_1)) {
            components.add(Component.translatable("tooltips.bblcore.shift").withStyle(ChatFormatting.YELLOW));
        }

        super.appendHoverText(itemStack, context, components, flag);
    }


    @Nullable
    @Override
    public BlockEntity newBlockEntity(BlockPos pos, BlockState state) {
        return new MixerBlockEntity(pos, state);
    }

    @Nullable
    @Override
    public <T extends BlockEntity> BlockEntityTicker<T> getTicker(@NotNull Level level, @NotNull BlockState blockState, @NotNull BlockEntityType<T> blockEntityType) {
        return createTickerHelper(blockEntityType, CastingBlockEntities.MIXER_BLOCK_ENTITY.get(),
                (world, blockPos, thisBlockState, blockEntity) -> blockEntity.tick());
    }
}

