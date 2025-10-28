package com.benbenlaw.casting.block.custom;

import com.benbenlaw.casting.block.entity.CastingBlockEntities;
import com.benbenlaw.casting.block.entity.SolidifierBlockEntity;
import com.benbenlaw.casting.item.CastingDataComponents;
import com.benbenlaw.casting.screen.SolidifierMenu;
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

public class SolidifierBlock extends BaseEntityBlock {

    public static final MapCodec<SolidifierBlock> CODEC = simpleCodec(SolidifierBlock::new);

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

    public SolidifierBlock(Properties properties) {
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
            if (blockEntity instanceof SolidifierBlockEntity) {
                ((SolidifierBlockEntity) blockEntity).drops();
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

            SolidifierBlockEntity solidifierBlockEntity = (SolidifierBlockEntity) level.getBlockEntity(blockPos);


            if (solidifierBlockEntity instanceof SolidifierBlockEntity) {

                //Use Bucket First else open menu not both

                if (solidifierBlockEntity.onPlayerUse(player, InteractionHand.MAIN_HAND)) {
                    return InteractionResult.SUCCESS;
                }

                else {
                    ContainerData data = solidifierBlockEntity.data;
                    player.openMenu(new SimpleMenuProvider(
                            (windowId, playerInventory, playerEntity) -> new SolidifierMenu(windowId, playerInventory, blockPos, data),
                            Component.translatable("block.casting.solidifier")), (buf -> buf.writeBlockPos(blockPos)));
                    solidifierBlockEntity.sync();
                }
            }


            return InteractionResult.SUCCESS;
        }
        return InteractionResult.FAIL;
    }

    @Override
    public void appendHoverText(ItemStack itemStack, Item.@NotNull TooltipContext context, @NotNull List<Component> components, @NotNull TooltipFlag flag) {

        if (Screen.hasShiftDown()) {

            if (itemStack.has(CastingDataComponents.FLUIDS)) {
                components.add(Component.literal("Fluids:").withStyle(ChatFormatting.BLUE));

                List<FluidStack> fluidStacks = itemStack.get(CastingDataComponents.FLUIDS);

                assert fluidStacks != null;
                for (FluidStack fluidStack : fluidStacks) {
                    FluidType fluid = fluidStack.getFluid().getFluidType();
                    int amount = fluidStack.getAmount();
                    components.add(Component.literal("- ").append(amount + "mb ").append(Component.translatable(fluid.getDescriptionId())).withStyle(ChatFormatting.GREEN));
                }
            }
        }

        else {
            components.add(Component.translatable("tooltips.bblcore.shift").withStyle(ChatFormatting.YELLOW));
        }
        super.appendHoverText(itemStack, context, components, flag);

    }

    @Nullable
    @Override
    public BlockEntity newBlockEntity(BlockPos pos, BlockState state) {
        return new SolidifierBlockEntity(pos, state);
    }

    @Nullable
    @Override
    public <T extends BlockEntity> BlockEntityTicker<T> getTicker(@NotNull Level level, @NotNull BlockState blockState, @NotNull BlockEntityType<T> blockEntityType) {
        return createTickerHelper(blockEntityType, CastingBlockEntities.SOLIDIFIER_BLOCK_ENTITY.get(),
                (world, blockPos, thisBlockState, blockEntity) -> blockEntity.tick());
    }
}

