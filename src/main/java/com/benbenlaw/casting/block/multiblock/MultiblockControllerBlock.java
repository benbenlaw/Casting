package com.benbenlaw.casting.block.multiblock;

import com.benbenlaw.casting.block.CastingBlockStateProperties;
import com.benbenlaw.casting.block.entity.CastingBlockEntities;
import com.benbenlaw.casting.block.entity.multiblock.MultiblockControllerBlockEntity;
import com.benbenlaw.casting.item.CastingDataComponents;
import com.benbenlaw.casting.screen.multiblock.MultiblockControllerMenu;
import com.mojang.serialization.MapCodec;
import net.minecraft.ChatFormatting;
import net.minecraft.client.gui.screens.Screen;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.core.particles.ParticleTypes;
import net.minecraft.network.chat.Component;
import net.minecraft.sounds.SoundEvents;
import net.minecraft.sounds.SoundSource;
import net.minecraft.util.RandomSource;
import net.minecraft.world.InteractionResult;
import net.minecraft.world.SimpleMenuProvider;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.inventory.ContainerData;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.TooltipFlag;
import net.minecraft.world.item.context.BlockPlaceContext;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.BaseEntityBlock;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.RenderShape;
import net.minecraft.world.level.block.Rotation;
import net.minecraft.world.level.block.entity.BlockEntity;
import net.minecraft.world.level.block.entity.BlockEntityTicker;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.block.state.StateDefinition;
import net.minecraft.world.level.block.state.properties.BlockStateProperties;
import net.minecraft.world.level.block.state.properties.BooleanProperty;
import net.minecraft.world.level.block.state.properties.DirectionProperty;
import net.minecraft.world.phys.BlockHitResult;
import net.neoforged.neoforge.fluids.FluidStack;
import net.neoforged.neoforge.fluids.FluidType;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;

public class MultiblockControllerBlock extends BaseEntityBlock {

    public static final MapCodec<MultiblockControllerBlock> CODEC = simpleCodec(MultiblockControllerBlock::new);

    @Override
    protected @NotNull MapCodec<? extends BaseEntityBlock> codec() {
        return CODEC;
    }

    public static final DirectionProperty FACING = BlockStateProperties.HORIZONTAL_FACING;
    public static final BooleanProperty WORKING = CastingBlockStateProperties.WORKING;
    public static final BooleanProperty ENABLED = CastingBlockStateProperties.ENABLED;

    /* FACING */
    @Override
    public BlockState getStateForPlacement(BlockPlaceContext pContext) {
        return this.defaultBlockState().setValue(FACING, pContext.getHorizontalDirection().getOpposite()).setValue(WORKING, false).setValue(ENABLED, false);
    }

    @Override
    public @NotNull BlockState rotate(BlockState blockState, Rotation pRotation) {
        return blockState.setValue(FACING, pRotation.rotate(blockState.getValue(FACING))).setValue(WORKING, blockState.getValue(WORKING)).setValue(ENABLED, blockState.getValue(ENABLED));
    }

    @Override
    protected void createBlockStateDefinition(StateDefinition.Builder<Block, BlockState> pBuilder) {
        pBuilder.add(FACING, WORKING, ENABLED);
    }

    public MultiblockControllerBlock(Properties properties) {
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
            if (blockEntity instanceof MultiblockControllerBlockEntity) {
                ((MultiblockControllerBlockEntity) blockEntity).drops();
            }
        }
        super.onRemove(pState, pLevel, pPos, pNewState, pIsMoving);
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

    @Override
    protected @NotNull InteractionResult useWithoutItem(@NotNull BlockState state, Level level, @NotNull BlockPos pos, @NotNull Player player, @NotNull BlockHitResult hit) {

        if (level.isClientSide()) {
            return InteractionResult.SUCCESS;
        }

        if (!level.isClientSide()) {

            MultiblockControllerBlockEntity controllerBlockEntity = (MultiblockControllerBlockEntity) level.getBlockEntity(pos);

            //Open Menu
            if (controllerBlockEntity instanceof MultiblockControllerBlockEntity) {
                ContainerData data = controllerBlockEntity.data;
                player.openMenu(new SimpleMenuProvider(
                        (windowId, playerInventory, playerEntity) -> new MultiblockControllerMenu(windowId, playerInventory, pos, data),
                        Component.translatable("block.casting.multiblock_controller")), (buf -> buf.writeBlockPos(pos)));

            }
            return InteractionResult.SUCCESS;


        }
        return InteractionResult.FAIL;
    }

    @Override
    public void animateTick(BlockState p_221253_, Level p_221254_, BlockPos p_221255_, RandomSource p_221256_) {
        if (p_221253_.getValue(WORKING)) {
            double d0 = (double)p_221255_.getX() + 0.5;
            double d1 = (double)p_221255_.getY();
            double d2 = (double)p_221255_.getZ() + 0.5;
            if (p_221256_.nextDouble() < 0.1) {
                p_221254_.playLocalSound(d0, d1, d2, SoundEvents.FURNACE_FIRE_CRACKLE, SoundSource.BLOCKS, 1.0F, 1.0F, false);
            }

            Direction direction = p_221253_.getValue(FACING);
            Direction.Axis direction$axis = direction.getAxis();
            double d3 = 0.52;
            double d4 = p_221256_.nextDouble() * 0.6 - 0.3;
            double d5 = direction$axis == Direction.Axis.X ? (double)direction.getStepX() * 0.52 : d4;
            double d6 = p_221256_.nextDouble() * 6.0 / 16.0;
            double d7 = direction$axis == Direction.Axis.Z ? (double)direction.getStepZ() * 0.52 : d4;
            p_221254_.addParticle(ParticleTypes.SMOKE, d0 + d5, d1 + d6, d2 + d7, 0.0, 0.0, 0.0);
            p_221254_.addParticle(ParticleTypes.FLAME, d0 + d5, d1 + d6, d2 + d7, 0.0, 0.0, 0.0);
        }
    }

    @Nullable
    @Override
    public BlockEntity newBlockEntity(@NotNull BlockPos pos, @NotNull BlockState state) {
        return new MultiblockControllerBlockEntity(pos, state);
    }

    @Nullable
    @Override
    public <T extends BlockEntity> BlockEntityTicker<T> getTicker(@NotNull Level level, @NotNull BlockState blockState, @NotNull BlockEntityType<T> blockEntityType) {
        return createTickerHelper(blockEntityType, CastingBlockEntities.MULTIBLOCK_CONTROLLER_BLOCK_ENTITY.get(),
                (world, blockPos, thisBlockState, blockEntity) -> blockEntity.tick());
    }
}
