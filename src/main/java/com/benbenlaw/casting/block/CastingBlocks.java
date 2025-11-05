package com.benbenlaw.casting.block;

import com.benbenlaw.casting.Casting;
import com.benbenlaw.casting.block.custom.*;
import com.benbenlaw.casting.block.multiblock.*;
import com.benbenlaw.casting.item.CastingItems;
import net.minecraft.core.registries.Registries;
import net.minecraft.resources.ResourceKey;
import net.minecraft.world.item.BlockItem;
import net.minecraft.world.item.Item;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.Blocks;
import net.minecraft.world.level.block.SoundType;
import net.minecraft.world.level.block.TransparentBlock;
import net.minecraft.world.level.block.state.BlockBehaviour;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.block.state.properties.BlockStateProperties;
import net.minecraft.world.level.block.state.properties.BooleanProperty;
import net.neoforged.neoforge.registries.DeferredBlock;
import net.neoforged.neoforge.registries.DeferredRegister;

import java.util.function.Supplier;
import java.util.function.ToIntFunction;

public class CastingBlocks {

    public static final DeferredRegister.Blocks BLOCKS = DeferredRegister.createBlocks(Casting.MOD_ID);

    //Misc
    public static final DeferredBlock<Block> BLACK_BRICKS = registerBlock("black_bricks",
            () -> new Block(BlockBehaviour.Properties.ofFullCopy(Blocks.BRICKS).sound(SoundType.STONE)));

    public static final DeferredBlock<Block> BLACK_BRICK_GLASS = registerBlock("black_brick_glass",
            () -> new TransparentBlock(BlockBehaviour.Properties.ofFullCopy(Blocks.GLASS).sound(SoundType.GLASS).noOcclusion()));


    //Multiblock
    public static final DeferredBlock<Block> MULTIBLOCK_CONTROLLER = registerBlock("multiblock_controller",
            () -> new MultiblockControllerBlock(BlockBehaviour.Properties.ofFullCopy(Blocks.BRICKS).sound(SoundType.STONE)
                    .lightLevel(litBlockEmission(15)).noOcclusion().setId(createID("multiblock_controller"))));

    public static final DeferredBlock<Block> MULTIBLOCK_SOLIDIFIER = registerBlock("multiblock_solidifier",
            () -> new MultiblockSolidifierBlock(BlockBehaviour.Properties.ofFullCopy(Blocks.BRICKS).sound(SoundType.STONE)
                    .noOcclusion().setId(createID("multiblock_solidifier"))));

    public static final DeferredBlock<Block> MULTIBLOCK_FUEL_TANK = registerBlock("multiblock_fuel_tank",
            () -> new MultiblockFuelTankBlock(BlockBehaviour.Properties.ofFullCopy(Blocks.BRICKS).sound(SoundType.STONE)
                    .lightLevel(state -> state.getValue(MultiblockFuelTankBlock.LIGHT_LEVEL)).noOcclusion().setId(createID("multiblock_fuel_tank"))));

    public static final DeferredBlock<Block> MULTIBLOCK_COOLANT_TANK = registerBlock("multiblock_coolant_tank",
            () -> new MultiblockCoolantTankBlock(BlockBehaviour.Properties.ofFullCopy(Blocks.BRICKS).sound(SoundType.STONE)
                    .lightLevel(state -> state.getValue(MultiblockCoolantTankBlock.LIGHT_LEVEL)).noOcclusion().setId(createID("multiblock_coolant_tank"))));

    public static final DeferredBlock<Block> MULTIBLOCK_MIXER = registerBlock("multiblock_mixer",
            () -> new MultiblockMixerBlock(BlockBehaviour.Properties.ofFullCopy(Blocks.BRICKS).sound(SoundType.STONE)
                    .noOcclusion().setId(createID("multiblock_mixer"))));

    public static final DeferredBlock<Block> MULTIBLOCK_REGULATOR = registerBlock("multiblock_regulator",
            () -> new MultiblockRegulatorBlock(BlockBehaviour.Properties.ofFullCopy(Blocks.BRICKS).sound(SoundType.STONE)
                    .noOcclusion().setId(createID("multiblock_regulator"))));

    public static final DeferredBlock<Block> MULTIBLOCK_VALVE = registerBlock("multiblock_valve",
            () -> new MultiblockValveBlock(BlockBehaviour.Properties.ofFullCopy(Blocks.BRICKS).sound(SoundType.STONE).noOcclusion()));

    //Simple

    public static final DeferredBlock<Block> CONTROLLER = registerBlock("controller",
            () -> new ControllerBlock(BlockBehaviour.Properties.ofFullCopy(Blocks.STONE).sound(SoundType.STONE)
                    .lightLevel(litBlockEmission(15)).noOcclusion().setId(createID("controller"))));

    public static final DeferredBlock<Block> SOLIDIFIER = registerBlock("solidifier",
            () -> new SolidifierBlock(BlockBehaviour.Properties.ofFullCopy(Blocks.STONE).sound(SoundType.STONE)
                    .noOcclusion().setId(createID("solidifier"))));

    public static final DeferredBlock<Block> TANK = registerBlock("tank",
            () -> new TankBlock(BlockBehaviour.Properties.ofFullCopy(Blocks.STONE).sound(SoundType.STONE)
                    .noOcclusion().setId(createID("tank"))));

    public static final DeferredBlock<Block> MIXER = registerBlock("mixer",
            () -> new MixerBlock(BlockBehaviour.Properties.ofFullCopy(Blocks.STONE).sound(SoundType.STONE)
                    .noOcclusion().setId(createID("mixer"))));

    public static final DeferredBlock<Block> EQUIPMENT_MODIFIER = registerBlock("equipment_modifier",
            () -> new EquipmentModifierBlock(BlockBehaviour.Properties.ofFullCopy(Blocks.STONE).sound(SoundType.STONE)
                    .noOcclusion().setId(createID("equipment_modifier"))));




    private static <T extends Block> DeferredBlock<T> registerBlock(String name, Supplier<T> block) {
        DeferredBlock<T> toReturn = BLOCKS.register(name, block);
        registerBlockItem(name, toReturn);
        return toReturn;
    }

    private static <T extends Block> void registerBlockItem(String name, DeferredBlock<T> block) {
        CastingItems.ITEMS.registerItem(name, (properties) -> new BlockItem(block.get(), properties.useBlockDescriptionPrefix()));
    }

    private static ToIntFunction<BlockState> litBlockEmission(int lightLevel) {
        return (blockState) -> blockState.getValue(BooleanProperty.create("running")) ? lightLevel : 0;
    }

    public static ResourceKey<Block> createID(String name) {
        return ResourceKey.create(Registries.BLOCK, Casting.rl(name));
    }
}
