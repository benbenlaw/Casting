package com.benbenlaw.casting.data;

import com.benbenlaw.casting.Casting;
import com.benbenlaw.casting.block.CastingBlockStateProperties;
import com.benbenlaw.casting.block.CastingBlocks;
import net.minecraft.core.Direction;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.data.PackOutput;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.state.properties.BlockStateProperties;
import net.neoforged.neoforge.client.model.generators.BlockStateProvider;
import net.neoforged.neoforge.client.model.generators.ConfiguredModel;
import net.neoforged.neoforge.client.model.generators.ModelFile;
import net.neoforged.neoforge.common.data.ExistingFileHelper;
import net.neoforged.neoforge.registries.DeferredBlock;

import static com.benbenlaw.casting.fluid.CastingFluids.FLUIDS_MAP;

public class CastingBlockStatesProvider extends BlockStateProvider {

    public CastingBlockStatesProvider(PackOutput output, ExistingFileHelper existingFileHelper) {
        super(output, Casting.MOD_ID, existingFileHelper);
    }

    @Override
    protected void registerStatesAndModels() {

        //Blocks
        addMultiFaceBlockWithPoweredState(CastingBlocks.MULTIBLOCK_CONTROLLER.get());
        addMultiFaceBlockWithPoweredState(CastingBlocks.MULTIBLOCK_SOLIDIFIER.get());
        addMultiFaceBlockWithPoweredState(CastingBlocks.MULTIBLOCK_VALVE.get());
        addMultiFaceBlockWithPoweredState(CastingBlocks.MULTIBLOCK_MIXER.get());

        addMultiFaceBlockController(CastingBlocks.CONTROLLER.get());
        addMultiFaceBlock(CastingBlocks.SOLIDIFIER.get());
        addMultiFaceBlock(CastingBlocks.MIXER.get());
        addMultiFaceBlock(CastingBlocks.EQUIPMENT_MODIFIER.get());

        blockWithItem(CastingBlocks.BLACK_BRICKS);
        blockWithItem(CastingBlocks.MIXER_WHISK);

        simpleBlockWithItem(CastingBlocks.MULTIBLOCK_FUEL_TANK.get(), models().cubeAll(blockTexture(CastingBlocks.MULTIBLOCK_FUEL_TANK.get()).getPath(),
                blockTexture(CastingBlocks.MULTIBLOCK_FUEL_TANK.get())).renderType("cutout"));

        simpleBlockWithItem(CastingBlocks.MULTIBLOCK_COOLANT_TANK.get(), models().cubeAll(blockTexture(CastingBlocks.MULTIBLOCK_COOLANT_TANK.get()).getPath(),
                blockTexture(CastingBlocks.MULTIBLOCK_COOLANT_TANK.get())).renderType("cutout"));

        simpleBlockWithItem(CastingBlocks.TANK.get(), models().cubeAll(blockTexture(CastingBlocks.TANK.get()).getPath(),
                blockTexture(CastingBlocks.TANK.get())).renderType("cutout"));

        simpleBlockWithItem(CastingBlocks.BLACK_BRICK_GLASS.get(), models().cubeAll(blockTexture(CastingBlocks.BLACK_BRICK_GLASS.get()).getPath(),
                blockTexture(CastingBlocks.BLACK_BRICK_GLASS.get())).renderType("cutout"));

        //Fluids
        for (var entry : FLUIDS_MAP.entrySet()) {
            fluidBlocks(entry.getKey(), entry.getValue().getBlock());
        }
    }

    private void fluidBlocks(String name, Block block) {
        simpleBlock(block, models().getBuilder(name).texture("particle", ResourceLocation.fromNamespaceAndPath(Casting.MOD_ID, "block/molten_flow")));
    }

    private void blockWithItem(DeferredBlock<Block> blockRegistryObject) {
        simpleBlockWithItem(blockRegistryObject.get(), cubeAll(blockRegistryObject.get()));
    }


    /**
     * This method is used to add a block with a powered state and a multi-face model.
     * Textures names are blockname_side, blockname_front, blockname_top, blockname_front_lit
     * Creates the model, item model and block states for the block.
    */
    private void addMultiFaceBlockWithPoweredState(Block block) {
        ResourceLocation textureRegistryName = BuiltInRegistries.BLOCK.getKey(block);


        ModelFile modelFileOff = models().orientable(
                textureRegistryName.getPath() + "_off",
                ResourceLocation.parse("casting:block/" + textureRegistryName.getPath() + "_side"),
                ResourceLocation.parse("casting:block/" + textureRegistryName.getPath() + "_front"),
                ResourceLocation.parse("casting:block/" + textureRegistryName.getPath() + "_top"));
        ModelFile modelFileOn = models().orientable(
                textureRegistryName.getPath() + "_on",
                ResourceLocation.parse("casting:block/" + textureRegistryName.getPath() + "_side"),
                ResourceLocation.parse("casting:block/" + textureRegistryName.getPath() + "_front_lit"),
                ResourceLocation.parse("casting:block/" + textureRegistryName.getPath() + "_top"));

        simpleBlockItem(block, modelFileOff);

        getVariantBuilder(block)
                .forAllStates(state -> {
                    boolean working = state.getValue(CastingBlockStateProperties.WORKING);
                    Direction facing = state.getValue(BlockStateProperties.HORIZONTAL_FACING);
                    return ConfiguredModel.builder()
                            .modelFile(working ? modelFileOn : modelFileOff)
                            .rotationY((int) ((facing.toYRot() + 180) % 360))
                            .build();
                });

    }

    private void addMultiFaceBlock(Block block) {
        ResourceLocation textureRegistryName = BuiltInRegistries.BLOCK.getKey(block);


        ModelFile modelFileOff = models().orientable(
                textureRegistryName.getPath() + "_off",
                ResourceLocation.parse("casting:block/" + textureRegistryName.getPath() + "_side"),
                ResourceLocation.parse("casting:block/" + textureRegistryName.getPath() + "_front"),
                ResourceLocation.parse("casting:block/" + textureRegistryName.getPath() + "_top"));
        ModelFile modelFileOn = models().orientable(
                textureRegistryName.getPath() + "_on",
                ResourceLocation.parse("casting:block/" + textureRegistryName.getPath() + "_side"),
                ResourceLocation.parse("casting:block/" + textureRegistryName.getPath() + "_front_lit"),
                ResourceLocation.parse("casting:block/" + textureRegistryName.getPath() + "_top"));

        simpleBlockItem(block, modelFileOff);

        getVariantBuilder(block)
                .forAllStates(state -> {
                    Direction facing = state.getValue(BlockStateProperties.HORIZONTAL_FACING);
                    return ConfiguredModel.builder()
                            .modelFile(modelFileOff)
                            .rotationY((int) ((facing.toYRot() + 180) % 360))
                            .build();
                });

    }

    private void addMultiFaceBlockController(Block block) {
        ResourceLocation textureRegistryName = BuiltInRegistries.BLOCK.getKey(block);


        ModelFile modelFileOff = models().orientable(
                textureRegistryName.getPath() + "_off",
                ResourceLocation.parse("casting:block/" + textureRegistryName.getPath() + "_side"),
                ResourceLocation.parse("casting:block/" + textureRegistryName.getPath() + "_front"),
                ResourceLocation.parse("casting:block/" + textureRegistryName.getPath() + "_top"));
        ModelFile modelFileOn = models().orientable(
                textureRegistryName.getPath() + "_on",
                ResourceLocation.parse("casting:block/" + textureRegistryName.getPath() + "_side"),
                ResourceLocation.parse("casting:block/" + textureRegistryName.getPath() + "_front_lit"),
                ResourceLocation.parse("casting:block/" + textureRegistryName.getPath() + "_top"));

        simpleBlockItem(block, modelFileOff);

        getVariantBuilder(block)
                .forAllStates(state -> {
                    Direction facing = state.getValue(BlockStateProperties.HORIZONTAL_FACING);
                    return ConfiguredModel.builder()
                            .modelFile(state.getValue(BlockStateProperties.POWERED) ? modelFileOn : modelFileOff)
                            .rotationY((int) ((facing.toYRot() + 180) % 360))
                            .build();
                });

    }






    @Override
    public String getName() {
        return Casting.MOD_ID + " Block States";
    }
}
