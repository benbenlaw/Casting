package com.benbenlaw.casting.data;

import com.benbenlaw.casting.Casting;
import com.benbenlaw.casting.block.CastingBlocks;
import com.benbenlaw.casting.item.CastingItems;
import com.benbenlaw.casting.item.EquipmentModifier;
import net.minecraft.client.data.models.BlockModelGenerators;
import net.minecraft.client.data.models.ItemModelGenerators;
import net.minecraft.client.data.models.ModelProvider;
import net.minecraft.client.data.models.model.ModelTemplates;
import net.minecraft.client.data.models.model.TexturedModel;
import net.minecraft.core.Holder;
import net.minecraft.data.PackOutput;
import net.minecraft.world.item.Item;
import net.minecraft.world.level.block.Block;
import net.neoforged.neoforge.client.model.generators.ItemModelBuilder;
import net.neoforged.neoforge.client.model.generators.ItemModelProvider;
import net.neoforged.neoforge.client.model.generators.loaders.DynamicFluidContainerModelBuilder;
import net.neoforged.neoforge.common.data.ExistingFileHelper;
import org.jetbrains.annotations.NotNull;

import java.util.stream.Stream;

import static com.benbenlaw.casting.fluid.CastingFluids.FLUIDS_MAP;

public class CastingModelProvider extends ModelProvider {

    public CastingModelProvider(PackOutput output) {
        super(output, Casting.MOD_ID);
    }


    @Override
    protected void registerModels(BlockModelGenerators blockModels, ItemModelGenerators itemModels) {

        //Items
        itemModels.generateFlatItem(CastingItems.BLACK_BRICK.get(), ModelTemplates.FLAT_ITEM);
        itemModels.generateFlatItem(CastingItems.FLUID_MOVER.get(), ModelTemplates.FLAT_HANDHELD_ITEM);

        itemModels.generateFlatItem(CastingItems.BLOCK_MOLD.get(), ModelTemplates.FLAT_ITEM);
        itemModels.generateFlatItem(CastingItems.GEAR_MOLD.get(), ModelTemplates.FLAT_ITEM);
        itemModels.generateFlatItem(CastingItems.INGOT_MOLD.get(), ModelTemplates.FLAT_ITEM);
        itemModels.generateFlatItem(CastingItems.NUGGET_MOLD.get(), ModelTemplates.FLAT_ITEM);
        itemModels.generateFlatItem(CastingItems.PLATE_MOLD.get(), ModelTemplates.FLAT_ITEM);
        itemModels.generateFlatItem(CastingItems.ROD_MOLD.get(), ModelTemplates.FLAT_ITEM);
        itemModels.generateFlatItem(CastingItems.GEM_MOLD.get(), ModelTemplates.FLAT_ITEM);
        itemModels.generateFlatItem(CastingItems.DUST_MOLD.get(), ModelTemplates.FLAT_ITEM);
        itemModels.generateFlatItem(CastingItems.BALL_MOLD.get(), ModelTemplates.FLAT_ITEM);
        itemModels.generateFlatItem(CastingItems.WIRE_MOLD.get(), ModelTemplates.FLAT_ITEM);
        itemModels.generateFlatItem(CastingItems.REPAIRING_MOLD.get(), ModelTemplates.FLAT_ITEM);

        for (EquipmentModifier modifier : EquipmentModifier.values()) {
            itemModels.generateFlatItem(modifier.item.get(), ModelTemplates.FLAT_ITEM);
        }

        for (var entry : FLUIDS_MAP.entrySet()) {
            String fluidName = entry.getKey();
            var fluid = entry.getValue();

            //itemModels.generateFlatItem(fluidName + "_bucket");

        }

        //Blocks
        blockModels.createTrivialCube(CastingBlocks.BLACK_BRICKS.get());
        blockModels.createTrivialCube(CastingBlocks.BLACK_BRICK_GLASS.get());

        blockModels.createFurnace(CastingBlocks.MULTIBLOCK_CONTROLLER.get(), TexturedModel.ORIENTABLE_ONLY_TOP);
        blockModels.createFurnace(CastingBlocks.MULTIBLOCK_SOLIDIFIER.get(), TexturedModel.ORIENTABLE_ONLY_TOP);
        blockModels.createTrivialCube(CastingBlocks.MULTIBLOCK_FUEL_TANK.get());
        blockModels.createTrivialCube(CastingBlocks.MULTIBLOCK_COOLANT_TANK.get());
        blockModels.createFurnace(CastingBlocks.MULTIBLOCK_MIXER.get(), TexturedModel.ORIENTABLE_ONLY_TOP);
        blockModels.createFurnace(CastingBlocks.MULTIBLOCK_REGULATOR.get(), TexturedModel.ORIENTABLE_ONLY_TOP);
        blockModels.createFurnace(CastingBlocks.MULTIBLOCK_VALVE.get(), TexturedModel.ORIENTABLE_ONLY_TOP);

        blockModels.createFurnace(CastingBlocks.CONTROLLER.get(), TexturedModel.ORIENTABLE_ONLY_TOP);
        blockModels.createFurnace(CastingBlocks.SOLIDIFIER.get(), TexturedModel.ORIENTABLE_ONLY_TOP);
        blockModels.createTrivialCube(CastingBlocks.TANK.get());
        blockModels.createFurnace(CastingBlocks.MIXER.get(), TexturedModel.ORIENTABLE_ONLY_TOP);
        blockModels.createFurnace(CastingBlocks.EQUIPMENT_MODIFIER.get(), TexturedModel.ORIENTABLE_ONLY_TOP);

    }

    @Override
    protected @NotNull Stream<? extends Holder<Block>> getKnownBlocks() {
        return CastingBlocks.BLOCKS.getEntries().stream();
    }

    @Override
    protected @NotNull Stream<? extends Holder<Item>> getKnownItems() {
        return CastingItems.ITEMS.getEntries().stream();
    }

    @Override
    public String getName() {
        return Casting.MOD_ID + " Models";
    }
}
