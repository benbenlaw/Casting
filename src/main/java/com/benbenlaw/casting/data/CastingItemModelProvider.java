package com.benbenlaw.casting.data;

import com.benbenlaw.casting.Casting;
import com.benbenlaw.casting.item.CastingItems;
import com.benbenlaw.casting.item.EquipmentModifier;
import net.minecraft.data.PackOutput;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.item.Item;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.material.Fluid;
import net.neoforged.neoforge.client.model.generators.ItemModelBuilder;
import net.neoforged.neoforge.client.model.generators.ItemModelProvider;
import net.neoforged.neoforge.client.model.generators.loaders.DynamicFluidContainerModelBuilder;
import net.neoforged.neoforge.common.data.ExistingFileHelper;
import net.neoforged.neoforge.internal.versions.neoforge.NeoForgeVersion;
import net.neoforged.neoforge.registries.DeferredBlock;
import net.neoforged.neoforge.registries.DeferredHolder;
import net.neoforged.neoforge.registries.DeferredItem;

import static com.benbenlaw.casting.fluid.CastingFluids.FLUIDS_MAP;

public class CastingItemModelProvider extends ItemModelProvider {

    public CastingItemModelProvider(PackOutput output, ExistingFileHelper existingFileHelper) {
        super(output, Casting.MOD_ID, existingFileHelper);
    }

    @Override
    protected void registerModels() {

        //Misc
        simpleItem(CastingItems.BLACK_BRICK);
        simpleItem(CastingItems.FLUID_MOVER);

        //Molds
        simpleItem(CastingItems.BLOCK_MOLD);
        simpleItem(CastingItems.GEAR_MOLD);
        simpleItem(CastingItems.INGOT_MOLD);
        simpleItem(CastingItems.NUGGET_MOLD);
        simpleItem(CastingItems.PLATE_MOLD);
        simpleItem(CastingItems.ROD_MOLD);
        simpleItem(CastingItems.GEM_MOLD);
        simpleItem(CastingItems.DUST_MOLD);
        simpleItem(CastingItems.BALL_MOLD);
        simpleItem(CastingItems.WIRE_MOLD);
        simpleItem(CastingItems.REPAIRING_MOLD);

        //Equipment Modifier Items
        for (EquipmentModifier modifier : EquipmentModifier.values()) {
            simpleItem(modifier.item);
        }

        //Bucket Models
        for (var entry : FLUIDS_MAP.entrySet()) {
            String fluidName = entry.getKey();
            var fluid = entry.getValue();

            simpleBucketItem(fluidName + "_bucket", fluid.getFluid());
        }
    }

    private void simpleBucketItem(String name, Fluid fluid) {
        withExistingParent(name, ResourceLocation.fromNamespaceAndPath(NeoForgeVersion.MOD_ID, "item/bucket"))
                .customLoader(DynamicFluidContainerModelBuilder::begin)
                .fluid(fluid);
    }


    private void simpleItem(DeferredItem<Item> item) {
        withExistingParent(item.getId().getPath(),
                ResourceLocation.withDefaultNamespace("item/generated")).texture("layer0",
                ResourceLocation.fromNamespaceAndPath(Casting.MOD_ID, "item/" + item.getId().getPath()));
    }

    public void simpleItem(DeferredHolder<Item, ?> item) {
        withExistingParent(item.getId().getPath(), mcLoc("item/generated"))
                .texture("layer0", modLoc("item/" + item.getId().getPath()));
    }

    private ItemModelBuilder simpleBlockItem(DeferredBlock<Block> item) {
        return withExistingParent(item.getId().getPath(),
                ResourceLocation.withDefaultNamespace("item/generated")).texture("layer0",
                ResourceLocation.fromNamespaceAndPath(Casting.MOD_ID,"item/" + item.getId().getPath()));
    }


    @Override
    public String getName() {
        return Casting.MOD_ID + " Item Models";
    }
}
