package com.benbenlaw.casting.data;

import com.benbenlaw.casting.Casting;
import com.benbenlaw.casting.block.CastingBlocks;
import com.benbenlaw.casting.item.CastingItems;
import com.benbenlaw.casting.item.EquipmentModifier;
import com.benbenlaw.casting.util.CastingTags;
import com.benbenlaw.core.tag.ModdedTagBuilder;
import com.benbenlaw.core.tag.ResourceMaterial;
import com.benbenlaw.core.tag.ResourceNames;
import net.minecraft.core.HolderLookup;
import net.minecraft.data.PackOutput;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.tags.FluidTags;
import net.minecraft.tags.ItemTags;
import net.minecraft.tags.TagKey;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.Items;
import net.minecraft.world.level.material.Fluid;
import net.neoforged.neoforge.common.Tags;
import net.neoforged.neoforge.common.data.BlockTagsProvider;
import net.neoforged.neoforge.common.data.ExistingFileHelper;
import net.neoforged.neoforge.common.data.ItemTagsProvider;
import net.neoforged.neoforge.registries.DeferredHolder;
import org.jetbrains.annotations.NotNull;

import java.util.Locale;
import java.util.Objects;
import java.util.concurrent.CompletableFuture;

import static com.benbenlaw.casting.fluid.CastingFluids.FLUIDS_MAP;

public class CastingItemTags extends ItemTagsProvider {

    CastingItemTags(PackOutput output, CompletableFuture<HolderLookup.Provider> lookupProvider) {
        super(output, lookupProvider, Casting.MOD_ID);
    }

    @Override
    protected void addTags(HolderLookup.@NotNull Provider provider) {

        //All Equipment Modifiers
        for (DeferredHolder<Item, ? extends Item> item : EquipmentModifier.ITEMS.getEntries()) {
            tag(CastingTags.Items.EQUIPMENT_MODIFIERS).add(item.get());
        }

        //Can be disabled with shift modifiers
        tag(CastingTags.Items.CAN_BE_DISABLED_WITH_SHIFT)
                .add(EquipmentModifier.WATER_WALKER.item.get())
                .add(EquipmentModifier.LAVA_WALKER.item.get());

        //Can be toggled with shift modifiers
        tag(CastingTags.Items.CAN_BE_TOGGLED_WITH_SHIFT)
                .add(EquipmentModifier.AUTO_SMELT.item.get())
                .add(EquipmentModifier.EXCAVATION.item.get())
                .add(EquipmentModifier.SILK_TOUCH.item.get());

        tag(CastingTags.Items.CAN_BE_TOGGLED_WITH_KEYBIND)
                .add(EquipmentModifier.NIGHT_VISION.item.get())
                .add(EquipmentModifier.FLIGHT.item.get())
                .add(EquipmentModifier.STEP_ASSIST.item.get())
                .add(EquipmentModifier.WATER_WALKER.item.get())
                .add(EquipmentModifier.LAVA_WALKER.item.get())
                .add(EquipmentModifier.SPEED.item.get())
                .add(EquipmentModifier.MAGNET.item.get())
                .add(EquipmentModifier.JETS.item.get());



        //Ball Items
        tag(CastingTags.Items.BALL_ITEMS)
                .add(Items.SNOWBALL)
                .add(Items.SLIME_BALL)
                .add(Items.MAGMA_CREAM)
                .add(Items.FIRE_CHARGE)
                .add(Items.ENDER_PEARL)
                .add(Items.ENDER_EYE)
                .add(Items.CLAY_BALL);

        //Bricks
        tag(Tags.Items.BRICKS).add(CastingItems.BLACK_BRICK.asItem());

        //Controller Floors
        tag(CastingTags.Items.CONTROLLER_FLOORS)
                .add(CastingBlocks.BLACK_BRICKS.get().asItem())
        ;

        //Controller Walls
        tag(CastingTags.Items.CONTROLLER_WALLS)
                .addTag(CastingTags.Items.CONTROLLER_EXTRA_BLOCKS)
                .add(CastingBlocks.BLACK_BRICK_GLASS.get().asItem())
                .add(CastingBlocks.BLACK_BRICKS.get().asItem())
                .add(CastingBlocks.MULTIBLOCK_CONTROLLER.get().asItem())
                .add(CastingBlocks.MULTIBLOCK_REGULATOR.get().asItem())
        ;

        //Controller Tanks
        tag(CastingTags.Items.CONTROLLER_TANKS)
                .add(CastingBlocks.MULTIBLOCK_FUEL_TANK.get().asItem());

        //Controller Extra Blocks, includes any non wall and floor block to add to the multiblock data
        tag(CastingTags.Items.CONTROLLER_EXTRA_BLOCKS)
                .addTag(CastingTags.Items.CONTROLLER_TANKS)
                .add(CastingBlocks.MULTIBLOCK_SOLIDIFIER.get().asItem())
                .add(CastingBlocks.MULTIBLOCK_VALVE.get().asItem())
                .add(CastingBlocks.MULTIBLOCK_MIXER.get().asItem())
        ;

        //Molds
        tag(CastingTags.Items.MOLDS)
                .add(CastingItems.BLOCK_MOLD.asItem())
                .add(CastingItems.DUST_MOLD.asItem())
                .add(CastingItems.GEAR_MOLD.asItem())
                .add(CastingItems.INGOT_MOLD.asItem())
                .add(CastingItems.NUGGET_MOLD.asItem())
                .add(CastingItems.PLATE_MOLD.asItem())
                .add(CastingItems.GEM_MOLD.asItem())
                .add(CastingItems.ROD_MOLD.asItem())
                .add(CastingItems.BALL_MOLD.asItem())
                .add(CastingItems.WIRE_MOLD.asItem())
        ;

        tag(CastingTags.Items.INGOT_MOLD).add(CastingItems.INGOT_MOLD.asItem());
        tag(CastingTags.Items.NUGGET_MOLD).add(CastingItems.NUGGET_MOLD.asItem());
        tag(CastingTags.Items.GEM_MOLD).add(CastingItems.GEM_MOLD.asItem());
        tag(CastingTags.Items.DUST_MOLD).add(CastingItems.DUST_MOLD.asItem());
        tag(CastingTags.Items.PLATE_MOLD).add(CastingItems.PLATE_MOLD.asItem());
        tag(CastingTags.Items.GEAR_MOLD).add(CastingItems.GEAR_MOLD.asItem());
        tag(CastingTags.Items.ROD_MOLD).add(CastingItems.ROD_MOLD.asItem());
        tag(CastingTags.Items.BLOCK_MOLD).add(CastingItems.BLOCK_MOLD.asItem());
        tag(CastingTags.Items.BALL_MOLD).add(CastingItems.BALL_MOLD.asItem());
        tag(CastingTags.Items.WIRE_MOLD).add(CastingItems.WIRE_MOLD.asItem());


        //Buckets
        for (var entry : FLUIDS_MAP.entrySet()) {
            tag(Tags.Items.BUCKETS).add(entry.getValue().getBucket());
        }

        //Melting Output Amount Effected
        tag(CastingTags.Items.MELTING_OUTPUT_AMOUNT_EFFECTED)
                .addTag(createNeoFabricItemTag("ores"))
                .addTag(createNeoFabricItemTag("raw_materials"))
                .add(Items.ANCIENT_DEBRIS)
        ;

        //Melting Produces Experience
        tag(CastingTags.Items.MELTING_PRODUCES_EXPERIENCE)
                .addTag(createNeoFabricItemTag("ores"))
                .addTag(createNeoFabricItemTag("raw_materials"))
                .add(Items.ANCIENT_DEBRIS)
        ;

        for (ResourceMaterial resource : ResourceMaterial.values()) {
            String rawStorageBlock = "storage_blocks/raw_" + resource.toString().toLowerCase(Locale.ROOT);
            tag(CastingTags.Items.MELTING_OUTPUT_AMOUNT_EFFECTED).addOptionalTag(createNeoFabricItemTag(rawStorageBlock));
        }
    }

    public static TagKey<Item> createNeoFabricItemTag(String path) {
        return ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
                String.valueOf(ResourceLocation.fromNamespaceAndPath("c", path)))));
    }
}
