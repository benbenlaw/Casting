package com.benbenlaw.casting.data;

import com.benbenlaw.casting.Casting;
import com.benbenlaw.casting.fluid.CastingFluids;
import com.benbenlaw.casting.item.EquipmentModifierItem;
import com.benbenlaw.casting.item.EquipmentModifierItems;
import com.benbenlaw.casting.item.ModItems;
import com.benbenlaw.casting.util.CastingTags;
import net.minecraft.core.HolderLookup;
import net.minecraft.data.PackOutput;
import net.minecraft.data.tags.ItemTagsProvider;
import net.neoforged.neoforge.common.Tags;
import net.neoforged.neoforge.common.data.BlockTagsProvider;
import net.neoforged.neoforge.common.data.ExistingFileHelper;
import org.jetbrains.annotations.NotNull;

import java.util.concurrent.CompletableFuture;

public class CastingItemTags extends ItemTagsProvider {

    CastingItemTags(PackOutput output, CompletableFuture<HolderLookup.Provider> lookupProvider, BlockTagsProvider blockTags, ExistingFileHelper existingFileHelper) {
        super(output, lookupProvider, blockTags.contentsGetter(), Casting.MOD_ID, existingFileHelper);
    }

    @Override
    protected void addTags(HolderLookup.@NotNull Provider provider) {

        tag(Tags.Items.BRICKS).add(ModItems.BLACK_BRICK.asItem());

        tag(CastingTags.Items.CAN_BE_DISABLED_WITH_SHIFT)
                .add(EquipmentModifierItems.AUTO_SMELT.asItem())
                .add(EquipmentModifierItems.EXCAVATION.asItem())
                .add(EquipmentModifierItems.STEP_ASSIST.asItem())
                .add(EquipmentModifierItems.SILK_TOUCH.asItem())
                .add(EquipmentModifierItems.WATER_WALKER.asItem())
                .add(EquipmentModifierItems.LAVA_WALKER.asItem());




        tag(CastingTags.Items.MOLDS)
                .add(ModItems.BLOCK_MOLD.asItem())
                .add(ModItems.BLOCK_MOLD.asItem())
                .add(ModItems.DUST_MOLD.asItem())
                .add(ModItems.GEAR_MOLD.asItem())
                .add(ModItems.INGOT_MOLD.asItem())
                .add(ModItems.NUGGET_MOLD.asItem())
                .add(ModItems.PLATE_MOLD.asItem())
                .add(ModItems.GEM_MOLD.asItem())
                .add(ModItems.ROD_MOLD.asItem())
                .add(ModItems.BALL_MOLD.asItem())
        ;

        tag(CastingTags.Items.INGOT_MOLD).add(ModItems.INGOT_MOLD.asItem());
        tag(CastingTags.Items.NUGGET_MOLD).add(ModItems.NUGGET_MOLD.asItem());
        tag(CastingTags.Items.GEM_MOLD).add(ModItems.GEM_MOLD.asItem());
        tag(CastingTags.Items.DUST_MOLD).add(ModItems.DUST_MOLD.asItem());
        tag(CastingTags.Items.PLATE_MOLD).add(ModItems.PLATE_MOLD.asItem());
        tag(CastingTags.Items.GEAR_MOLD).add(ModItems.GEAR_MOLD.asItem());
        tag(CastingTags.Items.ROD_MOLD).add(ModItems.ROD_MOLD.asItem());
        tag(CastingTags.Items.BLOCK_MOLD).add(ModItems.BLOCK_MOLD.asItem());
        tag(CastingTags.Items.BALL_MOLD).add(ModItems.BALL_MOLD.asItem());

        tag(Tags.Items.BUCKETS)
                .add(CastingFluids.MOLTEN_BRONZE.getBucket())
                .add(CastingFluids.MOLTEN_OBSIDIAN.getBucket())
                .add(CastingFluids.MOLTEN_STEEL.getBucket())
                .add(CastingFluids.MOLTEN_NETHERITE.getBucket())
                .add(CastingFluids.MOLTEN_ELECTRUM.getBucket())
                .add(CastingFluids.MOLTEN_INVAR.getBucket())
                .add(CastingFluids.MOLTEN_IRON.getBucket())
                .add(CastingFluids.MOLTEN_GOLD.getBucket())
                .add(CastingFluids.MOLTEN_COPPER.getBucket())
                .add(CastingFluids.MOLTEN_TIN.getBucket())
                .add(CastingFluids.MOLTEN_LEAD.getBucket())
                .add(CastingFluids.MOLTEN_SILVER.getBucket())
                .add(CastingFluids.MOLTEN_NICKEL.getBucket())
                .add(CastingFluids.MOLTEN_OSMIUM.getBucket())
                .add(CastingFluids.MOLTEN_QUARTZ.getBucket())
                .add(CastingFluids.MOLTEN_LAPIS.getBucket())
                .add(CastingFluids.MOLTEN_REDSTONE.getBucket())
                .add(CastingFluids.MOLTEN_DIAMOND.getBucket())
                .add(CastingFluids.MOLTEN_EMERALD.getBucket())
                .add(CastingFluids.MOLTEN_URANIUM.getBucket())
                .add(CastingFluids.MOLTEN_GLASS.getBucket())
                .add(CastingFluids.MOLTEN_DEBRIS.getBucket())
                .add(CastingFluids.MOLTEN_STONE.getBucket())
                .add(CastingFluids.MOLTEN_COAL.getBucket())
                .add(CastingFluids.MOLTEN_ALUMINUM.getBucket())
                .add(CastingFluids.MOLTEN_ZINC.getBucket())
                .add(CastingFluids.MOLTEN_PLATINUM.getBucket())
                .add(CastingFluids.MOLTEN_IRIDIUM.getBucket())
                .add(CastingFluids.MOLTEN_GLOWSTONE.getBucket())
                .add(CastingFluids.MOLTEN_ENDER.getBucket())
                .add(CastingFluids.MOLTEN_CONSTANTAN.getBucket())
                .add(CastingFluids.MOLTEN_BRASS.getBucket())
                .add(CastingFluids.MOLTEN_SIGNALUM.getBucket())
                .add(CastingFluids.MOLTEN_LUMIUM.getBucket())
                .add(CastingFluids.MOLTEN_ENDERIUM.getBucket())
                .add(CastingFluids.MOLTEN_ENERGETIC_ALLOY.getBucket())
                .add(CastingFluids.MOLTEN_VIBRANT_ALLOY.getBucket())
                .add(CastingFluids.MOLTEN_PULSATING_ALLOY.getBucket())
                .add(CastingFluids.MOLTEN_COPPER_ALLOY.getBucket())
                .add(CastingFluids.MOLTEN_DARK_STEEL.getBucket())
                .add(CastingFluids.MOLTEN_SOULARIUM.getBucket())
                .add(CastingFluids.MOLTEN_END_STEEL.getBucket())
                .add(CastingFluids.MOLTEN_CONDUCTIVE_ALLOY.getBucket())
                .add(CastingFluids.MOLTEN_SILICON.getBucket())
                .add(CastingFluids.MOLTEN_SOUL.getBucket())
                .add(CastingFluids.MOLTEN_END_STONE.getBucket())
                .add(CastingFluids.MOLTEN_REDSTONE_ALLOY.getBucket())

        ;


    }
}
