package com.benbenlaw.casting.data;

import appeng.core.definitions.AEItems;
import com.benbenlaw.casting.Casting;
import com.benbenlaw.casting.item.CastingItems;
import com.benbenlaw.casting.util.CastingTags;
import net.minecraft.core.HolderLookup;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.core.registries.Registries;
import net.minecraft.data.PackOutput;
import net.minecraft.resources.Identifier;
import net.minecraft.tags.TagKey;
import net.minecraft.world.item.Items;
import net.minecraft.world.level.block.Blocks;
import net.neoforged.neoforge.common.Tags;
import net.neoforged.neoforge.common.data.ItemTagsProvider;
import org.jetbrains.annotations.NotNull;

import java.util.concurrent.CompletableFuture;

import static com.benbenlaw.casting.fluid.CastingFluids.FLUIDS_MAP;

public class CastingItemTags extends ItemTagsProvider {

    public CastingItemTags(PackOutput output, CompletableFuture<HolderLookup.Provider> lookupProvider) {
        super(output, lookupProvider, Casting.MOD_ID);
    }

    @Override
    protected void addTags(HolderLookup.@NotNull Provider provider) {

        //Ball Items
        tag(CastingTags.Items.BALL_ITEMS)
                .add(Items.SNOWBALL)
                .add(Items.SLIME_BALL)
                .add(Items.MAGMA_CREAM)
                .add(Items.FIRE_CHARGE)
                .add(Items.ENDER_PEARL)
                .add(Items.ENDER_EYE)
                .add(Items.CLAY_BALL);

        //All Shards
        tag(CastingTags.Items.SHARDS)
                .add(Items.AMETHYST_SHARD)
                .addOptionalTag(TagKey.create(Registries.ITEM, Identifier.parse("geore:geore_shards")))
        ;

        tag(CastingTags.Items.ENERGIZED_STEEL).addOptional(BuiltInRegistries.ITEM.getValue(Identifier.parse("powah:steel_energized")));
        tag(CastingTags.Items.ENERGIZED_STEEL_BLOCK).addOptional(BuiltInRegistries.ITEM.getValue(Identifier.parse("powah:energized_steel_block")));
        tag(CastingTags.Items.BLAZING_CRYSTAL).addOptional(BuiltInRegistries.ITEM.getValue(Identifier.parse("powah:crystal_blazing")));
        tag(CastingTags.Items.BLAZING_CRYSTAL_BLOCK).addOptional(BuiltInRegistries.ITEM.getValue(Identifier.parse("powah:blazing_crystal_block")));
        tag(CastingTags.Items.NIOTIC_CRYSTAL).addOptional(BuiltInRegistries.ITEM.getValue(Identifier.parse("powah:crystal_niotic")));
        tag(CastingTags.Items.NIOTIC_CRYSTAL_BLOCK).addOptional(BuiltInRegistries.ITEM.getValue(Identifier.parse("powah:niotic_crystal_block")));
        tag(CastingTags.Items.SPIRITED_CRYSTAL).addOptional(BuiltInRegistries.ITEM.getValue(Identifier.parse("powah:crystal_spirited")));
        tag(CastingTags.Items.SPIRITED_CRYSTAL_BLOCK).addOptional(BuiltInRegistries.ITEM.getValue(Identifier.parse("powah:spirited_crystal_block")));
        tag(CastingTags.Items.NITRO_CRYSTAL).addOptional(BuiltInRegistries.ITEM.getValue(Identifier.parse("powah:crystal_nitro")));
        tag(CastingTags.Items.NITRO_CRYSTAL_BLOCK).addOptional(BuiltInRegistries.ITEM.getValue(Identifier.parse("powah:nitro_crystal_block")));


        //Bricks
        tag(Tags.Items.BRICKS).add(CastingItems.BLACK_BRICK.asItem());

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
                .add(CastingItems.SHARD_MOLD.asItem())
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
        tag(CastingTags.Items.SHARD_MOLD).add(CastingItems.SHARD_MOLD.asItem());

        //Buckets
        for (var entry : FLUIDS_MAP.entrySet()) {
            tag(Tags.Items.BUCKETS).add(entry.getValue().getBucket());

        }

        //Processing Tags
        tag(TagKey.create(Registries.ITEM, Identifier.parse("c:storage_blocks/glowstone"))).add(Blocks.GLOWSTONE.asItem());
        tag(TagKey.create(Registries.ITEM, Identifier.parse("c:storage_blocks/quartz"))).add(Blocks.QUARTZ_BLOCK.asItem());
        tag(TagKey.create(Registries.ITEM, Identifier.parse("c:gems/charged_certus_quartz"))).add(AEItems.CERTUS_QUARTZ_CRYSTAL_CHARGED.get());

    }
}
