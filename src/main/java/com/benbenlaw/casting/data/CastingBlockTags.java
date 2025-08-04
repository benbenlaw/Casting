package com.benbenlaw.casting.data;

import com.benbenlaw.casting.Casting;
import com.benbenlaw.casting.block.CastingBlocks;
import com.benbenlaw.casting.util.CastingTags;
import net.minecraft.core.HolderLookup;
import net.minecraft.data.PackOutput;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.tags.BlockTags;
import net.minecraft.world.level.block.Blocks;
import net.neoforged.neoforge.common.Tags;
import net.neoforged.neoforge.common.data.BlockTagsProvider;
import net.neoforged.neoforge.common.data.ExistingFileHelper;
import org.jetbrains.annotations.NotNull;

import java.util.concurrent.CompletableFuture;

public class CastingBlockTags extends BlockTagsProvider {

    CastingBlockTags(PackOutput output, CompletableFuture<HolderLookup.Provider> lookupProvider, ExistingFileHelper existingFileHelper) {
        super(output, lookupProvider, Casting.MOD_ID, existingFileHelper);
    }

    @Override
    protected void addTags(HolderLookup.@NotNull Provider provider) {

        //Lava Walker
        tag(CastingTags.Blocks.EFFECTED_BY_LAVA_WALKER)
                .add(Blocks.LAVA)
                .addOptional(ResourceLocation.parse("netherdepthsupgrade:warped_seagrass"))
                .addOptional(ResourceLocation.parse("netherdepthsupgrade:tall_warped_seagrass"))
                .addOptional(ResourceLocation.parse("netherdepthsupgrade:warped_kelp_plant"))
                .addOptional(ResourceLocation.parse("netherdepthsupgrade:crimson_seagrass"))
                .addOptional(ResourceLocation.parse("netherdepthsupgrade:tall_crimson_seagrass"))
                .addOptional(ResourceLocation.parse("netherdepthsupgrade:crimson_kelp_plant"));

        //Water Walker
        tag(CastingTags.Blocks.EFFECTED_BY_WATER_WALKER)
                .add(Blocks.WATER);

        //All Controller Blocks
        tag(CastingTags.Blocks.CONTROLLER_ALL)
                .addTag(CastingTags.Blocks.CONTROLLER_FLOORS)
                .addTag(CastingTags.Blocks.CONTROLLER_WALLS)
                .addTag(CastingTags.Blocks.CONTROLLER_EXTRA_BLOCKS)
                .addTag(CastingTags.Blocks.CONTROLLER_TANKS)
                .add(CastingBlocks.MULTIBLOCK_CONTROLLER.get());


        //Controller Floors
        tag(CastingTags.Blocks.CONTROLLER_FLOORS)
                .add(CastingBlocks.BLACK_BRICKS.get())
        ;

        //Controller Walls
        tag(CastingTags.Blocks.CONTROLLER_WALLS)
                .addTag(CastingTags.Blocks.CONTROLLER_EXTRA_BLOCKS)
                .add(CastingBlocks.BLACK_BRICK_GLASS.get())
                .add(CastingBlocks.BLACK_BRICKS.get())
                .add(CastingBlocks.MULTIBLOCK_CONTROLLER.get())
                .add(CastingBlocks.MULTIBLOCK_REGULATOR.get())
        ;

        //Controller Tanks
        tag(CastingTags.Blocks.CONTROLLER_TANKS)
                .add(CastingBlocks.MULTIBLOCK_FUEL_TANK.get())
                .add(CastingBlocks.MULTIBLOCK_COOLANT_TANK.get());

        //Controller Extra Blocks, includes any non wall and floor block to add to the multiblock data
        tag(CastingTags.Blocks.CONTROLLER_EXTRA_BLOCKS)
                .addTag(CastingTags.Blocks.CONTROLLER_TANKS)
                .add(CastingBlocks.MULTIBLOCK_SOLIDIFIER.get())
                .add(CastingBlocks.MULTIBLOCK_VALVE.get())
                .add(CastingBlocks.MULTIBLOCK_MIXER.get())
                .add(CastingBlocks.MULTIBLOCK_REGULATOR.get())
        ;

        tag(BlockTags.MINEABLE_WITH_PICKAXE)
                .add(CastingBlocks.MULTIBLOCK_CONTROLLER.get())
                .add(CastingBlocks.BLACK_BRICKS.get())
                .add(CastingBlocks.BLACK_BRICK_GLASS.get())
                .add(CastingBlocks.MULTIBLOCK_FUEL_TANK.get())
                .add(CastingBlocks.MULTIBLOCK_COOLANT_TANK.get())
                .add(CastingBlocks.MULTIBLOCK_SOLIDIFIER.get())
                .add(CastingBlocks.MULTIBLOCK_VALVE.get())
                .add(CastingBlocks.MULTIBLOCK_MIXER.get())
                .add(CastingBlocks.MULTIBLOCK_REGULATOR.get())

                //OG Casting
                .add(CastingBlocks.TANK.get())
                .add(CastingBlocks.SOLIDIFIER.get())
                .add(CastingBlocks.MIXER.get())
                .add(CastingBlocks.EQUIPMENT_MODIFIER.get())
                .add(CastingBlocks.CONTROLLER.get())
                .add(CastingBlocks.MIXER_WHISK.get())


        ;


    }

}
