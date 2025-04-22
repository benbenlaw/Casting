package com.benbenlaw.casting.data;

import com.benbenlaw.casting.Casting;
import com.benbenlaw.casting.block.ModBlocks;
import net.minecraft.core.HolderLookup;
import net.minecraft.data.PackOutput;
import net.minecraft.tags.BlockTags;
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

        tag(BlockTags.MINEABLE_WITH_PICKAXE)
                .add(ModBlocks.BLACK_BRICKS.get())
                .add(ModBlocks.CONTROLLER.get())
                .add(ModBlocks.TANK.get())
                .add(ModBlocks.SOLIDIFIER.get())
                .add(ModBlocks.MIXER.get())
                .add(ModBlocks.MIXER_WHISK.get())
                .add(ModBlocks.EQUIPMENT_MODIFIER.get())
        ;


    }

}
