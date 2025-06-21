package com.benbenlaw.casting.data;

import com.benbenlaw.casting.Casting;
import com.benbenlaw.core.tag.ModdedTagBuilder;
import net.minecraft.core.HolderLookup;
import net.minecraft.data.PackOutput;
import net.minecraft.data.tags.FluidTagsProvider;
import net.minecraft.tags.TagKey;
import net.minecraft.world.level.material.Fluid;
import net.neoforged.neoforge.common.data.ExistingFileHelper;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.concurrent.CompletableFuture;

import static com.benbenlaw.casting.fluid.CastingFluids.FLUIDS_MAP;

public class CastingFluidTags extends FluidTagsProvider {


    public CastingFluidTags(PackOutput output, CompletableFuture<HolderLookup.Provider> lookupProvider, String modId, @Nullable ExistingFileHelper existingFileHelper) {
        super(output, lookupProvider, Casting.MOD_ID, existingFileHelper);
    }

    @Override
    protected void addTags(HolderLookup.@NotNull Provider provider) {

        //Fluids
        for (var entry : FLUIDS_MAP.entrySet()) {

            TagKey<Fluid> tag = ModdedTagBuilder.createNeoFabricFluidTag(entry.getKey());

            tag(tag).add(entry.getValue().getFluid());
            tag(tag).add(entry.getValue().getFlowingFluid());
        }
    }
}
