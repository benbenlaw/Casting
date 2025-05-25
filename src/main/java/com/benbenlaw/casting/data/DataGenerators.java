package com.benbenlaw.casting.data;


import com.benbenlaw.casting.Casting;
import net.minecraft.core.HolderLookup;
import net.minecraft.data.DataGenerator;
import net.minecraft.data.PackOutput;
import net.minecraft.data.loot.LootTableProvider;
import net.minecraft.world.level.storage.loot.parameters.LootContextParamSets;
import net.neoforged.bus.api.SubscribeEvent;
import net.neoforged.fml.common.EventBusSubscriber;
import net.neoforged.neoforge.data.event.GatherDataEvent;

import java.util.Collections;
import java.util.List;
import java.util.concurrent.CompletableFuture;

@EventBusSubscriber(modid = Casting.MOD_ID, bus = EventBusSubscriber.Bus.MOD)
public class DataGenerators {

    @SubscribeEvent
    public static void gatherData(GatherDataEvent event) {

        DataGenerator generator = event.getGenerator();
        PackOutput packOutput = generator.getPackOutput();
        CompletableFuture<HolderLookup.Provider> lookupProvider = event.getLookupProvider();


        generator.addProvider(event.includeServer(), new CastingRecipeProvider(packOutput, event.getLookupProvider()));

        generator.addProvider(event.includeServer(), new LootTableProvider(packOutput, Collections.emptySet(),
                List.of(new LootTableProvider.SubProviderEntry(CastingLootTableProvider::new, LootContextParamSets.BLOCK)), event.getLookupProvider()));


        CastingBlockTags blockTags = new CastingBlockTags(packOutput, lookupProvider, event.getExistingFileHelper());
        generator.addProvider(event.includeServer(), blockTags);

        CastingItemTags itemTags = new CastingItemTags(packOutput, lookupProvider, blockTags, event.getExistingFileHelper());
        generator.addProvider(event.includeServer(), itemTags);

        CastingFluidTags fluidTags = new CastingFluidTags(packOutput, lookupProvider, Casting.MOD_ID, event.getExistingFileHelper());
        generator.addProvider(event.includeServer(), fluidTags);

        generator.addProvider(event.includeClient(), new CastingItemModelProvider(packOutput, event.getExistingFileHelper()));
        generator.addProvider(event.includeClient(), new CastingBlockStatesProvider(packOutput, event.getExistingFileHelper()));

        generator.addProvider(event.includeClient(), new CastingLangProvider(packOutput, event.getExistingFileHelper()));



    }


}
