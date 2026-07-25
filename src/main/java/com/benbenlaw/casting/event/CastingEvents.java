package com.benbenlaw.casting.event;

import com.benbenlaw.casting.Casting;
import com.benbenlaw.casting.block.CastingBlockEntities;
import com.benbenlaw.casting.event.client.ClientRecipeCache;
import com.benbenlaw.casting.recipe.CastingRecipeTypes;
import com.benbenlaw.casting.recipe.custom.FuelRecipe;
import com.benbenlaw.casting.recipe.custom.MeltingRecipe;
import com.benbenlaw.casting.recipe.custom.MixingRecipe;
import com.benbenlaw.casting.recipe.custom.SolidifierRecipe;
import net.minecraft.resources.Identifier;
import net.minecraft.world.entity.monster.piglin.PiglinAi;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.Items;
import net.minecraft.world.item.crafting.RecipeHolder;
import net.minecraft.world.item.crafting.RecipeMap;
import net.neoforged.bus.api.SubscribeEvent;
import net.neoforged.fml.common.EventBusSubscriber;
import net.neoforged.neoforge.capabilities.Capabilities;
import net.neoforged.neoforge.capabilities.RegisterCapabilitiesEvent;
import net.neoforged.neoforge.client.event.RecipesReceivedEvent;
import net.neoforged.neoforge.event.OnDatapackSyncEvent;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

@EventBusSubscriber(modid = Casting.MOD_ID)
public class CastingEvents {

    @SubscribeEvent
    public static void onDataPackSync(OnDatapackSyncEvent event) {
        event.sendRecipes(CastingRecipeTypes.MELTING_TYPE.get());
        event.sendRecipes(CastingRecipeTypes.SOLIDIFIER_TYPE.get());
        event.sendRecipes(CastingRecipeTypes.MIXING_TYPE.get());
        event.sendRecipes(CastingRecipeTypes.FUEL_TYPE.get());
    }

    @SubscribeEvent
    public static void onRecipeReceived(RecipesReceivedEvent event) {
        RecipeMap recipeMap = event.getRecipeMap();

        //Melting
        Collection<RecipeHolder<MeltingRecipe>> meltingRecipe = recipeMap.byType(CastingRecipeTypes.MELTING_TYPE.get());
        Map<Identifier, MeltingRecipe> meltingRecipeMap = new HashMap<>();

        for (RecipeHolder<MeltingRecipe> recipeHolder : meltingRecipe) {
            meltingRecipeMap.put(recipeHolder.id().identifier(), recipeHolder.value());
        }
        ClientRecipeCache.setCachedMeltingRecipes(meltingRecipeMap);

        //Solidifier
        Collection<RecipeHolder<SolidifierRecipe>> solidifierRecipe = recipeMap.byType(CastingRecipeTypes.SOLIDIFIER_TYPE.get());
        Map<Identifier, SolidifierRecipe> solidifierRecipeMap = new HashMap<>();

        for (RecipeHolder<SolidifierRecipe> recipeHolder : solidifierRecipe) {
            solidifierRecipeMap.put(recipeHolder.id().identifier(), recipeHolder.value());
        }
        ClientRecipeCache.setCachedSolidifierRecipes(solidifierRecipeMap);

        //Mixing
        Collection<RecipeHolder<MixingRecipe>> mixingRecipe = recipeMap.byType(CastingRecipeTypes.MIXING_TYPE.get());
        Map<Identifier, MixingRecipe> mixingRecipeMap = new HashMap<>();

        for (RecipeHolder<MixingRecipe> recipeHolder : mixingRecipe) {
            mixingRecipeMap.put(recipeHolder.id().identifier(), recipeHolder.value());
        }
        ClientRecipeCache.setCachedMixingRecipes(mixingRecipeMap);

        //Fuel
        Collection<RecipeHolder<FuelRecipe>> fuelRecipe = recipeMap.byType(CastingRecipeTypes.FUEL_TYPE.get());
        Map<Identifier, FuelRecipe> fuelRecipeMap = new HashMap<>();

        for (RecipeHolder<FuelRecipe> recipeHolder : fuelRecipe) {
            fuelRecipeMap.put(recipeHolder.id().identifier(), recipeHolder.value());
        }
        ClientRecipeCache.setCachedFuelRecipes(fuelRecipeMap);

    }

    @SubscribeEvent
    public static void registerCapabilities(RegisterCapabilitiesEvent event) {

        //Controller
        event.registerBlockEntity(Capabilities.Item.BLOCK, CastingBlockEntities.CONTROLLER_BLOCK_ENTITY.get(),
                (blockEntity, side) -> blockEntity.getItemHandler());

        event.registerBlockEntity(Capabilities.Fluid.BLOCK, CastingBlockEntities.CONTROLLER_BLOCK_ENTITY.get(),
                (blockEntity, side) -> blockEntity.getFluidHandler());

        //Solidifier
        event.registerBlockEntity(Capabilities.Item.BLOCK, CastingBlockEntities.SOLIDIFIER_BLOCK_ENTITY.get(),
                (blockEntity, side) -> blockEntity.getItemHandler());

        event.registerBlockEntity(Capabilities.Fluid.BLOCK, CastingBlockEntities.SOLIDIFIER_BLOCK_ENTITY.get(),
                (blockEntity, side) -> blockEntity.getFluidHandler());

        //Mixer
        event.registerBlockEntity(Capabilities.Fluid.BLOCK, CastingBlockEntities.MIXER_BLOCK_ENTITY.get(),
                (blockEntity, side) -> blockEntity.getFluidHandler());

        //Tank
        event.registerBlockEntity(Capabilities.Fluid.BLOCK, CastingBlockEntities.TANK_BLOCK_ENTITY.get(),
                (blockEntity, side) -> blockEntity.getFluidHandler());

    }

}
