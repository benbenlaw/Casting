package com.benbenlaw.casting.integration.jei;

import com.benbenlaw.casting.Casting;
import com.benbenlaw.casting.block.ModBlocks;
import com.benbenlaw.casting.item.ModItems;
import com.benbenlaw.casting.recipe.FuelRecipe;
import mezz.jei.api.IModPlugin;
import mezz.jei.api.JeiPlugin;
import mezz.jei.api.constants.VanillaTypes;
import mezz.jei.api.ingredients.ITypedIngredient;
import mezz.jei.api.recipe.vanilla.IJeiIngredientInfoRecipe;
import mezz.jei.api.registration.IRecipeRegistration;
import net.minecraft.client.Minecraft;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.FormattedText;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.crafting.Ingredient;
import net.minecraft.world.item.crafting.RecipeHolder;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.annotations.Unmodifiable;

import java.util.List;

@JeiPlugin
public class Information implements IModPlugin {

    @Override
    public ResourceLocation getPluginUid() {
        return ResourceLocation.fromNamespaceAndPath(Casting.MOD_ID, "information");
    }

    @Override
    public void registerRecipes(IRecipeRegistration reg) {

        reg.addIngredientInfo(new ItemStack(ModBlocks.SOLIDIFIER), VanillaTypes.ITEM_STACK,
                Component.translatable("jei.casting.solidifier"));

        reg.addIngredientInfo(new ItemStack(ModBlocks.TANK), VanillaTypes.ITEM_STACK,
                Component.translatable("jei.casting.tank"));

        reg.addIngredientInfo(new ItemStack(ModBlocks.MIXER), VanillaTypes.ITEM_STACK,
                Component.translatable("jei.casting.mixer"));

        reg.addIngredientInfo(new ItemStack(ModBlocks.CONTROLLER), VanillaTypes.ITEM_STACK,
                Component.translatable("jei.casting.controller"));

        reg.addIngredientInfo(new ItemStack(ModItems.FLUID_MOVER.get()), VanillaTypes.ITEM_STACK,
                Component.translatable("jei.casting.fluid_mover"));

        reg.addIngredientInfo(new ItemStack(ModBlocks.MIXER_WHISK), VanillaTypes.ITEM_STACK,
                Component.translatable("jei.casting.mixer_whisk"));

        reg.addIngredientInfo(new ItemStack(ModBlocks.TOOL_MODIFIER), VanillaTypes.ITEM_STACK,
                Component.translatable("jei.casting.tool_modifier"));

    }

}
