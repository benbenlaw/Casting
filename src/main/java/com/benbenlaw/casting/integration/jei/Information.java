package com.benbenlaw.casting.integration.jei;

import com.benbenlaw.casting.Casting;
import com.benbenlaw.casting.block.CastingBlocks;
import com.benbenlaw.casting.item.CastingItems;
import mezz.jei.api.IModPlugin;
import mezz.jei.api.JeiPlugin;
import mezz.jei.api.constants.VanillaTypes;
import mezz.jei.api.registration.IRecipeRegistration;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.item.ItemStack;
import org.jetbrains.annotations.NotNull;

@JeiPlugin
public class Information implements IModPlugin {

    @Override
    public @NotNull ResourceLocation getPluginUid() {
        return ResourceLocation.fromNamespaceAndPath(Casting.MOD_ID, "information");
    }

    @Override
    public void registerRecipes(IRecipeRegistration reg) {

        reg.addIngredientInfo(new ItemStack(CastingBlocks.SOLIDIFIER), VanillaTypes.ITEM_STACK,
                Component.translatable("gui.casting.jei.information.solidifier"));

        reg.addIngredientInfo(new ItemStack(CastingBlocks.TANK), VanillaTypes.ITEM_STACK,
                Component.translatable("gui.casting.jei.information.tank"));

        reg.addIngredientInfo(new ItemStack(CastingBlocks.MIXER), VanillaTypes.ITEM_STACK,
                Component.translatable("gui.casting.jei.information.mixer"));

        reg.addIngredientInfo(new ItemStack(CastingBlocks.CONTROLLER), VanillaTypes.ITEM_STACK,
                Component.translatable("gui.casting.jei.information.controller"));

        reg.addIngredientInfo(new ItemStack(CastingItems.FLUID_MOVER.get()), VanillaTypes.ITEM_STACK,
                Component.translatable("gui.casting.jei.information.fluid_mover"));

        reg.addIngredientInfo(new ItemStack(CastingBlocks.MIXER_WHISK), VanillaTypes.ITEM_STACK,
                Component.translatable("gui.casting.jei.information.mixer_whisk"));

        reg.addIngredientInfo(new ItemStack(CastingBlocks.EQUIPMENT_MODIFIER), VanillaTypes.ITEM_STACK,
                Component.translatable("gui.casting.jei.information.equipment_modifier"));

    }

}
