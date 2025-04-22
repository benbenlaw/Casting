package com.benbenlaw.casting.integration.jei;

import com.benbenlaw.casting.Casting;
import com.benbenlaw.casting.block.ModBlocks;
import com.benbenlaw.casting.recipe.*;
import mezz.jei.api.IModPlugin;
import mezz.jei.api.JeiPlugin;
import mezz.jei.api.gui.drawable.IDrawableStatic;
import mezz.jei.api.helpers.IGuiHelper;
import mezz.jei.api.recipe.RecipeType;
import mezz.jei.api.registration.IRecipeCatalystRegistration;
import mezz.jei.api.registration.IRecipeCategoryRegistration;
import mezz.jei.api.registration.IRecipeRegistration;
import net.minecraft.client.Minecraft;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.crafting.RecipeHolder;
import org.jetbrains.annotations.NotNull;


@JeiPlugin
public class JEISmeltingPlugin implements IModPlugin {

    public static IDrawableStatic slotDrawable;
    public static RecipeType<MeltingRecipe> MELTING_RECIPE =
            new RecipeType<>(MeltingRecipeCatagory.UID, MeltingRecipe.class);

    public static RecipeType<SolidifierRecipe> SOLIDIFIER_RECIPE =
            new RecipeType<>(SolidifierRecipeCategory.UID, SolidifierRecipe.class);

    public static RecipeType<FuelRecipe> FUEL_RECIPE =
            new RecipeType<>(FuelRecipeCategory.UID, FuelRecipe.class);

    public static RecipeType<MixingRecipe> MIXER_RECIPE =
            new RecipeType<>(MixingRecipeCatagory.UID, MixingRecipe.class);

    public static RecipeType<EquipmentModifierRecipe> TOOL_MODIFIER_RECIPE =
            new RecipeType<>(EquipmentModifierRecipeCategory.UID, EquipmentModifierRecipe.class);

    @Override
    public ResourceLocation getPluginUid() {
        return ResourceLocation.fromNamespaceAndPath(Casting.MOD_ID, "jei_plugin");
    }

    @Override
    public void registerRecipeCatalysts(@NotNull IRecipeCatalystRegistration registration) {
        registration.addRecipeCatalyst(new ItemStack(ModBlocks.TANK.get()), FuelRecipeCategory.RECIPE_TYPE);
        registration.addRecipeCatalyst(new ItemStack(ModBlocks.SOLIDIFIER.get()), SolidifierRecipeCategory.RECIPE_TYPE);
        registration.addRecipeCatalyst(new ItemStack(ModBlocks.CONTROLLER.get()), MeltingRecipeCatagory.RECIPE_TYPE);
        registration.addRecipeCatalyst(new ItemStack(ModBlocks.MIXER.get()), MixingRecipeCatagory.RECIPE_TYPE);
        registration.addRecipeCatalyst(new ItemStack(ModBlocks.EQUIPMENT_MODIFIER.get()), EquipmentModifierRecipeCategory.RECIPE_TYPE);
    }

    @Override
    public void registerCategories(IRecipeCategoryRegistration registration) {

        IGuiHelper guiHelper = registration.getJeiHelpers().getGuiHelper();

        registration.addRecipeCategories(new
                FuelRecipeCategory(registration.getJeiHelpers().getGuiHelper()));

        registration.addRecipeCategories(new
                SolidifierRecipeCategory(registration.getJeiHelpers().getGuiHelper()));

        registration.addRecipeCategories(new
                MeltingRecipeCatagory(registration.getJeiHelpers().getGuiHelper()));

        registration.addRecipeCategories(new
                MixingRecipeCatagory(registration.getJeiHelpers().getGuiHelper()));

        registration.addRecipeCategories(new
                EquipmentModifierRecipeCategory(registration.getJeiHelpers().getGuiHelper()));




        slotDrawable = guiHelper.getSlotDrawable();
    }



    @Override
    public void registerRecipes(IRecipeRegistration registration) {
        assert Minecraft.getInstance().level != null;
        final var recipeManager = Minecraft.getInstance().level.getRecipeManager();

        registration.addRecipes(FuelRecipeCategory.RECIPE_TYPE,
                recipeManager.getAllRecipesFor(ModRecipes.FUEL_TYPE.get()).stream().map(RecipeHolder::value).toList());


        registration.addRecipes(MeltingRecipeCatagory.RECIPE_TYPE,
                recipeManager.getAllRecipesFor(ModRecipes.MELTING_TYPE.get()).stream().map(RecipeHolder::value).toList());

        registration.addRecipes(SolidifierRecipeCategory.RECIPE_TYPE,
                recipeManager.getAllRecipesFor(ModRecipes.SOLIDIFIER_TYPE.get()).stream().map(RecipeHolder::value).toList());

        registration.addRecipes(MixingRecipeCatagory.RECIPE_TYPE,
                recipeManager.getAllRecipesFor(ModRecipes.MIXING_TYPE.get()).stream().map(RecipeHolder::value).toList());

        registration.addRecipes(EquipmentModifierRecipeCategory.RECIPE_TYPE,
                recipeManager.getAllRecipesFor(ModRecipes.EQUIPMENT_MODIFIER_TYPE.get()).stream().map(RecipeHolder::value).toList());


    }

}


