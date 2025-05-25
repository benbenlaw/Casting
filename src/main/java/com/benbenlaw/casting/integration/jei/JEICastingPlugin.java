package com.benbenlaw.casting.integration.jei;

import com.benbenlaw.casting.Casting;
import com.benbenlaw.casting.block.CastingBlocks;
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
public class JEICastingPlugin implements IModPlugin {

    public static IDrawableStatic slotDrawable;
    public static RecipeType<MeltingRecipe> MELTING_RECIPE = new RecipeType<>(MeltingRecipeCategory.UID, MeltingRecipe.class);
    public static RecipeType<SolidifierRecipe> SOLIDIFIER_RECIPE = new RecipeType<>(SolidifierRecipeCategory.UID, SolidifierRecipe.class);
    public static RecipeType<FuelRecipe> FUEL_RECIPE = new RecipeType<>(FuelRecipeCategory.UID, FuelRecipe.class);
    public static RecipeType<CoolantRecipe> COOLANT_RECIPE = new RecipeType<>(CoolantRecipeCategory.UID, CoolantRecipe.class);
    public static RecipeType<MixingRecipe> MIXER_RECIPE = new RecipeType<>(MixingRecipeCategory.UID, MixingRecipe.class);
    public static RecipeType<EquipmentModifierRecipe> EQUIPMENT_MODIFIER_RECIPE = new RecipeType<>(EquipmentModifierRecipeCategory.UID, EquipmentModifierRecipe.class);

   //public static RecipeType<EquipmentModifierRecipe> TOOL_MODIFIER_RECIPE =
   //        new RecipeType<>(EquipmentModifierRecipeCategory.UID, EquipmentModifierRecipe.class);

    @Override
    public ResourceLocation getPluginUid() {
        return ResourceLocation.fromNamespaceAndPath(Casting.MOD_ID, "jei_plugin");
    }

    @Override
    public void registerRecipeCatalysts(@NotNull IRecipeCatalystRegistration registration) {
        registration.addRecipeCatalyst(new ItemStack(CastingBlocks.MULTIBLOCK_FUEL_TANK.get()), FuelRecipeCategory.RECIPE_TYPE);
        registration.addRecipeCatalyst(new ItemStack(CastingBlocks.TANK.get()), FuelRecipeCategory.RECIPE_TYPE);

        registration.addRecipeCatalyst(new ItemStack(CastingBlocks.MULTIBLOCK_COOLANT_TANK.get()), CoolantRecipeCategory.RECIPE_TYPE);
        registration.addRecipeCatalyst(new ItemStack(CastingBlocks.TANK.get()), CoolantRecipeCategory.RECIPE_TYPE);

        registration.addRecipeCatalyst(new ItemStack(CastingBlocks.MULTIBLOCK_SOLIDIFIER.get()), SolidifierRecipeCategory.RECIPE_TYPE);
        registration.addRecipeCatalyst(new ItemStack(CastingBlocks.SOLIDIFIER.get()), SolidifierRecipeCategory.RECIPE_TYPE);

        registration.addRecipeCatalyst(new ItemStack(CastingBlocks.MULTIBLOCK_CONTROLLER.get()), MeltingRecipeCategory.RECIPE_TYPE);
        registration.addRecipeCatalyst(new ItemStack(CastingBlocks.CONTROLLER.get()), MeltingRecipeCategory.RECIPE_TYPE);

        registration.addRecipeCatalyst(new ItemStack(CastingBlocks.MULTIBLOCK_MIXER.get()), MixingRecipeCategory.RECIPE_TYPE);
        registration.addRecipeCatalyst(new ItemStack(CastingBlocks.MIXER.get()), MixingRecipeCategory.RECIPE_TYPE);

        registration.addRecipeCatalyst(new ItemStack(CastingBlocks.EQUIPMENT_MODIFIER.get()), EquipmentModifierRecipeCategory.RECIPE_TYPE);
    }

    @Override
    public void registerCategories(IRecipeCategoryRegistration registration) {

        IGuiHelper guiHelper = registration.getJeiHelpers().getGuiHelper();

        registration.addRecipeCategories(new FuelRecipeCategory(registration.getJeiHelpers().getGuiHelper()));
        registration.addRecipeCategories(new CoolantRecipeCategory(registration.getJeiHelpers().getGuiHelper()));
        registration.addRecipeCategories(new SolidifierRecipeCategory(registration.getJeiHelpers().getGuiHelper()));
        registration.addRecipeCategories(new MeltingRecipeCategory(registration.getJeiHelpers().getGuiHelper()));
        registration.addRecipeCategories(new MixingRecipeCategory(registration.getJeiHelpers().getGuiHelper()));
        registration.addRecipeCategories(new EquipmentModifierRecipeCategory(registration.getJeiHelpers().getGuiHelper()));

        slotDrawable = guiHelper.getSlotDrawable();
    }



    @Override
    public void registerRecipes(IRecipeRegistration registration) {
        assert Minecraft.getInstance().level != null;
        final var recipeManager = Minecraft.getInstance().level.getRecipeManager();

        registration.addRecipes(FuelRecipeCategory.RECIPE_TYPE,
                recipeManager.getAllRecipesFor(CastingRecipes.FUEL_TYPE.get()).stream().map(RecipeHolder::value).toList());

        registration.addRecipes(CoolantRecipeCategory.RECIPE_TYPE,
                recipeManager.getAllRecipesFor(CastingRecipes.COOLANT_TYPE.get()).stream().map(RecipeHolder::value).toList());

        registration.addRecipes(MeltingRecipeCategory.RECIPE_TYPE,
                recipeManager.getAllRecipesFor(CastingRecipes.MELTING_TYPE.get()).stream().map(RecipeHolder::value).toList());

        registration.addRecipes(SolidifierRecipeCategory.RECIPE_TYPE,
                recipeManager.getAllRecipesFor(CastingRecipes.SOLIDIFIER_TYPE.get()).stream().map(RecipeHolder::value).toList());

        registration.addRecipes(MixingRecipeCategory.RECIPE_TYPE,
                recipeManager.getAllRecipesFor(CastingRecipes.MIXING_TYPE.get()).stream().map(RecipeHolder::value).toList());

        registration.addRecipes(EquipmentModifierRecipeCategory.RECIPE_TYPE,
                recipeManager.getAllRecipesFor(CastingRecipes.EQUIPMENT_MODIFIER_TYPE.get()).stream().map(RecipeHolder::value).toList());


    }

}


