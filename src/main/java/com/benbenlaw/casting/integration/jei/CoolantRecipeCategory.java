package com.benbenlaw.casting.integration.jei;

import com.benbenlaw.casting.Casting;
import com.benbenlaw.casting.block.CastingBlocks;
import com.benbenlaw.casting.recipe.CoolantRecipe;
import mezz.jei.api.constants.VanillaTypes;
import mezz.jei.api.gui.builder.IRecipeLayoutBuilder;
import mezz.jei.api.gui.drawable.IDrawable;
import mezz.jei.api.helpers.IGuiHelper;
import mezz.jei.api.recipe.IFocusGroup;
import mezz.jei.api.recipe.RecipeIngredientRole;
import mezz.jei.api.recipe.RecipeType;
import mezz.jei.api.recipe.category.IRecipeCategory;
import net.minecraft.ChatFormatting;
import net.minecraft.client.Minecraft;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.crafting.RecipeHolder;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.awt.*;
import java.util.List;
import java.util.*;

public class CoolantRecipeCategory implements IRecipeCategory<CoolantRecipe> {
    public final static ResourceLocation UID = ResourceLocation.fromNamespaceAndPath(Casting.MOD_ID, "coolant");
    public final static ResourceLocation TEXTURE =
            ResourceLocation.fromNamespaceAndPath(Casting.MOD_ID, "textures/gui/jei_dynamic.png");

    public static final RecipeType<CoolantRecipe> RECIPE_TYPE = RecipeType.create(Casting.MOD_ID, "coolant",
            CoolantRecipe.class);

    private IDrawable background;
    private final IDrawable icon;
    private final IGuiHelper helper;
    private int tabs_used = 0;

    public CoolantRecipeCategory(IGuiHelper helper) {
        this.helper = helper;
        this.background = helper.createDrawable(TEXTURE, 0, 0, 159, 104);
        this.icon = helper.createDrawableIngredient(VanillaTypes.ITEM_STACK, new ItemStack(CastingBlocks.MULTIBLOCK_COOLANT_TANK.get()));
    }

    @Override
    public @NotNull RecipeType<CoolantRecipe> getRecipeType() {
        return JEICastingPlugin.COOLANT_RECIPE;
    }

    @Override
    public boolean isHandled(CoolantRecipe recipe) {
        return tabs_used == 0;
    }

    @Override
    public @NotNull Component getTitle() {
        return Component.translatable("gui.casting.jei.coolant");
    }

    @Override
    public @NotNull IDrawable getBackground() {
        return this.background;
    }

    @Override
    public @NotNull IDrawable getIcon() {
        return this.icon;
    }

    public @Nullable ResourceLocation getRegistryName(CoolantRecipe recipe) {
        assert Minecraft.getInstance().level != null;
        return Minecraft.getInstance().level.getRecipeManager().getAllRecipesFor(CoolantRecipe.Type.INSTANCE).stream()
                .filter(recipeHolder -> recipeHolder.value().equals(recipe))
                .map(RecipeHolder::id)
                .findFirst()
                .orElse(null);
    }

    private final Map<Point, CoolantRecipe> slotRecipes = new HashMap<>();
    private int backgroundWidth;

    @Override
    public void setRecipe(IRecipeLayoutBuilder builder, CoolantRecipe recipe, IFocusGroup focusGroup) {
        tabs_used++;

        List<CoolantRecipe> recipes = new ArrayList<>(Minecraft.getInstance().level.getRecipeManager().getAllRecipesFor(CoolantRecipe.Type.INSTANCE)).stream().map(RecipeHolder::value).toList();

        List<CoolantRecipe> mutableRecipes = new ArrayList<>(recipes);

        mutableRecipes.sort(Comparator.comparingInt(CoolantRecipe::duration).reversed());

        // Background Size
        int yOffset = 1;

        int numRows = (int) Math.ceil((double) mutableRecipes.size() / 9);
        int numCols = 8; //Math.min(9, mutableRecipes.size()); // Maximum of 9 columns
        int backgroundHeight = 2 + numRows * 18 + yOffset;

        background = helper.createDrawable(TEXTURE, 0, 0, 148, backgroundHeight);

        slotRecipes.clear();

        for (int i = 0; i < mutableRecipes.size(); i++) {
            final int slotX = 3 + (i % 8 * 18);
            final int slotY = yOffset + 2 + i / 8 * 18; // Add yOffset to the Y position

            CoolantRecipe coolant = mutableRecipes.get(i);

            builder.addSlot(RecipeIngredientRole.INPUT, slotX, slotY)
                    .addFluidStack(coolant.getFluid(), 1000)
                    .addRichTooltipCallback((slot, tooltip) ->
                            tooltip.add(Component.literal(coolant.getFluidStack().getAmount() + "mB").withStyle(ChatFormatting.GOLD)))
                    .addRichTooltipCallback((slot, tooltip) ->
                            tooltip.add(Component.translatable("gui.casting.jei.coolant_speed", coolant.duration()).withStyle(ChatFormatting.GOLD)))
                    .setBackground(JEICastingPlugin.slotDrawable, slotX - (i % 8 * 18) - 4, slotY - (yOffset + 2 + i / 8 * 18) - 1);

            // Store the position of this slot with its corresponding recipe
            slotRecipes.put(new Point(slotX, slotY), mutableRecipes.get(i));
        }
    }
}