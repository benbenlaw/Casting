package com.benbenlaw.casting.integration.jei;

import com.benbenlaw.casting.Casting;
import com.benbenlaw.casting.block.CastingBlocks;
import com.benbenlaw.casting.recipe.SolidifierRecipe;
import mezz.jei.api.constants.VanillaTypes;
import mezz.jei.api.gui.builder.IRecipeLayoutBuilder;
import mezz.jei.api.gui.drawable.IDrawable;
import mezz.jei.api.gui.ingredient.IRecipeSlotsView;
import mezz.jei.api.helpers.IGuiHelper;
import mezz.jei.api.recipe.IFocusGroup;
import mezz.jei.api.recipe.RecipeIngredientRole;
import mezz.jei.api.recipe.RecipeType;
import mezz.jei.api.recipe.category.IRecipeCategory;
import net.minecraft.ChatFormatting;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.crafting.RecipeHolder;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.awt.*;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Stream;

public class SolidifierRecipeCategory implements IRecipeCategory<SolidifierRecipe> {
    public final static ResourceLocation UID = ResourceLocation.fromNamespaceAndPath(Casting.MOD_ID, "solidifier");
    public final static ResourceLocation TEXTURE =
            ResourceLocation.fromNamespaceAndPath(Casting.MOD_ID, "textures/gui/jei_solidifier.png");

    public static final RecipeType<SolidifierRecipe> RECIPE_TYPE = RecipeType.create(Casting.MOD_ID, "solidifier",
            SolidifierRecipe.class);

    private IDrawable background;
    private final IDrawable icon;
    private final IGuiHelper helper;

    public @Nullable ResourceLocation getRegistryName(SolidifierRecipe recipe) {
        assert Minecraft.getInstance().level != null;
        return Minecraft.getInstance().level.getRecipeManager().getAllRecipesFor(SolidifierRecipe.Type.INSTANCE).stream()
                .filter(recipeHolder -> recipeHolder.value().equals(recipe))
                .map(RecipeHolder::id)
                .findFirst()
                .orElse(null);
    }

    public SolidifierRecipeCategory(IGuiHelper helper) {
        this.helper = helper;
        this.background = helper.createDrawable(TEXTURE, 0, 0, 132, 19);
        this.icon = helper.createDrawableIngredient(VanillaTypes.ITEM_STACK, new ItemStack(CastingBlocks.MULTIBLOCK_SOLIDIFIER.get()));
    }

    @Override
    public @NotNull RecipeType<SolidifierRecipe> getRecipeType() {
        return JEICastingPlugin.SOLIDIFIER_RECIPE;
    }

    @Override
    public @NotNull Component getTitle() {
        return Component.translatable("gui.casting.jei.solidifier");
    }

    @Override
    public @NotNull IDrawable getBackground() {
        return this.background;
    }

    @Override
    public @NotNull IDrawable getIcon() {
        return this.icon;
    }

    @Override
    public void setRecipe(IRecipeLayoutBuilder builder, SolidifierRecipe recipe, IFocusGroup focusGroup) {
        int fluidAmount = recipe.fluid().getAmount();

        List<Stream<ItemStack>> ingredients = List.of(Arrays.stream(recipe.output().getItems())).reversed();
        List<ItemStack> items = ingredients.stream().flatMap(stream -> stream).toList();

        builder.addSlot(RecipeIngredientRole.INPUT, 40, 2).addFluidStack(recipe.fluid().getFluid(), recipe.fluid().getAmount())
                .addRichTooltipCallback((slot, tooltip) ->
                        tooltip.add(Component.literal(recipe.fluid().getAmount() + " mB").withStyle(ChatFormatting.GOLD)));

        builder.addSlot(RecipeIngredientRole.RENDER_ONLY, 40, 2).addFluidStack(recipe.fluid().getFluid(), 1000);


        builder.addSlot(RecipeIngredientRole.CATALYST, 4, 2).addIngredients(VanillaTypes.ITEM_STACK, Arrays.asList(recipe.mold().getItems()));
        builder.addSlot(RecipeIngredientRole.OUTPUT, 113, 2).addItemStack(recipe.output().getItems()[0]);
    }

    public void draw(SolidifierRecipe recipe, IRecipeSlotsView recipeSlotsView, GuiGraphics guiGraphics, double mouseX, double mouseY) {
        final Minecraft minecraft = Minecraft.getInstance();
        int fluidAmount = recipe.fluid().getAmount();
        String fluidText = fluidAmount + "mb";
        int textWidth = minecraft.font.width(fluidText);
        int xPosition = 76 - (textWidth / 2);
        guiGraphics.drawString(minecraft.font.self(), Component.literal(fluidText), xPosition, 6, Color.GRAY.getRGB(), false);
    }
}