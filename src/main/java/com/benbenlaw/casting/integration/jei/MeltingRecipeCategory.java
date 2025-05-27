package com.benbenlaw.casting.integration.jei;

import com.benbenlaw.casting.Casting;
import com.benbenlaw.casting.block.CastingBlocks;
import com.benbenlaw.casting.config.CastingConfig;
import com.benbenlaw.casting.recipe.MeltingRecipe;
import com.benbenlaw.casting.util.CastingTags;
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

public class MeltingRecipeCategory implements IRecipeCategory<MeltingRecipe> {
    public final static ResourceLocation UID = ResourceLocation.fromNamespaceAndPath(Casting.MOD_ID, "melting");
    public final static ResourceLocation TEXTURE =
            ResourceLocation.fromNamespaceAndPath(Casting.MOD_ID, "textures/gui/jei_melting.png");

    public static final RecipeType<MeltingRecipe> RECIPE_TYPE = RecipeType.create(Casting.MOD_ID, "melting",
            MeltingRecipe.class);

    private IDrawable background;
    private final IDrawable icon;
    private final IGuiHelper helper;

    public @Nullable ResourceLocation getRegistryName(MeltingRecipe recipe) {
        assert Minecraft.getInstance().level != null;
        return Minecraft.getInstance().level.getRecipeManager().getAllRecipesFor(MeltingRecipe.Type.INSTANCE).stream()
                .filter(recipeHolder -> recipeHolder.value().equals(recipe))
                .map(RecipeHolder::id)
                .findFirst()
                .orElse(null);
    }

    public MeltingRecipeCategory(IGuiHelper helper) {
        this.helper = helper;
        this.background = helper.createDrawable(TEXTURE, 0, 0, 120, 19);
        this.icon = helper.createDrawableIngredient(VanillaTypes.ITEM_STACK, new ItemStack(CastingBlocks.MULTIBLOCK_CONTROLLER.get()));
    }

    @Override
    public @NotNull RecipeType<MeltingRecipe> getRecipeType() {
        return JEICastingPlugin.MELTING_RECIPE;
    }

    @Override
    public @NotNull Component getTitle() {
        return Component.translatable("gui.casting.jei.melting");
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
    public void setRecipe(IRecipeLayoutBuilder builder, MeltingRecipe recipe, IFocusGroup focusGroup) {

        builder.addSlot(RecipeIngredientRole.INPUT, 4, 2).addIngredients(recipe.getIngredients().getFirst());

        int amount = recipe.output().getAmount();
        if (recipe.input().ingredient().getItems()[0].is(CastingTags.Items.MELTING_OUTPUT_AMOUNT_EFFECTED)) {

            amount = (int) (recipe.output().getAmount() * CastingConfig.oreMultiplier.get());

            int finalAmount1 = amount;
            builder.addSlot(RecipeIngredientRole.OUTPUT, 50, 2).addFluidStack(recipe.output().getFluid(), 1000)
                    .addRichTooltipCallback((slot, tooltip) ->
                            tooltip.add(Component.translatable("gui.casting.jei.multiblock_controller_output", finalAmount1).withStyle(ChatFormatting.GOLD)))
                    .addRichTooltipCallback((slot, tooltip) ->
                            tooltip.add(Component.translatable("gui.casting.jei.simple_controller_output", recipe.output().getAmount()).withStyle(ChatFormatting.GOLD)))
                    .addRichTooltipCallback((slot, tooltip) ->
                            tooltip.add(Component.translatable("gui.casting.jei.melting_temp", recipe.meltingTemp()).withStyle(ChatFormatting.GOLD)));



        } else {

            int finalAmount2 = amount;
            builder.addSlot(RecipeIngredientRole.OUTPUT, 50, 2).addFluidStack(recipe.output().getFluid(), 1000)
                    .addRichTooltipCallback((slot, tooltip) ->
                            tooltip.add(Component.literal(finalAmount2 + "mB").withStyle(ChatFormatting.GOLD)))
                    .addRichTooltipCallback((slot, tooltip) ->
                            tooltip.add(Component.translatable("gui.casting.jei.melting_temp", recipe.meltingTemp()).withStyle(ChatFormatting.GOLD)));
        }


    }

    @Override
    public void draw(MeltingRecipe recipe, IRecipeSlotsView recipeSlotsView, GuiGraphics guiGraphics, double mouseX, double mouseY) {
        final Minecraft minecraft = Minecraft.getInstance();
        int temp = recipe.meltingTemp();
        guiGraphics.drawString(minecraft.font.self(), Component.literal(String.valueOf(temp)), 80, 11, Color.GRAY.getRGB(), false);

        int amount = recipe.output().getAmount();

        if (recipe.input().ingredient().getItems()[0].isDamageableItem()) {
            guiGraphics.drawString(minecraft.font.self(), Component.literal("1-" + amount).append("mb"), 80, 1, Color.GRAY.getRGB(), false);
        }

        else {
            if (recipe.input().ingredient().getItems()[0].is(CastingTags.Items.MELTING_OUTPUT_AMOUNT_EFFECTED)) {
                guiGraphics.drawString(minecraft.font.self(), Component.literal("*" + amount).append("mb"), 80, 1, Color.GRAY.getRGB(), false);
            } else {
                guiGraphics.drawString(minecraft.font.self(), Component.literal(String.valueOf(amount)).append("mb"), 80, 1, Color.GRAY.getRGB(), false);
            }
        }
    }
}