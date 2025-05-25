package com.benbenlaw.casting.integration.jei;

import com.benbenlaw.casting.Casting;
import com.benbenlaw.casting.block.CastingBlocks;
import com.benbenlaw.casting.recipe.EquipmentModifierRecipe;
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
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.crafting.RecipeHolder;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Arrays;

public class EquipmentModifierRecipeCategory implements IRecipeCategory<EquipmentModifierRecipe> {
    public final static ResourceLocation UID = ResourceLocation.fromNamespaceAndPath(Casting.MOD_ID, "equipment_modifier");
    public final static ResourceLocation TEXTURE =
            ResourceLocation.fromNamespaceAndPath(Casting.MOD_ID, "textures/gui/jei_equipment_modifier.png");

    public static final RecipeType<EquipmentModifierRecipe> RECIPE_TYPE = RecipeType.create(Casting.MOD_ID, "equipment_modifier",
            EquipmentModifierRecipe.class);

    private IDrawable background;
    private final IDrawable icon;
    private final IGuiHelper helper;

    public @Nullable ResourceLocation getRegistryName(EquipmentModifierRecipe recipe) {
        assert Minecraft.getInstance().level != null;
        return Minecraft.getInstance().level.getRecipeManager().getAllRecipesFor(EquipmentModifierRecipe.Type.INSTANCE).stream()
                .filter(recipeHolder -> recipeHolder.value().equals(recipe))
                .map(RecipeHolder::id)
                .findFirst()
                .orElse(null);
    }

    public EquipmentModifierRecipeCategory(IGuiHelper helper) {
        this.helper = helper;
        this.background = helper.createDrawable(TEXTURE, 0, 0, 95, 19);
        this.icon = helper.createDrawableIngredient(VanillaTypes.ITEM_STACK, new ItemStack(CastingBlocks.EQUIPMENT_MODIFIER.get()));
    }

    @Override
    public @NotNull RecipeType<EquipmentModifierRecipe> getRecipeType() {
        return JEICastingPlugin.EQUIPMENT_MODIFIER_RECIPE;
    }

    @Override
    public @NotNull Component getTitle() {
        return Component.literal("Tool Modifier");
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
    public void setRecipe(IRecipeLayoutBuilder builder, EquipmentModifierRecipe recipe, IFocusGroup focusGroup) {

        ItemStack outputEffectAsItem = BuiltInRegistries.ITEM.get(ResourceLocation.fromNamespaceAndPath(Casting.MOD_ID, recipe.effect())).getDefaultInstance();


        builder.addSlot(RecipeIngredientRole.OUTPUT, 76, 2).addItemStack(outputEffectAsItem);

        if (Arrays.asList(recipe.upgradeItem().getItems()).isEmpty()) {
            builder.addSlot(RecipeIngredientRole.INPUT, 4, 2).addFluidStack(recipe.upgradeFluid().getFluid()).addRichTooltipCallback(
                    (ingredient, tooltip) -> tooltip.add(Component.translatable(recipe.upgradeFluid().getAmount() + "mB").withStyle(ChatFormatting.GOLD))
            );
        }

        if (recipe.upgradeFluid().isEmpty()) {
            builder.addSlot(RecipeIngredientRole.INPUT, 4, 2).addItemStacks(Arrays.asList(recipe.upgradeItem().getItems()));
        }

        else {
            builder.addSlot(RecipeIngredientRole.INPUT, 4, 2).addFluidStack(recipe.upgradeFluid().getFluid()).addRichTooltipCallback(
                    (ingredient, tooltip) -> tooltip.add(Component.translatable(recipe.upgradeFluid().getAmount() + "mB").withStyle(ChatFormatting.GOLD))
            );
            builder.addSlot(RecipeIngredientRole.INPUT, 40, 2).addItemStacks(Arrays.asList(recipe.upgradeItem().getItems()));
        }
    }
}