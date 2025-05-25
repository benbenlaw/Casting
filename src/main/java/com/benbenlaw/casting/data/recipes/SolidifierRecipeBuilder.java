package com.benbenlaw.casting.data.recipes;

import com.benbenlaw.casting.Casting;
import com.benbenlaw.casting.recipe.SolidifierRecipe;
import net.minecraft.advancements.Advancement;
import net.minecraft.advancements.AdvancementRequirements;
import net.minecraft.advancements.AdvancementRewards;
import net.minecraft.advancements.Criterion;
import net.minecraft.advancements.critereon.RecipeUnlockedTrigger;
import net.minecraft.data.recipes.RecipeBuilder;
import net.minecraft.data.recipes.RecipeOutput;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.neoforged.neoforge.common.crafting.SizedIngredient;
import net.neoforged.neoforge.fluids.FluidStack;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.LinkedHashMap;
import java.util.Map;

public class SolidifierRecipeBuilder implements RecipeBuilder {

    protected String group;
    protected SizedIngredient mold;
    protected SizedIngredient output;
    protected FluidStack fluid;
    protected final Map<String, Criterion<?>> criteria = new LinkedHashMap<>();

    public SolidifierRecipeBuilder(SizedIngredient mold, SizedIngredient output, FluidStack fluid) {
        this.mold = mold;
        this.output = output;
        this.fluid = fluid;
    }

    public static SolidifierRecipeBuilder solidifierRecipesBuilder(SizedIngredient mold, SizedIngredient output, FluidStack fluid) {
        return new SolidifierRecipeBuilder(mold, output, fluid);
    }



    @Override
    public @NotNull RecipeBuilder unlockedBy(String name, Criterion<?> criterion) {
        this.criteria.put(name, criterion);
        return this;
    }

    @Override
    public @NotNull RecipeBuilder group(@Nullable String groupName) {
        this.group = groupName;
        return this;
    }

    @Override
    public Item getResult() {
        return ItemStack.EMPTY.getItem();
    }

    public void save(@NotNull RecipeOutput recipeOutput) {
        this.save(recipeOutput, ResourceLocation.fromNamespaceAndPath(Casting.MOD_ID, "solidifier/"));
    }

    @Override
    public void save(@NotNull RecipeOutput recipeOutput, @NotNull ResourceLocation id) {
        Advancement.Builder builder = Advancement.Builder.advancement()
                .addCriterion("has_the_recipe", RecipeUnlockedTrigger.unlocked(id))
                .rewards(AdvancementRewards.Builder.recipe(id))
                .requirements(AdvancementRequirements.Strategy.OR);
        this.criteria.forEach(builder::addCriterion);
        SolidifierRecipe solidifierRecipe = new SolidifierRecipe(this.mold, this.output, this.fluid);
        recipeOutput.accept(id, solidifierRecipe, builder.build(id.withPrefix("recipes/solidifier/")));

    }

}
