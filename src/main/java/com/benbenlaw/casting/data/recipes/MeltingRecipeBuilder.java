package com.benbenlaw.casting.data.recipes;

import com.benbenlaw.casting.Casting;
import com.benbenlaw.casting.recipe.FuelRecipe;
import com.benbenlaw.casting.recipe.MeltingRecipe;
import net.minecraft.advancements.Advancement;
import net.minecraft.advancements.AdvancementRequirements;
import net.minecraft.advancements.AdvancementRewards;
import net.minecraft.advancements.Criterion;
import net.minecraft.advancements.critereon.RecipeUnlockedTrigger;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.core.registries.Registries;
import net.minecraft.data.recipes.RecipeBuilder;
import net.minecraft.data.recipes.RecipeOutput;
import net.minecraft.resources.ResourceKey;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.crafting.Recipe;
import net.neoforged.neoforge.common.crafting.SizedIngredient;
import net.neoforged.neoforge.fluids.FluidStack;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.LinkedHashMap;
import java.util.Map;

public class MeltingRecipeBuilder implements RecipeBuilder {

    protected String group;
    protected SizedIngredient input;
    protected FluidStack output;
    protected int meltingTemp;
    protected final Map<String, Criterion<?>> criteria = new LinkedHashMap<>();

    public MeltingRecipeBuilder(SizedIngredient input, FluidStack output, int meltingTemp) {
        this.input = input;
        this.output = output;
        this.meltingTemp = meltingTemp;
    }

    public static MeltingRecipeBuilder meltingRecipesBuilder(SizedIngredient input, FluidStack output, int meltingTemp) {
        return new MeltingRecipeBuilder(input, output, meltingTemp);
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



    @Override
    public void save(@NotNull RecipeOutput recipeOutput, @NotNull String id) {
        this.save(recipeOutput, ResourceKey.create(Registries.RECIPE, Casting.rl("melting/" + id)));
    }

    @Override
    public void save(RecipeOutput recipeOutput, ResourceKey<Recipe<?>> resourceKey) {
        Advancement.Builder builder = Advancement.Builder.advancement()
                .addCriterion("has_the_recipe", RecipeUnlockedTrigger.unlocked(resourceKey))
                .rewards(AdvancementRewards.Builder.recipe(resourceKey))
                .requirements(AdvancementRequirements.Strategy.OR);
        this.criteria.forEach(builder::addCriterion);
        MeltingRecipe meltingRecipe = new MeltingRecipe(this.input, this.output, this.meltingTemp);
        recipeOutput.accept(resourceKey, meltingRecipe, builder.build(resourceKey.location().withPrefix("recipes/melting/")));


    }

}
