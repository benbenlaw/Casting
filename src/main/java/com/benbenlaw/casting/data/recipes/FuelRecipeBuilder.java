package com.benbenlaw.casting.data.recipes;

import com.benbenlaw.casting.Casting;
import com.benbenlaw.casting.recipe.EquipmentModifierRecipe;
import com.benbenlaw.casting.recipe.FuelRecipe;
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
import net.neoforged.neoforge.fluids.FluidStack;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.LinkedHashMap;
import java.util.Map;

public class FuelRecipeBuilder implements RecipeBuilder {

    protected String group;
    protected FluidStack fluid;
    protected int temp;
    protected int duration;
    protected final Map<String, Criterion<?>> criteria = new LinkedHashMap<>();

    public FuelRecipeBuilder(FluidStack fluid, int temp, int duration) {
        this.fluid = fluid;
        this.temp = temp;
        this.duration = duration;
    }

    public static FuelRecipeBuilder fuelRecipesBuilder(FluidStack fluid, int temp, int duration) {
        return new FuelRecipeBuilder(fluid, temp, duration);
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
        this.save(recipeOutput, ResourceKey.create(Registries.RECIPE, Casting.rl("fuel/" + id)));
    }

    @Override
    public void save(RecipeOutput recipeOutput, ResourceKey<Recipe<?>> resourceKey) {
        Advancement.Builder builder = Advancement.Builder.advancement()
                .addCriterion("has_the_recipe", RecipeUnlockedTrigger.unlocked(resourceKey))
                .rewards(AdvancementRewards.Builder.recipe(resourceKey))
                .requirements(AdvancementRequirements.Strategy.OR);
        this.criteria.forEach(builder::addCriterion);
        FuelRecipe fuelRecipe = new FuelRecipe(this.fluid, this.temp, this.duration);
        recipeOutput.accept(resourceKey, fuelRecipe, builder.build(resourceKey.location().withPrefix("recipes/fuel/")));

    }
}
