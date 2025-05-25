package com.benbenlaw.casting.data.recipes;

import com.benbenlaw.casting.Casting;
import com.benbenlaw.casting.recipe.MeltingRecipe;
import net.minecraft.advancements.Advancement;
import net.minecraft.advancements.AdvancementRequirements;
import net.minecraft.advancements.AdvancementRewards;
import net.minecraft.advancements.Criterion;
import net.minecraft.advancements.critereon.RecipeUnlockedTrigger;
import net.minecraft.core.registries.BuiltInRegistries;
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

    public void save(@NotNull RecipeOutput recipeOutput) {
        this.save(recipeOutput, ResourceLocation.fromNamespaceAndPath(Casting.MOD_ID, "melting/" +
                BuiltInRegistries.FLUID.getKey(this.output.getFluid()).getPath()) + "/");
    }

    @Override
    public void save(@NotNull RecipeOutput recipeOutput, @NotNull ResourceLocation id) {
        Advancement.Builder builder = Advancement.Builder.advancement()
                .addCriterion("has_the_recipe", RecipeUnlockedTrigger.unlocked(id))
                .rewards(AdvancementRewards.Builder.recipe(id))
                .requirements(AdvancementRequirements.Strategy.OR);
        this.criteria.forEach(builder::addCriterion);
        MeltingRecipe meltingRecipe = new MeltingRecipe(this.input, this.output, this.meltingTemp);
        recipeOutput.accept(id, meltingRecipe, builder.build(id.withPrefix("recipes/melting/")));

    }

}
