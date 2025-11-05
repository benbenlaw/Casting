package com.benbenlaw.casting.data.recipes;

import com.benbenlaw.casting.Casting;
import com.benbenlaw.casting.recipe.CoolantRecipe;
import com.benbenlaw.casting.recipe.EquipmentModifierRecipe;
import net.minecraft.advancements.Advancement;
import net.minecraft.advancements.AdvancementRequirements;
import net.minecraft.advancements.AdvancementRewards;
import net.minecraft.advancements.Criterion;
import net.minecraft.advancements.critereon.RecipeUnlockedTrigger;
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

public class EquipmentModifierRecipeBuilder implements RecipeBuilder {

    protected String group;
    protected SizedIngredient upgradeItem;
    protected FluidStack upgradeFluid;
    protected String effect;
    protected final Map<String, Criterion<?>> criteria = new LinkedHashMap<>();

    public EquipmentModifierRecipeBuilder(SizedIngredient upgradeItem, FluidStack upgradeFluid, String effect) {
        this.upgradeItem = upgradeItem!= null ? upgradeItem : SizedIngredient.of(ItemStack.EMPTY.getItem(), 1);
        this.upgradeFluid = upgradeFluid != null ? upgradeFluid : FluidStack.EMPTY;
        this.effect = effect;
    }

    public static EquipmentModifierRecipeBuilder ToolModifierRecipesBuilder(SizedIngredient upgradeItem, FluidStack upgradeFluid, String effect) {
        return new EquipmentModifierRecipeBuilder(upgradeItem, upgradeFluid, effect);
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
        this.save(recipeOutput, ResourceKey.create(Registries.RECIPE, Casting.rl("equipment_modifiers/" + id)));
    }

    @Override
    public void save(RecipeOutput recipeOutput, ResourceKey<Recipe<?>> resourceKey) {
        Advancement.Builder builder = Advancement.Builder.advancement()
                .addCriterion("has_the_recipe", RecipeUnlockedTrigger.unlocked(resourceKey))
                .rewards(AdvancementRewards.Builder.recipe(resourceKey))
                .requirements(AdvancementRequirements.Strategy.OR);
        this.criteria.forEach(builder::addCriterion);
        EquipmentModifierRecipe solidifierRecipe = new EquipmentModifierRecipe(this.upgradeItem, this.upgradeFluid, this.effect);
        recipeOutput.accept(resourceKey, solidifierRecipe, builder.build(resourceKey.location().withPrefix("recipes/equipment_modifiers/")));
    }
}
