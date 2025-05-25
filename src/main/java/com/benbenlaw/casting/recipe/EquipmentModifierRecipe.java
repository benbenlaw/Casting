package com.benbenlaw.casting.recipe;

import com.mojang.serialization.Codec;
import com.mojang.serialization.MapCodec;
import com.mojang.serialization.codecs.RecordCodecBuilder;
import net.minecraft.core.HolderLookup;
import net.minecraft.core.NonNullList;
import net.minecraft.network.RegistryFriendlyByteBuf;
import net.minecraft.network.codec.StreamCodec;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.crafting.*;
import net.minecraft.world.level.Level;
import net.neoforged.neoforge.common.crafting.SizedIngredient;
import net.neoforged.neoforge.fluids.FluidStack;
import org.jetbrains.annotations.NotNull;

public record EquipmentModifierRecipe(SizedIngredient upgradeItem, FluidStack upgradeFluid, String effect) implements Recipe<RecipeInput> {

    @Override
    public @NotNull NonNullList<Ingredient> getIngredients() {
        NonNullList<Ingredient> ingredients = NonNullList.createWithCapacity(1);
        ingredients.add(upgradeItem.ingredient());
        return ingredients;
    }

    @Override
    public boolean matches(@NotNull RecipeInput container, @NotNull Level level) {
        return true;
    }

    @Override
    public boolean canCraftInDimensions(int pWidth, int pHeight) {
        return true;
    }

    @Override
    public @NotNull ItemStack getResultItem(HolderLookup.Provider provider) {
        return ItemStack.EMPTY;
    }

    public boolean requiresBothItemAndFluid() {
        return !upgradeItem.ingredient().isEmpty() && !upgradeFluid.isEmpty();
    }

    @Override
    public @NotNull ItemStack assemble(@NotNull RecipeInput container, HolderLookup.@NotNull Provider provider) {
        return ItemStack.EMPTY;
    }

    public FluidStack getFluidStack() {
        return this.upgradeFluid.copy();
    }

    @Override
    public @NotNull RecipeSerializer<?> getSerializer() {
        return Serializer.INSTANCE;
    }

    @Override
    public @NotNull RecipeType<?> getType() {
        return Type.INSTANCE;
    }


    @Override
    public boolean isSpecial() {
        return true;
    }

    public static class Type implements RecipeType<EquipmentModifierRecipe> {
        private Type() {}
        public static final Type INSTANCE = new Type();
    }

    public static class Serializer implements RecipeSerializer<EquipmentModifierRecipe> {
        public static final Serializer INSTANCE = new Serializer();

        public final MapCodec<EquipmentModifierRecipe> CODEC = RecordCodecBuilder.mapCodec(instance ->
                instance.group(
                        SizedIngredient.FLAT_CODEC.optionalFieldOf("upgrade_item", SizedIngredient.of(ItemStack.EMPTY.getItem(), 1)).forGetter(EquipmentModifierRecipe::upgradeItem),
                        FluidStack.OPTIONAL_CODEC.optionalFieldOf("upgrade_fluid", FluidStack.EMPTY).forGetter(EquipmentModifierRecipe::upgradeFluid),
                        Codec.STRING.fieldOf("effect").forGetter(EquipmentModifierRecipe::effect)
                ).apply(instance, Serializer::createToolModifierRecipe)
        );

        private static final StreamCodec<RegistryFriendlyByteBuf, EquipmentModifierRecipe> STREAM_CODEC = StreamCodec.of(
                Serializer::write, Serializer::read);

        @Override
        public @NotNull MapCodec<EquipmentModifierRecipe> codec() {
            return CODEC;
        }

        @Override
        public @NotNull StreamCodec<RegistryFriendlyByteBuf, EquipmentModifierRecipe> streamCodec() {
            return STREAM_CODEC;
        }

        private static EquipmentModifierRecipe read(RegistryFriendlyByteBuf buffer) {
            SizedIngredient upgradeItem = SizedIngredient.STREAM_CODEC.decode(buffer);
            FluidStack upgradeFluid = FluidStack.OPTIONAL_STREAM_CODEC.decode(buffer);
            String effect = buffer.readUtf();
            return new EquipmentModifierRecipe(upgradeItem, upgradeFluid, effect);
        }

        private static void write(RegistryFriendlyByteBuf buffer, EquipmentModifierRecipe recipe) {
            SizedIngredient.STREAM_CODEC.encode(buffer, recipe.upgradeItem);
            FluidStack.OPTIONAL_STREAM_CODEC.encode(buffer, recipe.upgradeFluid);
            buffer.writeUtf(recipe.effect);
        }

        static EquipmentModifierRecipe createToolModifierRecipe(SizedIngredient upgradeItem, FluidStack upgradeFluid, String effect) {
            return new EquipmentModifierRecipe(upgradeItem, upgradeFluid, effect);
        }
    }
}
