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

public record MeltingRecipe(SizedIngredient input, FluidStack output, int meltingTemp) implements Recipe<RecipeInput> {

    @Override
    public @NotNull NonNullList<Ingredient> getIngredients() {
        NonNullList<Ingredient> ingredients = NonNullList.createWithCapacity(1);
        ingredients.add(input.ingredient());
        return ingredients;
    }

    @Override
    public boolean matches(@NotNull RecipeInput container, @NotNull Level level) {
        for (int i = 0; i < 15; i++) {
            if (input.test(container.getItem(i))) {
                return true;
            }
        }
        return false;
    }

    @Override
    public boolean canCraftInDimensions(int pWidth, int pHeight) {
        return true;
    }

    @Override
    public @NotNull ItemStack getResultItem(HolderLookup.Provider provider) {
        return ItemStack.EMPTY;
    }

    @Override
    public @NotNull ItemStack assemble(@NotNull RecipeInput container, HolderLookup.@NotNull Provider provider) {
        return ItemStack.EMPTY;
    }

    public int getMeltingTemp() {
        return this.meltingTemp;
    }

    public int getIngredientStackCount() {
        return this.input.count();
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

    public static class Type implements RecipeType<MeltingRecipe> {
        private Type() {}
        public static final Type INSTANCE = new Type();
    }

    public static class Serializer implements RecipeSerializer<MeltingRecipe> {
        public static final Serializer INSTANCE = new Serializer();

        public final MapCodec<MeltingRecipe> CODEC = RecordCodecBuilder.mapCodec(instance ->
                instance.group(
                        SizedIngredient.FLAT_CODEC.fieldOf("input").forGetter(MeltingRecipe::input),
                        FluidStack.CODEC.fieldOf("output").forGetter(MeltingRecipe::output),
                        Codec.INT.fieldOf("meltingTemp").forGetter(MeltingRecipe::meltingTemp)
                ).apply(instance, Serializer::createMeltingRecipe)
        );

        private static final StreamCodec<RegistryFriendlyByteBuf, MeltingRecipe> STREAM_CODEC = StreamCodec.of(
                Serializer::write, Serializer::read);

        @Override
        public @NotNull MapCodec<MeltingRecipe> codec() {
            return CODEC;
        }

        @Override
        public @NotNull StreamCodec<RegistryFriendlyByteBuf, MeltingRecipe> streamCodec() {
            return STREAM_CODEC;
        }

        private static MeltingRecipe read(RegistryFriendlyByteBuf buffer) {
            SizedIngredient input = SizedIngredient.STREAM_CODEC.decode(buffer);
            FluidStack output = FluidStack.STREAM_CODEC.decode(buffer);
            int meltingTemp = buffer.readInt();
            return new MeltingRecipe(input, output, meltingTemp );
        }

        private static void write(RegistryFriendlyByteBuf buffer, MeltingRecipe recipe) {
            SizedIngredient.STREAM_CODEC.encode(buffer, recipe.input);
            FluidStack.STREAM_CODEC.encode(buffer, recipe.output);
            buffer.writeInt(recipe.meltingTemp);
        }

        static MeltingRecipe createMeltingRecipe(SizedIngredient input, FluidStack output, int duration) {
            return new MeltingRecipe(input, output, duration);
        }
    }
}
