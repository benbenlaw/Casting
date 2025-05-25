package com.benbenlaw.casting.recipe;

import com.mojang.serialization.Codec;
import com.mojang.serialization.MapCodec;
import com.mojang.serialization.codecs.RecordCodecBuilder;
import net.minecraft.core.HolderLookup;
import net.minecraft.network.RegistryFriendlyByteBuf;
import net.minecraft.network.codec.StreamCodec;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.crafting.Recipe;
import net.minecraft.world.item.crafting.RecipeInput;
import net.minecraft.world.item.crafting.RecipeSerializer;
import net.minecraft.world.item.crafting.RecipeType;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.material.Fluid;
import net.neoforged.neoforge.fluids.FluidStack;
import org.jetbrains.annotations.NotNull;

public record CoolantRecipe(FluidStack fluid, int duration) implements Recipe<RecipeInput> {

    //FluidStack contains the amount of fluid this is how much is used in recipes,
    //duration is the time it takes to cool the item in a solidifier when using this coolant

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

    @Override
    public @NotNull ItemStack assemble(@NotNull RecipeInput container, HolderLookup.@NotNull Provider provider) {
        return ItemStack.EMPTY;
    }

    public FluidStack getFluidStack() {
        return this.fluid.copy();
    }

    public Fluid getFluid() {
        return this.fluid.getFluid();
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

    public static class Type implements RecipeType<CoolantRecipe> {
        private Type() {}
        public static final Type INSTANCE = new Type();
    }

    public static class Serializer implements RecipeSerializer<CoolantRecipe> {
        public static final Serializer INSTANCE = new Serializer();

        public final MapCodec<CoolantRecipe> CODEC = RecordCodecBuilder.mapCodec(instance ->
                instance.group(
                        FluidStack.CODEC.fieldOf("fluid").forGetter(CoolantRecipe::fluid),
                        Codec.INT.fieldOf("duration").forGetter(CoolantRecipe::duration)
                ).apply(instance, Serializer::createCoolantRecipe)
        );

        private static final StreamCodec<RegistryFriendlyByteBuf, CoolantRecipe> STREAM_CODEC = StreamCodec.of(
                Serializer::write, Serializer::read);

        @Override
        public @NotNull MapCodec<CoolantRecipe> codec() {
            return CODEC;
        }

        @Override
        public @NotNull StreamCodec<RegistryFriendlyByteBuf, CoolantRecipe> streamCodec() {
            return STREAM_CODEC;
        }

        private static CoolantRecipe read(RegistryFriendlyByteBuf buffer) {
            FluidStack fluid = FluidStack.STREAM_CODEC.decode(buffer);
            int duration = buffer.readInt();
            return new CoolantRecipe(fluid, duration);
        }

        private static void write(RegistryFriendlyByteBuf buffer, CoolantRecipe recipe) {
            FluidStack.STREAM_CODEC.encode(buffer, recipe.fluid);
            buffer.writeInt(recipe.duration);
        }

        static CoolantRecipe createCoolantRecipe(FluidStack fluid, int duration) {
            return new CoolantRecipe(fluid, duration);
        }
    }
}
