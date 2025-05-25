package com.benbenlaw.casting.recipe;

import com.benbenlaw.core.recipe.NoInventoryRecipe;
import com.mojang.serialization.Codec;
import com.mojang.serialization.DataResult;
import com.mojang.serialization.MapCodec;
import com.mojang.serialization.codecs.RecordCodecBuilder;
import net.minecraft.core.HolderLookup;
import net.minecraft.core.NonNullList;
import net.minecraft.network.RegistryFriendlyByteBuf;
import net.minecraft.network.codec.StreamCodec;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.crafting.Ingredient;
import net.minecraft.world.item.crafting.Recipe;
import net.minecraft.world.item.crafting.RecipeSerializer;
import net.minecraft.world.item.crafting.RecipeType;
import net.minecraft.world.level.Level;
import net.neoforged.neoforge.fluids.FluidStack;
import org.jetbrains.annotations.NotNull;

import java.util.List;

public record MixingRecipe(NonNullList<FluidStack> fluids,
                           FluidStack outputFluid) implements Recipe<NoInventoryRecipe> {

    @Override
    public @NotNull NonNullList<Ingredient> getIngredients() {
        return NonNullList.create();
    }
    @Override
    public boolean matches(NoInventoryRecipe recipe, Level level) {

            return true;

    }
    @Override
    public ItemStack assemble(NoInventoryRecipe p_345149_, HolderLookup.Provider p_346030_) {
        return ItemStack.EMPTY;
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
    public List<FluidStack> getAllFluids() {
        List<FluidStack> allFluids = fluids;
        allFluids.removeIf(FluidStack::isEmpty);
        return allFluids;
    }

    public static class Type implements RecipeType<MixingRecipe> {
        private Type() {}
        public static final Type INSTANCE = new Type();
    }

    public static class Serializer implements RecipeSerializer<MixingRecipe> {
        public static final Serializer INSTANCE = new Serializer();

        public final MapCodec<MixingRecipe> CODEC = RecordCodecBuilder.mapCodec(instance ->
                instance.group(

                        Codec.list(FluidStack.CODEC).fieldOf("inputs").flatXmap(inputFluids -> {
                            /* Maybe enable this in the future if an issue with too many fluids for an alloy comes up
                            if (inputFluids.size() > 8) {
                                    return DataResult.error(
                                            () -> "Too many results for cloche recipe! The maximum quantity of unique results is "
                                                    + 12);
                            }

                            */

                            NonNullList<FluidStack> nonNullList = NonNullList.create();
                            nonNullList.addAll(inputFluids);
                            return DataResult.success(nonNullList);
                        }, DataResult::success).forGetter(MixingRecipe::fluids),
                        FluidStack.CODEC.fieldOf("output").forGetter(MixingRecipe::outputFluid)
                ).apply(instance, Serializer::createSolidifierRecipe)
        );

        private static final StreamCodec<RegistryFriendlyByteBuf, MixingRecipe> STREAM_CODEC = StreamCodec.of(
                Serializer::write, Serializer::read);

        @Override
        public @NotNull MapCodec<MixingRecipe> codec() {
            return CODEC;
        }

        @Override
        public @NotNull StreamCodec<RegistryFriendlyByteBuf, MixingRecipe> streamCodec() {
            return STREAM_CODEC;
        }

        private static MixingRecipe read(RegistryFriendlyByteBuf buffer) {
            int fluidCount = buffer.readInt();
            NonNullList<FluidStack> fluids = NonNullList.create();
            for (int i = 0; i < fluidCount; i++) {
                FluidStack fluid = FluidStack.OPTIONAL_STREAM_CODEC.decode(buffer);
                if (!fluid.isEmpty()) {
                    fluids.add(fluid);
                }
            }

            FluidStack output = FluidStack.STREAM_CODEC.decode(buffer);
            return new MixingRecipe(fluids, output);
        }

        private static void write(RegistryFriendlyByteBuf buffer, MixingRecipe recipe) {
            buffer.writeInt(recipe.fluids.size());
            for (FluidStack fluid : recipe.fluids) {
                FluidStack.OPTIONAL_STREAM_CODEC.encode(buffer, fluid);
            }

            FluidStack.STREAM_CODEC.encode(buffer, recipe.outputFluid);
        }

        static MixingRecipe createSolidifierRecipe(NonNullList<FluidStack> fluids, FluidStack output) {
            return new MixingRecipe(fluids, output);
        }
    }
}
