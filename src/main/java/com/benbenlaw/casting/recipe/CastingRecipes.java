package com.benbenlaw.casting.recipe;

import com.benbenlaw.casting.Casting;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.world.item.crafting.RecipeSerializer;
import net.minecraft.world.item.crafting.RecipeType;
import net.neoforged.neoforge.registries.DeferredRegister;

import java.util.function.Supplier;

public class CastingRecipes {

    public static final DeferredRegister<RecipeSerializer<?>> SERIALIZER =
            DeferredRegister.create(BuiltInRegistries.RECIPE_SERIALIZER, Casting.MOD_ID);
    public static final DeferredRegister<RecipeType<?>> TYPES =
            DeferredRegister.create(BuiltInRegistries.RECIPE_TYPE, Casting.MOD_ID);


    //Melting
    public static final Supplier<RecipeSerializer<MeltingRecipe>> MELTING_SERIALIZER =
            SERIALIZER.register("melting", () -> MeltingRecipe.Serializer.INSTANCE);
    public static final Supplier<RecipeType<MeltingRecipe>> MELTING_TYPE =
            TYPES.register("melting", () -> MeltingRecipe.Type.INSTANCE);

    //Fuel
    public static final Supplier<RecipeSerializer<FuelRecipe>> FUEL_SERIALIZER =
            SERIALIZER.register("fuel", () -> FuelRecipe.Serializer.INSTANCE);
    public static final Supplier<RecipeType<FuelRecipe>> FUEL_TYPE =
            TYPES.register("fuel", () -> FuelRecipe.Type.INSTANCE);

    //Coolant
    public static final Supplier<RecipeSerializer<CoolantRecipe>> COOLANT_SERIALIZER =
            SERIALIZER.register("coolant", () -> CoolantRecipe.Serializer.INSTANCE);
    public static final Supplier<RecipeType<CoolantRecipe>> COOLANT_TYPE =
            TYPES.register("coolant", () -> CoolantRecipe.Type.INSTANCE);

    //Solidifier
    public static final Supplier<RecipeSerializer<SolidifierRecipe>> SOLIDIFIER_SERIALIZER =
            SERIALIZER.register("solidifier", () -> SolidifierRecipe.Serializer.INSTANCE);
    public static final Supplier<RecipeType<SolidifierRecipe>> SOLIDIFIER_TYPE =
            TYPES.register("solidifier", () -> SolidifierRecipe.Type.INSTANCE);

    //Mixer
    public static final Supplier<RecipeSerializer<MixingRecipe>> MIXING_SERIALIZER =
            SERIALIZER.register("mixing", () -> MixingRecipe.Serializer.INSTANCE);
    public static final Supplier<RecipeType<MixingRecipe>> MIXING_TYPE =
            TYPES.register("mixing", () -> MixingRecipe.Type.INSTANCE);

    //Equipment Modifier
    public static final Supplier<RecipeSerializer<EquipmentModifierRecipe>> EQUIPMENT_MODIFIER_SERIALIZER =
            SERIALIZER.register("equipment_modifier", () -> EquipmentModifierRecipe.Serializer.INSTANCE);

    public static final Supplier<RecipeType<EquipmentModifierRecipe>> EQUIPMENT_MODIFIER_TYPE =
            TYPES.register("equipment_modifier", () -> EquipmentModifierRecipe.Type.INSTANCE);
}
