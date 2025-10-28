package com.benbenlaw.casting.item;

import com.benbenlaw.casting.Casting;
import com.mojang.serialization.Codec;
import net.minecraft.core.component.DataComponentType;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.network.RegistryFriendlyByteBuf;
import net.minecraft.network.codec.ByteBufCodecs;
import net.minecraft.network.codec.StreamCodec;
import net.neoforged.neoforge.fluids.FluidStack;
import net.neoforged.neoforge.registries.DeferredHolder;
import net.neoforged.neoforge.registries.DeferredRegister;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;

public class CastingDataComponents {

    public static final DeferredRegister<DataComponentType<?>> COMPONENTS = DeferredRegister.create(BuiltInRegistries.DATA_COMPONENT_TYPE, Casting.MOD_ID);

    public static final DeferredHolder<DataComponentType<?>, DataComponentType<List<FluidStack>>> FLUIDS =
            COMPONENTS.register("fluids", () ->
                    DataComponentType.<List<FluidStack>>builder().persistent(FluidStack.OPTIONAL_CODEC.listOf()).networkSynchronized(FluidStack.OPTIONAL_STREAM_CODEC.apply(ByteBufCodecs.list())).build());



}
