package com.benbenlaw.casting.item;

import com.benbenlaw.casting.Casting;
import com.benbenlaw.casting.item.util.FluidListComponent;
import net.minecraft.core.component.DataComponentType;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.network.codec.ByteBufCodecs;
import net.minecraft.world.item.ItemStack;
import net.neoforged.neoforge.registries.DeferredHolder;
import net.neoforged.neoforge.registries.DeferredRegister;

import java.util.List;

public class CastingDataComponents {

    public static final DeferredRegister<DataComponentType<?>> COMPONENTS = DeferredRegister.create(BuiltInRegistries.DATA_COMPONENT_TYPE, Casting.MOD_ID);

    public static final DeferredHolder<DataComponentType<?>, DataComponentType<FluidListComponent>> FLUIDS =
            COMPONENTS.register("fluids", () ->
                    DataComponentType.<FluidListComponent>builder()
                            .persistent(FluidListComponent.CODEC)
                            .networkSynchronized(FluidListComponent.STREAM_CODEC)
                            .cacheEncoding()
                            .build());

    public static final DeferredHolder<DataComponentType<?>, DataComponentType<List<ItemStack>>> STORED_MOLDS =
            COMPONENTS.register("stored_molds", () ->
                    DataComponentType.<List<ItemStack>>builder()
                            .persistent(ItemStack.CODEC.listOf())
                            .networkSynchronized(ItemStack.STREAM_CODEC.apply(ByteBufCodecs.list()))
                            .cacheEncoding()
                            .build());

}
