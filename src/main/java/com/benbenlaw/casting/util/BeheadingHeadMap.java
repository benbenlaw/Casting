package com.benbenlaw.casting.util;

import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.function.Supplier;

public class BeheadingHeadMap {

    private static final Map<ResourceLocation, Supplier<Item>> ENTITY_HEADS = new HashMap<>();

    public static Optional<ItemStack> getHeadForEntity(Entity entity) {
        ResourceLocation id = BuiltInRegistries.ENTITY_TYPE.getKey(entity.getType());
        if (ENTITY_HEADS.containsKey(id)) {
            return Optional.of(new ItemStack(ENTITY_HEADS.get(id).get()));
        }
        return Optional.empty();
    }

    public static void register(ResourceLocation entityId, Supplier<Item> headItemSupplier) {
        ENTITY_HEADS.put(entityId, headItemSupplier);
    }
}