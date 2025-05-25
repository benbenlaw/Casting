package com.benbenlaw.casting.config;

import com.benbenlaw.casting.util.BeheadingHeadMap;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.item.Item;
import net.neoforged.neoforge.common.ModConfigSpec;

import java.util.List;

public class BeheadingConfig {

    public static final ModConfigSpec.Builder BUILDER = new ModConfigSpec.Builder();
    public static final ModConfigSpec SPEC;

    public static ModConfigSpec.ConfigValue<List<? extends String>> HEAD_MAPPINGS;

    static {
        BUILDER.comment("Beheading Config for Casting")
                .push("Beheading Config");

        HEAD_MAPPINGS = BUILDER
                .comment("Mappings in the form: entity_id=item_id")
                .defineListAllowEmpty(
                        List.of("heads"),
                        () -> List.of(
                                "minecraft:zombie=minecraft:zombie_head",
                                "minecraft:skeleton=minecraft:skeleton_skull",
                                "minecraft:creeper=minecraft:creeper_head",
                                "minecraft:wither_skeleton=minecraft:wither_skeleton_skull",
                                "minecraft:player=minecraft:player_head",
                                "minecraft:enderman=enderio:enderman_head"
                        ),
                        obj -> obj instanceof String && ((String) obj).contains("=")
                );

        BUILDER.pop();
        SPEC = BUILDER.build();
    }

    public static void applyToHeadMap() {
        for (String entry : HEAD_MAPPINGS.get()) {
            String[] parts = entry.split("=");
            if (parts.length == 2) {
                ResourceLocation entityId = ResourceLocation.parse(parts[0].trim());
                ResourceLocation itemId = ResourceLocation.parse(parts[1].trim());

                Item item = BuiltInRegistries.ITEM.get(itemId);
                //Add the item to the head map
                BeheadingHeadMap.register(entityId, () -> item);
            } else {
                System.err.println("[BeheadingConfig] Invalid entry: " + entry);
            }
        }
    }
}