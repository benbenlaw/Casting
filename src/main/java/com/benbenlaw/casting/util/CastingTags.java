package com.benbenlaw.casting.util;

import com.benbenlaw.casting.Casting;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.tags.BlockTags;
import net.minecraft.tags.ItemTags;
import net.minecraft.tags.TagKey;
import net.minecraft.world.item.Item;
import net.minecraft.world.level.block.Block;

public class CastingTags {

    public static class Blocks {

        //Smelting Tags
       // public static final TagKey<Block> BANNED_IN_BLOCK_PLACER = tag("banned_in_block_placer");


        //Common Tags
     //   public static final TagKey<Block> ENDER_ORE = commonTags("ores/ender_ore");

        private static TagKey<Block> tag(String name) {
            return BlockTags.create(ResourceLocation.fromNamespaceAndPath(Casting.MOD_ID, name));
        }

        private static TagKey<Block> commonTags(String name) {
            return BlockTags.create(ResourceLocation.fromNamespaceAndPath("c", name));
        }

    }

    public static class Items {

        //Smelting Item Tags
        public static final TagKey<Item> MOLDS = commonTags("molds");
        public static final TagKey<Item> INGOT_MOLD = commonTags("molds/ingot");
        public static final TagKey<Item> NUGGET_MOLD = commonTags("molds/nugget");
        public static final TagKey<Item> GEM_MOLD = commonTags("molds/gem");
        public static final TagKey<Item> DUST_MOLD = commonTags("molds/dust");
        public static final TagKey<Item> PLATE_MOLD = commonTags("molds/plate");
        public static final TagKey<Item> GEAR_MOLD = commonTags("molds/gear");
        public static final TagKey<Item> ROD_MOLD = commonTags("molds/rod");
        //public static final TagKey<Item> WIRE_MOLD = commonTags("molds/wire");
        public static final TagKey<Item> BLOCK_MOLD = commonTags("molds/block");
        public static final TagKey<Item> BALL_MOLD = commonTags("molds/ball");
        public static final TagKey<Item> CAN_BE_DISABLED_WITH_SHIFT = tag("can_be_disabled_with_shift");



        //Common Tags
     //   public static final TagKey<Item> ENDER_ORE = commonTags("ores/ender_ore");


        private static TagKey<Item> tag(String name) {
            return ItemTags.create(ResourceLocation.fromNamespaceAndPath(Casting.MOD_ID, name));
        }

        private static TagKey<Item> commonTags(String name) {
            return ItemTags.create(ResourceLocation.fromNamespaceAndPath("c", name));
        }

    }
}
