package com.benbenlaw.casting.item;

import com.benbenlaw.casting.Casting;
import net.minecraft.world.item.Item;
import net.neoforged.neoforge.registries.DeferredItem;
import net.neoforged.neoforge.registries.DeferredRegister;

public class CastingItems {
    public static final DeferredRegister.Items ITEMS = DeferredRegister.createItems(Casting.MOD_ID);

    //Molds
    public static final DeferredItem<Item> INGOT_MOLD = ITEMS.register("ingot_mold",
            () -> new Item(new Item.Properties()));
    public static final DeferredItem<Item> NUGGET_MOLD = ITEMS.register("nugget_mold",
            () -> new Item(new Item.Properties()));
    public static final DeferredItem<Item> GEAR_MOLD = ITEMS.register("gear_mold",
            () -> new Item(new Item.Properties()));
    public static final DeferredItem<Item> PLATE_MOLD = ITEMS.register("plate_mold",
            () -> new Item(new Item.Properties()));
    public static final DeferredItem<Item> ROD_MOLD = ITEMS.register("rod_mold",
            () -> new Item(new Item.Properties()));
    public static final DeferredItem<Item> BLOCK_MOLD = ITEMS.register("block_mold",
            () -> new Item(new Item.Properties()));
    public static final DeferredItem<Item> GEM_MOLD = ITEMS.register("gem_mold",
            () -> new Item(new Item.Properties()));
    public static final DeferredItem<Item> DUST_MOLD = ITEMS.register("dust_mold",
            () -> new Item(new Item.Properties()));
    public static final DeferredItem<Item> BALL_MOLD = ITEMS.register("ball_mold",
            () -> new Item(new Item.Properties()));
    public static final DeferredItem<Item> WIRE_MOLD = ITEMS.register("wire_mold",
            () -> new Item(new Item.Properties()));
    public static final DeferredItem<Item> REPAIRING_MOLD = ITEMS.register("repairing_mold",
            () -> new Item(new Item.Properties()));

    //Misc
    public static final DeferredItem<Item> BLACK_BRICK = ITEMS.register("black_brick",
            () -> new Item(new Item.Properties()));

    public static final DeferredItem<Item> FLUID_MOVER = ITEMS.register("fluid_mover",
            () -> new FluidMoverItem(new Item.Properties().stacksTo(1)));


    //todo
    /*

    public static final DeferredItem<Item> REPAIRING_MOLD = ITEMS.register("repairing_mold",
            () -> new Item(new Item.Properties()));

    public static final DeferredItem<Item> BLACK_BRICK = ITEMS.register("black_brick",
            () -> new Item(new Item.Properties()));

    public static final DeferredItem<Item> FLUID_MOVER = ITEMS.register("fluid_mover",
            () -> new FluidMoverItem(new Item.Properties().stacksTo(1)));

     */


}

