package com.benbenlaw.casting.item;

import com.benbenlaw.casting.Casting;
import com.benbenlaw.casting.config.ToolModifierConfig;
import net.minecraft.world.item.Item;
import net.neoforged.bus.api.IEventBus;
import net.neoforged.neoforge.registries.DeferredItem;
import net.neoforged.neoforge.registries.DeferredRegister;

public class ToolModifierItems {

    public static final DeferredRegister.Items ITEMS =
            DeferredRegister.createItems(Casting.MOD_ID);

    public static final DeferredItem<Item> SILK_TOUCH = ITEMS.register("silk_touch",
            () -> new ToolModifierItem(new Item.Properties(), "casting.tooltip.silk_touch", 1));
    public static final DeferredItem<Item> EFFICIENCY = ITEMS.register("efficiency",
            () -> new ToolModifierItem(new Item.Properties(), "casting.tooltip.efficiency", ToolModifierConfig.maxEfficiencyAmount.get()));
    public static final DeferredItem<Item> FORTUNE = ITEMS.register("fortune",
            () -> new ToolModifierItem(new Item.Properties(), "casting.tooltip.fortune", ToolModifierConfig.maxFortuneAmount.get()));
    public static final DeferredItem<Item> UNBREAKING = ITEMS.register("unbreaking",
            () -> new ToolModifierItem(new Item.Properties(), "casting.tooltip.unbreaking", ToolModifierConfig.maxUnbreakingAmount.get()));
    public static final DeferredItem<Item> REPAIRING = ITEMS.register("repairing",
            () -> new ToolModifierItem(new Item.Properties(), "casting.tooltip.repairing", ToolModifierConfig.maxRepairingAmount.get()));
    public static final DeferredItem<Item> TORCH_PLACING = ITEMS.register("torch_placing",
            () -> new ToolModifierItem(new Item.Properties(), "casting.tooltip.torch_placing", 1));
    public static final DeferredItem<Item> AUTO_SMELT = ITEMS.register("auto_smelt",
            () -> new ToolModifierItem(new Item.Properties(), "casting.tooltip.auto_smelt", 1));
    public static final DeferredItem<Item> LOOTING = ITEMS.register("looting",
            () -> new ToolModifierItem(new Item.Properties(), "casting.tooltip.looting", ToolModifierConfig.maxLootingAmount.get()));
    public static final DeferredItem<Item> SHARPNESS = ITEMS.register("sharpness",
            () -> new ToolModifierItem(new Item.Properties(), "casting.tooltip.sharpness", ToolModifierConfig.maxSharpnessAmount.get()));
    public static final DeferredItem<Item> BEHEADING = ITEMS.register("beheading",
            () -> new ToolModifierItem(new Item.Properties(), "casting.tooltip.beheading", 1));
    public static final DeferredItem<Item> LIFESTEAL = ITEMS.register("lifesteal",
            () -> new ToolModifierItem(new Item.Properties(), "casting.tooltip.lifesteal", ToolModifierConfig.maxLifestealAmount.get()));
    public static final DeferredItem<Item> KNOCKBACK = ITEMS.register("knockback",
            () -> new ToolModifierItem(new Item.Properties(), "casting.tooltip.knockback", ToolModifierConfig.maxKnockbackAmount.get()));
    public static final DeferredItem<Item> IGNITE = ITEMS.register("ignite",
            () -> new ToolModifierItem(new Item.Properties(), "casting.tooltip.ignite", 1));
    public static final DeferredItem<Item> EXCAVATION = ITEMS.register("excavation",
            () -> new ToolModifierItem(new Item.Properties(), "casting.tooltip.excavation", ToolModifierConfig.maxExcavationAmount.get()));



    public static void register(IEventBus eventBus) {
        ITEMS.register(eventBus);
    }

}
