package com.benbenlaw.casting.item;

import com.benbenlaw.casting.Casting;
import com.benbenlaw.casting.config.EquipmentModifierConfig;
import net.minecraft.world.item.Item;
import net.neoforged.bus.api.IEventBus;
import net.neoforged.neoforge.registries.DeferredItem;
import net.neoforged.neoforge.registries.DeferredRegister;

public class EquipmentModifierItems {

    public static final DeferredRegister.Items ITEMS =
            DeferredRegister.createItems(Casting.MOD_ID);

    public static final DeferredItem<Item> SILK_TOUCH = ITEMS.register("silk_touch",
            () -> new EquipmentModifierItem(new Item.Properties(), "casting.tooltip.silk_touch", 1));
    public static final DeferredItem<Item> EFFICIENCY = ITEMS.register("efficiency",
            () -> new EquipmentModifierItem(new Item.Properties(), "casting.tooltip.efficiency", EquipmentModifierConfig.maxEfficiencyAmount.get()));
    public static final DeferredItem<Item> FORTUNE = ITEMS.register("fortune",
            () -> new EquipmentModifierItem(new Item.Properties(), "casting.tooltip.fortune", EquipmentModifierConfig.maxFortuneAmount.get()));
    public static final DeferredItem<Item> UNBREAKING = ITEMS.register("unbreaking",
            () -> new EquipmentModifierItem(new Item.Properties(), "casting.tooltip.unbreaking", EquipmentModifierConfig.maxUnbreakingAmount.get()));
    public static final DeferredItem<Item> REPAIRING = ITEMS.register("repairing",
            () -> new EquipmentModifierItem(new Item.Properties(), "casting.tooltip.repairing", EquipmentModifierConfig.maxRepairingAmount.get()));
    public static final DeferredItem<Item> TORCH_PLACING = ITEMS.register("torch_placing",
            () -> new EquipmentModifierItem(new Item.Properties(), "casting.tooltip.torch_placing", 1));
    public static final DeferredItem<Item> AUTO_SMELT = ITEMS.register("auto_smelt",
            () -> new EquipmentModifierItem(new Item.Properties(), "casting.tooltip.auto_smelt", 1));
    public static final DeferredItem<Item> LOOTING = ITEMS.register("looting",
            () -> new EquipmentModifierItem(new Item.Properties(), "casting.tooltip.looting", EquipmentModifierConfig.maxLootingAmount.get()));
    public static final DeferredItem<Item> SHARPNESS = ITEMS.register("sharpness",
            () -> new EquipmentModifierItem(new Item.Properties(), "casting.tooltip.sharpness", EquipmentModifierConfig.maxSharpnessAmount.get()));
    public static final DeferredItem<Item> BEHEADING = ITEMS.register("beheading",
            () -> new EquipmentModifierItem(new Item.Properties(), "casting.tooltip.beheading", 1));
    public static final DeferredItem<Item> LIFESTEAL = ITEMS.register("lifesteal",
            () -> new EquipmentModifierItem(new Item.Properties(), "casting.tooltip.lifesteal", EquipmentModifierConfig.maxLifestealAmount.get()));
    public static final DeferredItem<Item> KNOCKBACK = ITEMS.register("knockback",
            () -> new EquipmentModifierItem(new Item.Properties(), "casting.tooltip.knockback", EquipmentModifierConfig.maxKnockbackAmount.get()));
    public static final DeferredItem<Item> IGNITE = ITEMS.register("ignite",
            () -> new EquipmentModifierItem(new Item.Properties(), "casting.tooltip.ignite", 1));
    public static final DeferredItem<Item> EXCAVATION = ITEMS.register("excavation",
            () -> new EquipmentModifierItem(new Item.Properties(), "casting.tooltip.excavation", EquipmentModifierConfig.maxExcavationAmount.get()));
    public static final DeferredItem<Item> TELEPORTING = ITEMS.register("teleporting",
            () -> new EquipmentModifierItem(new Item.Properties(), "casting.tooltip.teleporting", EquipmentModifierConfig.maxTeleportationAmount.get()));
    public static final DeferredItem<Item> MAGNET = ITEMS.register("magnet",
            () -> new EquipmentModifierItem(new Item.Properties(), "casting.tooltip.magnet", EquipmentModifierConfig.maxMagnetAmount.get()));
    public static final DeferredItem<Item> PROTECTION = ITEMS.register("protection",
            () -> new EquipmentModifierItem(new Item.Properties(), "casting.tooltip.protection", EquipmentModifierConfig.maxProtectionAmount.get()));
    public static final DeferredItem<Item> STEP_ASSIST = ITEMS.register("step_assist",
            () -> new EquipmentModifierItem(new Item.Properties(), "casting.tooltip.step_assist", EquipmentModifierConfig.maxStepAssistAmount.get()));


    public static void register(IEventBus eventBus) {
        ITEMS.register(eventBus);
    }

}
