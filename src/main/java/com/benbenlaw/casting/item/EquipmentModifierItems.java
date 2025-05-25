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
            () -> new EquipmentModifierItem(new Item.Properties(), "tooltips.casting.information.silk_touch", 1));
    public static final DeferredItem<Item> EFFICIENCY = ITEMS.register("efficiency",
            () -> new EquipmentModifierItem(new Item.Properties(), "tooltips.casting.information.efficiency", EquipmentModifierConfig.maxEfficiencyAmount.get()));
    public static final DeferredItem<Item> FORTUNE = ITEMS.register("fortune",
            () -> new EquipmentModifierItem(new Item.Properties(), "tooltips.casting.information.fortune", EquipmentModifierConfig.maxFortuneAmount.get()));
    public static final DeferredItem<Item> UNBREAKING = ITEMS.register("unbreaking",
            () -> new EquipmentModifierItem(new Item.Properties(), "tooltips.casting.information.unbreaking", EquipmentModifierConfig.maxUnbreakingAmount.get()));
    public static final DeferredItem<Item> REPAIRING = ITEMS.register("repairing",
            () -> new EquipmentModifierItem(new Item.Properties(), "tooltips.casting.information.repairing", EquipmentModifierConfig.maxRepairingAmount.get()));
    public static final DeferredItem<Item> TORCH_PLACING = ITEMS.register("torch_placing",
            () -> new EquipmentModifierItem(new Item.Properties(), "tooltips.casting.information.torch_placing", 1));
    public static final DeferredItem<Item> AUTO_SMELT = ITEMS.register("auto_smelt",
            () -> new EquipmentModifierItem(new Item.Properties(), "tooltips.casting.information.auto_smelt", 1));
    public static final DeferredItem<Item> LOOTING = ITEMS.register("looting",
            () -> new EquipmentModifierItem(new Item.Properties(), "tooltips.casting.information.looting", EquipmentModifierConfig.maxLootingAmount.get()));
    public static final DeferredItem<Item> SHARPNESS = ITEMS.register("sharpness",
            () -> new EquipmentModifierItem(new Item.Properties(), "tooltips.casting.information.sharpness", EquipmentModifierConfig.maxSharpnessAmount.get()));
    public static final DeferredItem<Item> BEHEADING = ITEMS.register("beheading",
            () -> new EquipmentModifierItem(new Item.Properties(), "tooltips.casting.information.beheading", 1));
    public static final DeferredItem<Item> LIFESTEAL = ITEMS.register("lifesteal",
            () -> new EquipmentModifierItem(new Item.Properties(), "tooltips.casting.information.lifesteal", EquipmentModifierConfig.maxLifestealAmount.get()));
    public static final DeferredItem<Item> KNOCKBACK = ITEMS.register("knockback",
            () -> new EquipmentModifierItem(new Item.Properties(), "tooltips.casting.information.knockback", EquipmentModifierConfig.maxKnockbackAmount.get()));
    public static final DeferredItem<Item> IGNITE = ITEMS.register("ignite",
            () -> new EquipmentModifierItem(new Item.Properties(), "tooltips.casting.information.ignite", 1));
    public static final DeferredItem<Item> EXCAVATION = ITEMS.register("excavation",
            () -> new EquipmentModifierItem(new Item.Properties(), "tooltips.casting.information.excavation", EquipmentModifierConfig.maxExcavationAmount.get()));
    public static final DeferredItem<Item> TELEPORTING = ITEMS.register("teleporting",
            () -> new EquipmentModifierItem(new Item.Properties(), "tooltips.casting.information.teleporting", EquipmentModifierConfig.maxTeleportationAmount.get()));
    public static final DeferredItem<Item> MAGNET = ITEMS.register("magnet",
            () -> new EquipmentModifierItem(new Item.Properties(), "tooltips.casting.information.magnet", EquipmentModifierConfig.maxMagnetAmount.get()));
    public static final DeferredItem<Item> PROTECTION = ITEMS.register("protection",
            () -> new EquipmentModifierItem(new Item.Properties(), "tooltips.casting.information.protection", EquipmentModifierConfig.maxProtectionAmount.get()));
    public static final DeferredItem<Item> STEP_ASSIST = ITEMS.register("step_assist",
            () -> new EquipmentModifierItem(new Item.Properties(), "tooltips.casting.information.step_assist", EquipmentModifierConfig.maxStepAssistAmount.get()));
    public static final DeferredItem<Item> NIGHT_VISION = ITEMS.register("night_vision",
            () -> new EquipmentModifierItem(new Item.Properties(), "tooltips.casting.information.night_vision", 1));
    public static final DeferredItem<Item> WATER_BREATHING = ITEMS.register("water_breathing",
            () -> new EquipmentModifierItem(new Item.Properties(), "tooltips.casting.information.water_breathing", 1));
    public static final DeferredItem<Item> SPEED = ITEMS.register("speed",
            () -> new EquipmentModifierItem(new Item.Properties(), "tooltips.casting.information.speed", EquipmentModifierConfig.maxSpeedAmount.get()));
    public static final DeferredItem<Item> WATER_WALKER = ITEMS.register("water_walker",
            () -> new EquipmentModifierItem(new Item.Properties(), "tooltips.casting.information.water_walker", 1));
    public static final DeferredItem<Item> LAVA_WALKER = ITEMS.register("lava_walker",
            () -> new EquipmentModifierItem(new Item.Properties(), "tooltips.casting.information.lava_walker", 1));
    public static final DeferredItem<Item> FLIGHT = ITEMS.register("flight",
            () -> new EquipmentModifierItem(new Item.Properties(), "tooltips.casting.information.flight", 1));


    public static void register(IEventBus eventBus) {
        ITEMS.register(eventBus);
    }

}
