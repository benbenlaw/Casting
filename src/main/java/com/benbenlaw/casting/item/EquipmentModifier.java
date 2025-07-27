package com.benbenlaw.casting.item;

import com.benbenlaw.casting.Casting;
import com.benbenlaw.casting.config.EquipmentModifierConfig;
import com.mojang.serialization.Codec;
import net.minecraft.core.component.DataComponentType;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.network.codec.ByteBufCodecs;
import net.minecraft.world.item.Item;
import net.neoforged.neoforge.registries.DeferredHolder;
import net.neoforged.neoforge.registries.DeferredRegister;

import java.util.function.Supplier;


public enum EquipmentModifier {

    //Add Modifiers here, they will be registered as items, data component, tooltip, string and max levels
    SILK_TOUCH("silk_touch", Codec.BOOL, ByteBufCodecs.BOOL, () -> 1),
    EFFICIENCY("efficiency", Codec.INT, ByteBufCodecs.INT, EquipmentModifierConfig.maxEfficiencyAmount),
    FORTUNE("fortune", Codec.INT, ByteBufCodecs.INT, EquipmentModifierConfig.maxFortuneAmount),
    UNBREAKING("unbreaking", Codec.INT, ByteBufCodecs.INT, EquipmentModifierConfig.maxUnbreakingAmount),
    REPAIRING("repairing", Codec.INT, ByteBufCodecs.INT, EquipmentModifierConfig.maxRepairingAmount),
    TORCH_PLACING("torch_placing", Codec.BOOL, ByteBufCodecs.BOOL, () -> 1),
    AUTO_SMELT("auto_smelt", Codec.BOOL, ByteBufCodecs.BOOL, () -> 1),
    LOOTING("looting", Codec.INT, ByteBufCodecs.INT, EquipmentModifierConfig.maxLootingAmount),
    SHARPNESS("sharpness", Codec.INT, ByteBufCodecs.INT, EquipmentModifierConfig.maxSharpnessAmount),
    BEHEADING("beheading", Codec.BOOL, ByteBufCodecs.BOOL, () -> 1),
    LIFESTEAL("lifesteal", Codec.INT, ByteBufCodecs.INT, EquipmentModifierConfig.maxLifestealAmount),
    KNOCKBACK("knockback", Codec.INT, ByteBufCodecs.INT, EquipmentModifierConfig.maxKnockbackAmount),
    IGNITE("ignite", Codec.INT, ByteBufCodecs.INT, EquipmentModifierConfig.maxIgniteAmount),
    EXCAVATION("excavation", Codec.INT, ByteBufCodecs.INT, EquipmentModifierConfig.maxExcavationAmount),
    TELEPORTING("teleporting", Codec.INT, ByteBufCodecs.INT, EquipmentModifierConfig.maxTeleportationAmount),
    MAGNET("magnet", Codec.INT, ByteBufCodecs.INT, EquipmentModifierConfig.maxMagnetAmount),
    PROTECTION("protection", Codec.INT, ByteBufCodecs.INT, EquipmentModifierConfig.maxProtectionAmount),
    STEP_ASSIST("step_assist", Codec.INT, ByteBufCodecs.INT, EquipmentModifierConfig.maxStepAssistAmount),
    NIGHT_VISION("night_vision", Codec.BOOL, ByteBufCodecs.BOOL, () -> 1),
    WATER_BREATHING("water_breathing", Codec.BOOL, ByteBufCodecs.BOOL, () -> 1),
    SPEED("speed", Codec.INT, ByteBufCodecs.INT, EquipmentModifierConfig.maxSpeedAmount),
    WATER_WALKER("water_walker", Codec.BOOL, ByteBufCodecs.BOOL, () -> 1),
    LAVA_WALKER("lava_walker", Codec.BOOL, ByteBufCodecs.BOOL, () -> 1),
    FLIGHT("flight", Codec.BOOL, ByteBufCodecs.BOOL, () -> 1),
    FEATHER_FALLING("feather_falling", Codec.INT, ByteBufCodecs.INT, EquipmentModifierConfig.maxFeatherFallingAmount),
    SOULBOUND("soulbound", Codec.BOOL, ByteBufCodecs.BOOL, () -> 1),
    JETS("jets", Codec.INT, ByteBufCodecs.INT, EquipmentModifierConfig.maxJetsAmount),


    EQUIPMENT_LEVEL("equipment_level", Codec.INT, ByteBufCodecs.INT, EquipmentModifierConfig.maxEquipmentLevel);

    public final String id;
    public final Codec<?> codec;
    public final Object networkCodec;
    public final Supplier<Integer> maxLevel;
    public final String tooltipKey;

    public static final DeferredRegister<Item> ITEMS = DeferredRegister.create(BuiltInRegistries.ITEM, Casting.MOD_ID);
    public DeferredHolder<Item, EquipmentModifierItem> item;

    public static final DeferredRegister<DataComponentType<?>> COMPONENTS = DeferredRegister.create(BuiltInRegistries.DATA_COMPONENT_TYPE, Casting.MOD_ID);
    public DeferredHolder<DataComponentType<?>, ?> dataComponent;

    EquipmentModifier(String id, Codec<?> codec, Object networkCodec, Supplier<Integer> maxLevel) {
        this.id = id;
        this.codec = codec;
        this.networkCodec = networkCodec;
        this.maxLevel = maxLevel;
        this.tooltipKey = "tooltips.casting.information." + id;

    }


    public static void registerAllItemModifiers() {
        for (EquipmentModifier modifier : values()) {
            modifier.item = ITEMS.register(modifier.id,
                    () -> new EquipmentModifierItem(new Item.Properties(), modifier.tooltipKey, modifier.maxLevel.get()));
        }
    }

    public static void registerAllDataComponents() {
        for (EquipmentModifier modifier : values()) {
            if (modifier.codec == Codec.BOOL || modifier.networkCodec == ByteBufCodecs.BOOL) {
                modifier.dataComponent = COMPONENTS.register(modifier.id, () ->
                        DataComponentType.<Boolean>builder().persistent(Codec.BOOL).networkSynchronized(ByteBufCodecs.BOOL).build());

            } else if (modifier.codec == Codec.INT || modifier.networkCodec == ByteBufCodecs.INT) {
                modifier.dataComponent = COMPONENTS.register(modifier.id, () ->
                        DataComponentType.<Integer>builder().persistent(Codec.INT).networkSynchronized(ByteBufCodecs.INT).build());

            } else {
                throw new IllegalArgumentException("Unsupported codec type for modifier: " + modifier.id);
            }
        }
    }

    public static final DeferredHolder<DataComponentType<?>, DataComponentType<Integer>> EQUIPMENT_EXPERIENCE =
            COMPONENTS.register("equipment_experience", () ->
                    DataComponentType.<Integer>builder().persistent(Codec.INT).networkSynchronized(ByteBufCodecs.INT).build());

    public static final DeferredHolder<DataComponentType<?>, DataComponentType<Boolean>> TOGGLEABLE_MODIFIERS =
            COMPONENTS.register("toggleable_modifiers", () ->
                    DataComponentType.<Boolean>builder().persistent(Codec.BOOL).networkSynchronized(ByteBufCodecs.BOOL).build());
}
