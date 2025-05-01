package com.benbenlaw.casting.util;

import com.benbenlaw.casting.item.CastingDataComponents;
import net.minecraft.core.component.DataComponentType;

import java.util.*;

public class ValidToolTypesForToolModifiers {

    //Effects
    public static final String EQUIPMENT_LEVEL = "equipment_level";
    public static final String EQUIPMENT_EXPERIENCE = "equipment_experience";
    public static final String EFFICIENCY = "efficiency";
    public static final String UNBREAKING = "unbreaking";
    public static final String FORTUNE = "fortune";
    public static final String REPAIRING = "repairing";
    public static final String SILK_TOUCH = "silk_touch";
    public static final String TORCH_PLACING = "torch_placing";
    public static final String AUTO_SMELT = "auto_smelt";
    public static final String LOOTING = "looting";
    public static final String SHARPNESS = "sharpness";
    public static final String BEHEADING = "beheading";
    public static final String LIFESTEAL = "lifestyle";
    public static final String KNOCKBACK = "knockback";
    public static final String IGNITE = "ignite";
    public static final String EXCAVATION = "excavation";
    public static final String TELEPORTING = "teleporting";
    public static final String MAGNET = "magnet";
    public static final String PROTECTION = "protection";
    public static final String STEP_ASSIST = "step_assist";


    //Tool Types
    public static final String ALL_MODIFIERS = "all_modifiers";
    public static final String PICKAXE_MODIFIERS = "pickaxe_modifiers";
    public static final String AXE_MODIFIERS = "axe_modifiers";
    public static final String SHOVEL_MODIFIERS = "shovel_modifiers";
    public static final String HOE_MODIFIERS = "hoe_modifiers";
    public static final String SWORD_MODIFIERS = "sword_modifiers";
    public static final String PAXEL_MODIFIERS = "paxel_modifiers";
    public static final String SHEAR_MODIFIERS = "shear_modifiers";
    public static final String HELMET_MODIFIERS = "helmet_modifiers";
    public static final String CHESTPLATE_MODIFIERS = "chestplate_modifiers";
    public static final String LEGGINGS_MODIFIERS = "leggings_modifiers";
    public static final String BOOTS_MODIFIERS = "boots_modifiers";
    public static final String BODY_MODIFIERS = "body_modifiers";



    //Map
    public static final Map<String, List<String>> VALID_MODIFIERS;

    static {
        Map<String, List<String>> map = new HashMap<>();

        //Add all modifiers to this
        map.put(ALL_MODIFIERS, List.of(STEP_ASSIST, PROTECTION, TELEPORTING, EXCAVATION, IGNITE, EFFICIENCY, UNBREAKING, FORTUNE, REPAIRING, SILK_TOUCH, TORCH_PLACING, AUTO_SMELT, LOOTING, SHARPNESS, BEHEADING, LIFESTEAL, KNOCKBACK));

        //Add specific modifiers to this
        map.put(PICKAXE_MODIFIERS, List.of(TELEPORTING, EXCAVATION, EFFICIENCY, UNBREAKING, FORTUNE, REPAIRING, SILK_TOUCH, TORCH_PLACING, AUTO_SMELT));
        map.put(AXE_MODIFIERS, List.of(TELEPORTING, EXCAVATION, EFFICIENCY, UNBREAKING, FORTUNE, REPAIRING, SILK_TOUCH, LOOTING));
        map.put(SHOVEL_MODIFIERS, List.of(TELEPORTING, EXCAVATION, EFFICIENCY, UNBREAKING, FORTUNE, REPAIRING, SILK_TOUCH));
        map.put(PAXEL_MODIFIERS, List.of(TELEPORTING, EXCAVATION, EFFICIENCY, UNBREAKING, FORTUNE, REPAIRING, SILK_TOUCH, TORCH_PLACING, AUTO_SMELT));
        map.put(SHEAR_MODIFIERS, List.of(EXCAVATION, UNBREAKING, REPAIRING));
        map.put(HOE_MODIFIERS, List.of(EXCAVATION, EFFICIENCY, UNBREAKING, FORTUNE, REPAIRING, SILK_TOUCH));
        map.put(SWORD_MODIFIERS, List.of(TELEPORTING, UNBREAKING, LOOTING, SHARPNESS, REPAIRING, BEHEADING, LIFESTEAL, KNOCKBACK, LIFESTEAL, IGNITE));

        //Armor
        map.put(HELMET_MODIFIERS, List.of(MAGNET, REPAIRING, UNBREAKING));
        map.put(CHESTPLATE_MODIFIERS, List.of(MAGNET, REPAIRING, UNBREAKING));
        map.put(LEGGINGS_MODIFIERS, List.of(MAGNET, REPAIRING, UNBREAKING));
        map.put(BOOTS_MODIFIERS, List.of(STEP_ASSIST, MAGNET, REPAIRING, UNBREAKING));

        //Is body animals I think it is so will leave this empty
        map.put(BODY_MODIFIERS, List.of());

        VALID_MODIFIERS = Collections.unmodifiableMap(map);


    }

    public static DataComponentType<?> getDataComponentFromString(String effect) {

        if (Objects.equals(effect, FORTUNE)) {
            return CastingDataComponents.FORTUNE.get();
        }
        if (Objects.equals(effect, EFFICIENCY)) {
            return CastingDataComponents.EFFICIENCY.get();
        }
        if (Objects.equals(effect, UNBREAKING)) {
            return CastingDataComponents.UNBREAKING.get();
        }
        if (Objects.equals(effect, REPAIRING)) {
            return CastingDataComponents.REPAIRING.get();
        }
        if (Objects.equals(effect, SILK_TOUCH)) {
            return CastingDataComponents.SILK_TOUCH.get();
        }
        if (Objects.equals(effect, TORCH_PLACING)) {
            return CastingDataComponents.TORCH_PLACING.get();
        }
        if (Objects.equals(effect, AUTO_SMELT)) {
            return CastingDataComponents.AUTO_SMELT.get();
        }
        if (Objects.equals(effect, LOOTING)) {
            return CastingDataComponents.LOOTING.get();
        }
        if (Objects.equals(effect, SHARPNESS)) {
            return CastingDataComponents.SHARPNESS.get();
        }
        if (Objects.equals(effect, BEHEADING)) {
            return CastingDataComponents.BEHEADING.get();
        }
        if (Objects.equals(effect, LIFESTEAL)) {
            return CastingDataComponents.LIFESTEAL.get();
        }
        if (Objects.equals(effect, KNOCKBACK)) {
            return CastingDataComponents.KNOCKBACK.get();
        }
        if (Objects.equals(effect, IGNITE)) {
            return CastingDataComponents.IGNITE.get();
        }
        if (Objects.equals(effect, EXCAVATION)) {
            return CastingDataComponents.EXCAVATION.get();
        }
        if (Objects.equals(effect, TELEPORTING)) {
            return CastingDataComponents.TELEPORTING.get();
        }
        if (Objects.equals(effect, MAGNET)) {
            return CastingDataComponents.MAGNET.get();
        }
        if (Objects.equals(effect, PROTECTION)) {
            return CastingDataComponents.PROTECTION.get();
        }
        if (Objects.equals(effect, STEP_ASSIST)) {
            return CastingDataComponents.STEP_ASSIST.get();
        }
        return null;
    }








}
