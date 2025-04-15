package com.benbenlaw.casting.util;

import java.util.*;

public class ValidToolTypesForToolModifiers {

    //Effects
    public static final String EFFICIENCY = "efficiency";
    public static final String UNBREAKING = "unbreaking";
    public static final String FORTUNE = "fortune";
    public static final String REPAIRING = "repairing";
    public static final String SILK_TOUCH = "silk_touch";
    //Tool Types

    public static final String PICKAXE_MODIFIERS = "pickaxe_modifiers";
    public static final String AXE_MODIFIERS = "axe_modifiers";
    public static final String SHOVEL_MODIFIERS = "shovel_modifiers";
    public static final String HOE_MODIFIERS = "hoe_modifiers";
    public static final String SWORD_MODIFIERS = "sword_modifiers";
    public static final String PAXEL_MODIFIERS = "paxel_modifiers";
    public static final String SHEAR_MODIFIERS = "shear_modifiers";



    //Map
    public static final Map<String, List<String>> TOOL_TYPE_VALID_MODIFIERS;

    static {
        Map<String, List<String>> map = new HashMap<>();

        map.put(PICKAXE_MODIFIERS, List.of(EFFICIENCY, UNBREAKING, FORTUNE, REPAIRING, SILK_TOUCH));
        map.put(AXE_MODIFIERS, List.of(EFFICIENCY, UNBREAKING, FORTUNE, REPAIRING, SILK_TOUCH));
        map.put(SHOVEL_MODIFIERS, List.of(EFFICIENCY, UNBREAKING, FORTUNE, REPAIRING, SILK_TOUCH));
        map.put(PAXEL_MODIFIERS, List.of(EFFICIENCY, UNBREAKING, FORTUNE, REPAIRING, SILK_TOUCH));
        map.put(SHEAR_MODIFIERS, List.of(UNBREAKING, REPAIRING));

        map.put(HOE_MODIFIERS, List.of(EFFICIENCY, UNBREAKING, FORTUNE, REPAIRING, SILK_TOUCH));
        map.put(SWORD_MODIFIERS, List.of(FORTUNE, REPAIRING));


        TOOL_TYPE_VALID_MODIFIERS = Collections.unmodifiableMap(map);



     }








}
