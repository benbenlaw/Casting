package com.benbenlaw.casting.util;

import com.benbenlaw.core.tag.CommonTags;

public enum MaterialMelting {

    STORAGE_BLOCKS(CommonTags.ResourceType.STORAGE_BLOCKS, 810),
    INGOTS(CommonTags.ResourceType.INGOTS, 90),
    NUGGETS(CommonTags.ResourceType.NUGGETS, 10),
    RAW_STORAGE_BLOCKS(CommonTags.ResourceType.RAW_STORAGE_BLOCKS, 810),
    RAW_MATERIALS(CommonTags.ResourceType.RAW_MATERIALS, 90),
    ORES(CommonTags.ResourceType.ORES, 90),
    PLATES(CommonTags.ResourceType.PLATES, 90),
    DUSTS(CommonTags.ResourceType.DUSTS, 90),
    GEARS(CommonTags.ResourceType.GEARS, 360),
    RODS(CommonTags.ResourceType.RODS, 45),
    GEMS(CommonTags.ResourceType.GEMS, 90),
    WIRES(CommonTags.ResourceType.WIRES, 45);


    private final CommonTags.ResourceType type;
    private final int amount;

    MaterialMelting(CommonTags.ResourceType type, int amount) {
        this.type = type;
        this.amount = amount;
    }

    public CommonTags.ResourceType getType() {
        return type;
    }

    public int getAmount() {
        return amount;
    }


}
