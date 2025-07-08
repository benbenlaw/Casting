package com.benbenlaw.casting.util;

import com.benbenlaw.casting.Casting;
import com.mojang.blaze3d.platform.InputConstants;
import net.minecraft.client.KeyMapping;
import net.neoforged.bus.api.SubscribeEvent;
import net.neoforged.fml.common.EventBusSubscriber;
import net.neoforged.neoforge.client.event.RegisterKeyMappingsEvent;
import net.neoforged.neoforge.client.settings.KeyConflictContext;

public class KeyBinds {

    public static final String KEY_CATEGORY = "key.casting.category";

    public static final String HELMET_KEY = "key.casting.helmet_hotkey";
    public static final String CHESTPLATE_KEY = "key.casting.chestplate_hotkey";
    public static final String LEGGINGS_KEY = "key.casting.leggings_hotkey";
    public static final String BOOTS_KEY = "key.casting.boots_hotkey";

    public static final KeyMapping HELMET_HOTKEY = new KeyMapping(HELMET_KEY, KeyConflictContext.UNIVERSAL, InputConstants.Type.KEYSYM, InputConstants.UNKNOWN.getValue(), KEY_CATEGORY);
    public static final KeyMapping CHESTPLATE_HOTKEY = new KeyMapping(CHESTPLATE_KEY, KeyConflictContext.UNIVERSAL, InputConstants.Type.KEYSYM, InputConstants.UNKNOWN.getValue(), KEY_CATEGORY);
    public static final KeyMapping LEGGINGS_HOTKEY = new KeyMapping(LEGGINGS_KEY, KeyConflictContext.UNIVERSAL, InputConstants.Type.KEYSYM, InputConstants.UNKNOWN.getValue(), KEY_CATEGORY);
    public static final KeyMapping BOOTS_HOTKEY = new KeyMapping(BOOTS_KEY, KeyConflictContext.UNIVERSAL, InputConstants.Type.KEYSYM, InputConstants.UNKNOWN.getValue(), KEY_CATEGORY);



}
