package com.benbenlaw.casting.screen;

import com.benbenlaw.casting.Casting;
import com.benbenlaw.casting.screen.multiblock.*;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.world.inventory.MenuType;
import net.neoforged.neoforge.common.extensions.IMenuTypeExtension;
import net.neoforged.neoforge.registries.DeferredHolder;
import net.neoforged.neoforge.registries.DeferredRegister;

public class CastingMenuTypes {
    public static final DeferredRegister<MenuType<?>> MENUS =
            DeferredRegister.create(BuiltInRegistries.MENU, Casting.MOD_ID);

    public static final DeferredHolder<MenuType<?>, MenuType<MultiblockControllerMenu>> MULTIBLOCK_CONTROLLER_MENU;
    public static final DeferredHolder<MenuType<?>, MenuType<MultiblockSolidifierMenu>> MULTIBLOCK_SOLIDIFIER_MENU;
    public static final DeferredHolder<MenuType<?>, MenuType<MultiblockFuelTankMenu>> MULTIBLOCK_FUEL_TANK_MENU;
    public static final DeferredHolder<MenuType<?>, MenuType<MultiblockCoolantTankMenu>> MULTIBLOCK_COOLANT_TANK_MENU;
    public static final DeferredHolder<MenuType<?>, MenuType<MultiblockValveMenu>> MULTIBLOCK_VALVE_MENU;
    public static final DeferredHolder<MenuType<?>, MenuType<MultiblockMixerMenu>> MULTIBLOCK_MIXER_MENU;

    //OG Casting
    public static final DeferredHolder<MenuType<?>, MenuType<SmelterMenu>> SMELTER_MENU;
    public static final DeferredHolder<MenuType<?>, MenuType<SolidifierMenu>> SOLIDIFIER_MENU;
    public static final DeferredHolder<MenuType<?>, MenuType<MixerMenu>> MIXER_MENU;
    public static final DeferredHolder<MenuType<?>, MenuType<EquipmentModifierMenu>> EQUIPMENT_MODIFIER_MENU;

    static {
        MULTIBLOCK_CONTROLLER_MENU = MENUS.register("multiblock_controller_menu", () -> IMenuTypeExtension.create(MultiblockControllerMenu::new));
        MULTIBLOCK_SOLIDIFIER_MENU = MENUS.register("multiblock_solidifier_menu", () -> IMenuTypeExtension.create(MultiblockSolidifierMenu::new));
        MULTIBLOCK_FUEL_TANK_MENU = MENUS.register("multiblock_fuel_tank_menu", () -> IMenuTypeExtension.create(MultiblockFuelTankMenu::new));
        MULTIBLOCK_COOLANT_TANK_MENU = MENUS.register("multiblock_coolant_tank_menu", () -> IMenuTypeExtension.create(MultiblockCoolantTankMenu::new));
        MULTIBLOCK_VALVE_MENU = MENUS.register("multiblock_valve_menu", () -> IMenuTypeExtension.create(MultiblockValveMenu::new));
        MULTIBLOCK_MIXER_MENU = MENUS.register("multiblock_mixer_menu", () -> IMenuTypeExtension.create(MultiblockMixerMenu::new));

        //OG Casting
        SMELTER_MENU = MENUS.register("smelter_menu", () ->
                IMenuTypeExtension.create(SmelterMenu::new));

        SOLIDIFIER_MENU = MENUS.register("solidifier_menu", () ->
                IMenuTypeExtension.create(SolidifierMenu::new));

        MIXER_MENU = MENUS.register("mixer_menu", () ->
                IMenuTypeExtension.create(MixerMenu::new));

        EQUIPMENT_MODIFIER_MENU = MENUS.register("equipment_modifier_menu", () ->
                IMenuTypeExtension.create(EquipmentModifierMenu::new));
    }
}