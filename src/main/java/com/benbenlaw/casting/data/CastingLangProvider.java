package com.benbenlaw.casting.data;

import com.benbenlaw.casting.Casting;
import com.benbenlaw.casting.item.EquipmentModifierItems;
import net.minecraft.data.PackOutput;
import net.neoforged.neoforge.common.data.ExistingFileHelper;
import net.neoforged.neoforge.common.data.LanguageProvider;

import static com.benbenlaw.casting.fluid.CastingFluids.FLUIDS_MAP;

public class CastingLangProvider extends LanguageProvider {

    public CastingLangProvider(PackOutput output, ExistingFileHelper existingFileHelper) {
        super(output, Casting.MOD_ID, "en_us");
    }

    @Override
    protected void addTranslations() {

        //Creative Tab
        add("itemGroup.casting", "Casting");

        //Misc Items
        addItemTranslation("black_brick", "Black Brick");
        addItemTranslation("fluid_mover", "Fluid Mover");

        //Blocks
        addBlockTranslation("multiblock_controller", "Multiblock Controller (BETA)");
        addBlockTranslation("multiblock_fuel_tank", "Multiblock Fuel Tank (BETA)");
        addBlockTranslation("multiblock_coolant_tank", "Multiblock Coolant Tank (BETA)");
        addBlockTranslation("multiblock_solidifier", "Multiblock Solidifier (BETA)");
        addBlockTranslation("black_bricks", "Black Bricks");
        addBlockTranslation("black_brick_glass", "Black Brick Glass (BETA)");
        addBlockTranslation("multiblock_valve", "Multiblock Valve (BETA)");
        addBlockTranslation("multiblock_mixer", "Multiblock Mixer  (BETA)");

        //OG Casting
        addBlockTranslation("controller", "Simple Controller");
        addBlockTranslation("solidifier", "Simple Solidifier");
        addBlockTranslation("mixer", "Simple Mixer");
        addBlockTranslation("equipment_modifier", "Simple Equipment Modifier");
        addBlockTranslation("tank", "Simple Tank");
        addBlockTranslation("mixer_whisk", "Simple Mixer Whisk");

        //Multiblock - Controller
        addChatTranslation("multiblock_controller.no_valid_block", "No valid blocks found, must be exactly 2");
        addChatTranslation("multiblock_controller.only_one_valid_block", "Only one valid block found, must be exactly 2");
        addChatTranslation("multiblock_controller.too_many_valid_blocks", "Too many valid blocks found, must be exactly 2");

        //Gui Translations
        addGUITranslation("multiblock_controller.no_fuel_tank", "No Fuel Tank Found!");
        addGUITranslation("multiblock_controller.no_controller", "No Controller Found!");

        addGUITranslation("buttons.no_off", "On/Off Button");
        addGUITranslation("buttons.fluid", "Filter: Empty, Click to on fluid to filter");
        addGUITranslation("buttons.remove_fluid", "Filter: %s, Click to remove");
        addGUITranslation("buttons.no_alloy", "Filter: Empty, Click to cycle");
        addGUITranslation("buttons.cycle_valid_alloy", "Filter: %s, Click to cycle, Found %s Alloys");
        addGUITranslation("buttons.not_hot_enough", "Current fuel not hot enough for at least one item");
        addGUITranslation("buttons.no_fuel", "No fuel found in fuel tank");
        addGUITranslation("buttons.not_enough_space", "Not enough space in main tank");
        addGUITranslation("buttons.waiting_for_recipe", "Ready to work!");

        //Jei Translations
        addGUITranslation("jei.melting", "Melting");
        addGUITranslation("jei.melting_temp", "%s Min Temp");
        addGUITranslation("jei.mixing", "Mixing");
        addGUITranslation("jei.solidifier", "Solidifier");
        addGUITranslation("jei.fuel", "Fuels");
        addGUITranslation("jei.fuel_temp", "%s Fuel Temp");
        addGUITranslation("jei.fuel_speed", "%s ticks per recipe");
        addGUITranslation("jei.coolant", "Coolants");
        addGUITranslation("jei.coolant_speed", "%s ticks per recipe")
        ;
        addGUITranslation("jei.information.solidifier",   "The Solidifier is used to turn your molten resource into useful item! \n\nThis can use an optional Fuel Tank adjacent to it, cooler fluids (below 1000) in the tank will make the Solidifier faster\n\nFluid can be locked to prevent another fluids from going in, click the lock button next to the fluid tank.\n\nHolding Shift whilst above the tank and left clicking will remove the fluid from the tank!\n\nStores fluids when broken!");
        addGUITranslation("jei.information.mixer", "The Mixer can be used to mix molten fluids together! \n\nHolding Shift whilst above a certain tank and left clicking will remove the fluid from the that tank!\n\nStores fluids when broken!");
        addGUITranslation("jei.information.controller", "The Controller can be used to melt items into there molten variants! \n\nThis needs a Fuel Tank to work, the Fuel Tank must contain a Fuel that is hot enough to melt the item, Hotter fluids melt faster\n\nHolding Shift whilst above a certain tank and left clicking will remove the fluid from the that tank!\n\nStores fluids when broken!");
        addGUITranslation("jei.information.tank", "The Tank stores the fuel used in the Controller to melt items!\n\nCan also be used with the Solidifier colder fluids in the tank makes the Solidifier faster \n\nStores fluids when broken!");
        addGUITranslation("jei.information.equipment_modifier", "Used to apply modifiers to your equipment!, Your first modifier will add Level 1 to your equipment additional modifiers require an additional level. Use your tools to Level Up and get more modifier slots.\n\nUsing a Repairing Mold will allow you to repair Equipment using its molten variants! \n\nSearch Equipment Modifiers in JEI to view the modifiers!\n\nPlace the right items and or fluids in the Equipment Modifier then add a valid piece of Equipment to add that effect!\n\nHolding Shift whilst above the tank and left clicking will remove the fluid from the tank!\n\nStores fluids when broken!");
        addGUITranslation("jei.information.fluid_mover", "Used to move fluids between tanks, Holds up to 8000mb of a single fluid. \n\nClick on a tank to collect fluid and click on another tank to add!\n\nWill try to fill the selected tank before collecting fluids!");
        addGUITranslation("jei.information.mixer_whisk", "Used to speed up the mixer when placed above it. \n\nFor each whisk above the mixer the tick rate is decreased by 40 ticks!\n\nAdditional whisks placed after the tick rate is at its lowest (20) will have no effect!");

        //Molds
        addItemTranslation("block_mold", "Block Mold");
        addItemTranslation("gear_mold", "Gear Mold");
        addItemTranslation("ingot_mold", "Ingot Mold");
        addItemTranslation("nugget_mold", "Nugget Mold");
        addItemTranslation("plate_mold", "Plate Mold");
        addItemTranslation("rod_mold", "Rod Mold");
        addItemTranslation("gem_mold", "Gem Mold");
        addItemTranslation("dust_mold", "Dust Mold");
        addItemTranslation("ball_mold", "Ball Mold");
        addItemTranslation("wire_mold", "Wire Mold");
        addItemTranslation("repairing_mold", "Repairing Mold");

        //Equipment Modifiers
        addItemTranslation("auto_smelt", "Auto Smelt");
        addItemTranslation("beheading", "Beheading");
        addItemTranslation("efficiency", "Efficiency");
        addItemTranslation("excavation", "Excavation");
        addItemTranslation("fortune", "Fortune");
        addItemTranslation("ignite", "Ignite");
        addItemTranslation("knockback", "Knockback");
        addItemTranslation("lifesteal", "Lifesteal");
        addItemTranslation("looting", "Looting");
        addItemTranslation("magnet", "Magnet");
        addItemTranslation("protection", "Protection");
        addItemTranslation("repairing", "Repairing");
        addItemTranslation("sharpness", "Sharpness");
        addItemTranslation("silk_touch", "Silk Touch");
        addItemTranslation("step_assist", "Step Assist");
        addItemTranslation("teleporting", "Teleporting");
        addItemTranslation("torch_placing", "Torch Placing");
        addItemTranslation("unbreaking", "Unbreaking");
        addItemTranslation("water_walker", "Water Walker");
        addItemTranslation("lava_walker", "Lava Walker");
        addItemTranslation("speed", "Speed");
        addItemTranslation("water_breathing", "Water Breathing");
        addItemTranslation("night_vision", "Night Vision");
        addItemTranslation("flight", "Flight");

        //Tooltips
        addTooltipTranslation("information.repairing_mold", "When used in the Equipment Modifier, this will allow you to repair your equipment using its molten variant!");
        addTooltipTranslation("information.valid_tool_types", "Valid Equipment;");
        addTooltipTranslation("information.shift_to_disable", "- Hold SHIFT to disable");

        addTooltipTranslation("information.modifiers", "Modifiers - %s/%s");
        addTooltipTranslation("information.equipment_level", "Equipment Level %s XP: %s/%s");
        addTooltipTranslation("information.auto_smelt", "Allows the tool to automatically smelt items it mines up to level %s");
        addTooltipTranslation("information.beheading", "Gives a tool Beheading, drops mob heads");
        addTooltipTranslation("information.efficiency", "Gives a tool Efficiency up to level %s");
        addTooltipTranslation("information.excavation", "Gives a tool Excavation, allows the tool to mine a bigger area up to level %s");
        addTooltipTranslation("information.fortune", "Gives a tool Fortune up to level %s");
        addTooltipTranslation("information.ignite", "Gives a tool Ignite, ignites enemies on hit up to level %s");
        addTooltipTranslation("information.knockback", "Gives a tool Knockback up to level %s");
        addTooltipTranslation("information.lifesteal", "Gives a tool Lifesteal, restores health when damaging enemies up to level %s");
        addTooltipTranslation("information.looting", "Gives a tool Looting up to level %s");
        addTooltipTranslation("information.magnet", "Gives armor Magnet, attracts items to the player up to level %s");
        addTooltipTranslation("information.protection", "Gives armor Protection up to level %s");
        addTooltipTranslation("information.repairing", "Gives a tool Repairing up to level %s");
        addTooltipTranslation("information.sharpness", "Gives a tool Sharpness up to level %s");
        addTooltipTranslation("information.silk_touch", "Gives a tool Silk Touch up to level %s");
        addTooltipTranslation("information.step_assist", "Gives armor Step Assist, allows the player to step up blocks up to level %s");
        addTooltipTranslation("information.teleporting", "Gives a tool Teleporting, allows the tool to teleport you to far away blocks away up to level %s. Costs 5 durability per teleport.");
        addTooltipTranslation("information.torch_placing", "Allows the tool to place torches on right click up to level %s");
        addTooltipTranslation("information.unbreaking", "Gives a tool Unbreaking up to level %s");
        addTooltipTranslation("information.water_walker", "Gives armor Water Walker, allows the player to walk on water up to level %s");
        addTooltipTranslation("information.lava_walker", "Gives armor Lava Walker, allows the player to walk on lava up to level %s");
        addTooltipTranslation("information.speed", "Gives armor Speed, allows the player to move faster up to level %s");
        addTooltipTranslation("information.water_breathing", "Gives armor Water Breathing, allows the player to breathe underwater");
        addTooltipTranslation("information.night_vision", "Gives armor Night Vision, allows the player to see in the dark");
        addTooltipTranslation("information.flight", "Gives armor Flight, allows the player to fly");

        addTooltipTranslation("stats.modifiers", "Modifiers - %s/%s");
        addTooltipTranslation("stats.auto_smelt", "Auto Smelt - %s");
        addTooltipTranslation("stats.beheading", "Beheading - %s%% Chance");
        addTooltipTranslation("stats.efficiency", "Efficiency - %s");
        addTooltipTranslation("stats.excavation", "Excavation - %s (%sx%s Area)");
        addTooltipTranslation("stats.fortune", "Fortune - %s");
        addTooltipTranslation("stats.ignite", "Ignite - %s%% Chance");
        addTooltipTranslation("stats.knockback", "Knockback - %s");
        addTooltipTranslation("stats.lifesteal", "Lifesteal - %s%% Damage Restored");
        addTooltipTranslation("stats.looting", "Looting - %s");
        addTooltipTranslation("stats.magnet", "Magnet - %s");
        addTooltipTranslation("stats.protection", "Protection - %s");
        addTooltipTranslation("stats.repairing", "Repairing - %s");
        addTooltipTranslation("stats.sharpness", "Sharpness - %s (+%s Attack Damage)");
        addTooltipTranslation("stats.silk_touch", "Silk Touch");
        addTooltipTranslation("stats.step_assist", "Step Assist - %s");
        addTooltipTranslation("stats.teleporting", "Teleporting - %s (%s Blocks)");
        addTooltipTranslation("stats.torch_placing", "Torch Placing");
        addTooltipTranslation("stats.unbreaking", "Unbreaking - %s");
        addTooltipTranslation("stats.water_walker", "Water Walker");
        addTooltipTranslation("stats.lava_walker", "Lava Walker");
        addTooltipTranslation("stats.speed", "Speed - %s");
        addTooltipTranslation("stats.water_breathing", "Water Breathing");
        addTooltipTranslation("stats.night_vision", "Night Vision");
        addTooltipTranslation("stats.flight", "Flight");

        addTooltipTranslation("all_modifiers", "- All");
        addTooltipTranslation("pickaxe_modifiers", "- Pickaxes");
        addTooltipTranslation("shovel_modifiers", "- Shovels");
        addTooltipTranslation("axe_modifiers", "- Axes");
        addTooltipTranslation("hoe_modifiers", "- Hoes");
        addTooltipTranslation("sword_modifiers", "- Swords");
        addTooltipTranslation("shear_modifiers", "- Shears");
        addTooltipTranslation("paxel_modifiers", "- Paxels");
        addTooltipTranslation("chestplate_modifiers", "- Chestplates");
        addTooltipTranslation("helmet_modifiers", "- Helmets");
        addTooltipTranslation("leggings_modifiers", "- Leggings");
        addTooltipTranslation("boots_modifiers", "- Boots");





        //Fluid Files
        for (var entry : FLUIDS_MAP.entrySet()) {
            String fluidName = entry.getKey();
            String fluidDisplayName = capitalizeFirstLetterOfEachWord(fluidName);

            addBlockTranslation(fluidName, fluidDisplayName);
            addItemTranslation(fluidName + "_bucket", fluidDisplayName + " Bucket");
        }



    }

    private void addItemTranslation(String name, String translation) {
        add("item." + Casting.MOD_ID + "." + name, translation);
    }
    private void addBlockTranslation(String name, String translation) {
        add("block." + Casting.MOD_ID + "." + name, translation);
    }

    private void addChatTranslation(String name, String translation) {
        add("chat." + Casting.MOD_ID + "." + name, translation);
    }
    private void addGUITranslation(String name, String translation) {
        add("gui." + Casting.MOD_ID + "." + name, translation);
    }

    private void addTooltipTranslation(String name, String translation) {
        add("tooltips." + Casting.MOD_ID + "." + name, translation);
    }


    public static String capitalizeFirstLetterOfEachWord(String input) {
        String[] words = input.split("_");
        StringBuilder result = new StringBuilder();
        for (String word : words) {
            if (!word.isEmpty()) {
                result.append(word.substring(0, 1).toUpperCase()) // Capitalize first letter
                        .append(word.substring(1).toLowerCase()) // Keep the rest lowercase
                        .append(" ");
            }
        }
        return result.toString().trim(); // Remove trailing space
    }

}
