package com.benbenlaw.casting.item;

import com.benbenlaw.casting.util.CastingTags;
import net.minecraft.ChatFormatting;
import net.minecraft.client.gui.screens.Screen;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.network.chat.Component;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.TooltipFlag;
import net.neoforged.fml.ModList;

import java.util.List;
import java.util.Map;

import static com.benbenlaw.casting.util.ValidToolTypesForToolModifiers.*;

public class EquipmentModifierItem extends Item {

    private final String tooltip;
    private final int maxLevel;

    public EquipmentModifierItem(Properties properties, String tooltip, int maxLevel) {
        super(properties);
        this.tooltip = tooltip;
        this.maxLevel = maxLevel;
    }

    @Override
    public void appendHoverText(ItemStack stack, TooltipContext context, List<Component> components, TooltipFlag flag) {

        boolean requireShiftToDisableTooltip = stack.is(CastingTags.Items.CAN_BE_DISABLED_WITH_SHIFT);

        if (Screen.hasShiftDown()) {

            components.add(Component.translatable(tooltip, maxLevel).withStyle(ChatFormatting.YELLOW));

            if (requireShiftToDisableTooltip) {
                components.add(Component.translatable("tooltips.casting.information.shift_to_disable").withStyle(ChatFormatting.RED));
            }

            components.add(Component.translatable("tooltips.casting.information.valid_tool_types").withStyle(ChatFormatting.GOLD));

            //add list of valid tool types
            String effect = BuiltInRegistries.ITEM.getKey(this).getPath();

            List<String> validToolTypes = VALID_MODIFIERS.entrySet().stream()
                    .filter(entry -> {
                        String key = entry.getKey();
                        if (key.equals(ALL_MODIFIERS)) return false;
                        if (key.equals(PAXEL_MODIFIERS) && !ModList.get().isLoaded("mekanismtools")) return false;
                        return entry.getValue().contains(effect);
                    })
                    .map(Map.Entry::getKey)
                    .toList();

            for (String toolType : validToolTypes) {
                components.add(Component.translatable("tooltips.casting." + toolType).withStyle(ChatFormatting.BLUE));


            }




        }
        else if (stack.has(CastingDataComponents.FLUID_TYPE)) {
            components.add(Component.translatable("tooltips.bblcore.shift").withStyle(ChatFormatting.YELLOW));
        }
    }
}
