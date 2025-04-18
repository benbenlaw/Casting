package com.benbenlaw.casting.item;

import com.benbenlaw.core.item.TooltipUtil;
import net.minecraft.ChatFormatting;
import net.minecraft.client.gui.screens.Screen;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.network.chat.Component;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.TooltipFlag;
import net.neoforged.fml.ModList;
import net.neoforged.neoforge.registries.NeoForgeRegistries;

import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import static com.benbenlaw.casting.util.ValidToolTypesForToolModifiers.*;

public class ToolModifierItem extends Item {

    private final String tooltip;
    private final int maxLevel;

    public ToolModifierItem(Properties properties, String tooltip, int maxLevel) {
        super(properties);
        this.tooltip = tooltip;
        this.maxLevel = maxLevel;
    }

    @Override
    public void appendHoverText(ItemStack stack, TooltipContext context, List<Component> components, TooltipFlag flag) {

        if (Screen.hasShiftDown()) {
            components.add(Component.translatable(tooltip, maxLevel).withStyle(ChatFormatting.YELLOW));

            components.add(Component.translatable("casting.tooltip.valid_tool_types").withStyle(ChatFormatting.GOLD));

            //add list of valid tool types
            String effect = BuiltInRegistries.ITEM.getKey(this).getPath();

            List<String> validToolTypes = TOOL_TYPE_VALID_MODIFIERS.entrySet().stream()
                    .filter(entry -> {
                        String key = entry.getKey();
                        if (key.equals(ALL_MODIFIERS)) return false;
                        if (key.equals(PAXEL_MODIFIERS) && !ModList.get().isLoaded("mekanismtools")) return false;
                        return entry.getValue().contains(effect);
                    })
                    .map(Map.Entry::getKey)
                    .toList();

            for (String toolType : validToolTypes) {
                components.add(Component.translatable("casting.tooltip." + toolType).withStyle(ChatFormatting.BLUE));
            }


        } else {
            components.add(Component.translatable("tooltips.item.shift.not_held").withStyle(ChatFormatting.YELLOW));
        }
    }
}
