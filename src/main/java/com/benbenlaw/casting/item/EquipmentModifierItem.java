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
import org.jetbrains.annotations.NotNull;

import java.util.List;
import java.util.Map;
import java.util.function.Supplier;

import static com.benbenlaw.casting.util.ValidToolTypesForToolModifiers.*;

public class EquipmentModifierItem extends Item {

    private final String tooltip;
    private final Supplier<Integer> maxLevel;

    public EquipmentModifierItem(Properties properties, String tooltip, Supplier<Integer> maxLevel) {
        super(properties);
        this.tooltip = tooltip;
        this.maxLevel = maxLevel;
    }

    public EquipmentModifierItem(Properties properties, String tooltip, int maxLevel) {
        super(properties);
        this.tooltip = tooltip;
        this.maxLevel = maxLevel == -1 ? () -> Integer.MAX_VALUE : () -> maxLevel;
    }

    @Override
    public void appendHoverText(ItemStack stack, @NotNull TooltipContext context, @NotNull List<Component> components, @NotNull TooltipFlag flag) {

        boolean requireShiftToDisableTooltip = stack.is(CastingTags.Items.CAN_BE_DISABLED_WITH_SHIFT);
        boolean requireShiftToToggleTooltip = stack.is(CastingTags.Items.CAN_BE_TOGGLED_WITH_SHIFT);
        boolean keybindToToggle = stack.is(CastingTags.Items.CAN_BE_TOGGLED_WITH_KEYBIND);

        if (Screen.hasShiftDown()) {

            components.add(Component.translatable(tooltip, maxLevel.get()).withStyle(ChatFormatting.YELLOW));

            if (requireShiftToDisableTooltip) {
                components.add(Component.translatable("tooltips.casting.information.shift_to_disable").withStyle(ChatFormatting.RED));
            }

            if (requireShiftToToggleTooltip) {
                components.add(Component.translatable("tooltips.casting.information.shift_to_toggle").withStyle(ChatFormatting.RED));
            }

                if (keybindToToggle) {
                    components.add(Component.translatable("tooltips.casting.information.keybind_to_toggle").withStyle(ChatFormatting.RED));
                }

                components.add(Component.translatable("tooltips.casting.information.valid_tool_types").withStyle(ChatFormatting.GOLD));

            String itemPath = BuiltInRegistries.ITEM.getKey(this).getPath();
            EquipmentModifier modifier = EquipmentModifier.valueOf(itemPath.toUpperCase());

            List<String> validToolTypes = VALID_MODIFIERS.entrySet().stream()
                    .filter(entry -> {
                        String key = entry.getKey();
                        if (key.equals(ALL_MODIFIERS)) return false;
                        if (key.equals(PAXEL_MODIFIERS) && !ModList.get().isLoaded("mekanismtools")) return false;
                        return entry.getValue().contains(modifier); // Make sure this is EquipmentModifierItems
                    })
                    .map(Map.Entry::getKey)
                    .toList();

            for (String toolType : validToolTypes) {
                components.add(Component.translatable("tooltips.casting." + toolType).withStyle(ChatFormatting.BLUE));
            }

        } else {
            components.add(Component.translatable("tooltips.bblcore.shift").withStyle(ChatFormatting.YELLOW));
        }
    }
}
