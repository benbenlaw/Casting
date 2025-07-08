package com.benbenlaw.casting.network.packet;

import com.benbenlaw.casting.block.entity.multiblock.MultiblockSolidifierBlockEntity;
import com.benbenlaw.casting.item.CastingDataComponents;
import com.benbenlaw.casting.network.payload.SolidifierSelectedFluidPayload;
import com.benbenlaw.casting.network.payload.ToggleArmorModifiersPayload;
import net.minecraft.core.BlockPos;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.EquipmentSlot;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.entity.BlockEntity;
import net.neoforged.neoforge.network.handling.IPayloadContext;

public class ToggleArmorModifiersPacket {
    public static final ToggleArmorModifiersPacket INSTANCE = new ToggleArmorModifiersPacket();

    public static ToggleArmorModifiersPacket get() {
        return INSTANCE;
    }

    public void handle(final ToggleArmorModifiersPayload payload, IPayloadContext context) {

        Player player = context.player();
        Level level = player.level();
        int armorSlot = payload.armorSlot();

        if (armorSlot == 1) {
            boolean nightVision =  player.getItemBySlot(EquipmentSlot.HEAD).getComponents().keySet().contains(CastingDataComponents.NIGHT_VISION.get());
            boolean magnet =  player.getItemBySlot(EquipmentSlot.HEAD).getComponents().keySet().contains(CastingDataComponents.MAGNET.get());
            boolean containsToggleableModifier = nightVision || magnet;
            ItemStack armor = player.getItemBySlot(EquipmentSlot.HEAD);

            if (containsToggleableModifier) {
                if (!armor.getComponents().has(CastingDataComponents.TOGGLEABLE_MODIFIERS.get())) {
                    armor.set(CastingDataComponents.TOGGLEABLE_MODIFIERS.get(), true);
                }
                else if (Boolean.TRUE.equals(armor.get(CastingDataComponents.TOGGLEABLE_MODIFIERS.get()))) {
                    armor.set(CastingDataComponents.TOGGLEABLE_MODIFIERS.get(), false);
                }
                else if (Boolean.FALSE.equals(armor.get(CastingDataComponents.TOGGLEABLE_MODIFIERS.get()))) {
                    armor.set(CastingDataComponents.TOGGLEABLE_MODIFIERS.get(), true);
                }

                player.sendSystemMessage(Component.translatable("chat.casting.information.helmet_toggle"));
            }
        }

        if (armorSlot == 2) {
            boolean magnet =  player.getItemBySlot(EquipmentSlot.CHEST).getComponents().keySet().contains(CastingDataComponents.MAGNET.get());
            boolean flight =  player.getItemBySlot(EquipmentSlot.CHEST).getComponents().keySet().contains(CastingDataComponents.FLIGHT.get());
            boolean containsToggleableModifier = flight || magnet;
            ItemStack armor = player.getItemBySlot(EquipmentSlot.CHEST);

            if (containsToggleableModifier) {
                if (!armor.getComponents().has(CastingDataComponents.TOGGLEABLE_MODIFIERS.get())) {
                    armor.set(CastingDataComponents.TOGGLEABLE_MODIFIERS.get(), true);
                }
                else if (Boolean.TRUE.equals(armor.get(CastingDataComponents.TOGGLEABLE_MODIFIERS.get()))) {
                    armor.set(CastingDataComponents.TOGGLEABLE_MODIFIERS.get(), false);
                }
                else if (Boolean.FALSE.equals(armor.get(CastingDataComponents.TOGGLEABLE_MODIFIERS.get()))) {
                    armor.set(CastingDataComponents.TOGGLEABLE_MODIFIERS.get(), true);
                }

                player.sendSystemMessage(Component.translatable("chat.casting.information.chestplate_toggle"));

            }
        }

        if (armorSlot == 3) {
            boolean magnet =  player.getItemBySlot(EquipmentSlot.LEGS).getComponents().keySet().contains(CastingDataComponents.MAGNET.get());
            boolean speed =  player.getItemBySlot(EquipmentSlot.LEGS).getComponents().keySet().contains(CastingDataComponents.SPEED.get());
            boolean containsToggleableModifier = speed || magnet;
            ItemStack armor = player.getItemBySlot(EquipmentSlot.LEGS);

            if (containsToggleableModifier) {
                if (!armor.getComponents().has(CastingDataComponents.TOGGLEABLE_MODIFIERS.get())) {
                    armor.set(CastingDataComponents.TOGGLEABLE_MODIFIERS.get(), true);
                }
                else if (Boolean.TRUE.equals(armor.get(CastingDataComponents.TOGGLEABLE_MODIFIERS.get()))) {
                    armor.set(CastingDataComponents.TOGGLEABLE_MODIFIERS.get(), false);
                }
                else if (Boolean.FALSE.equals(armor.get(CastingDataComponents.TOGGLEABLE_MODIFIERS.get()))) {
                    armor.set(CastingDataComponents.TOGGLEABLE_MODIFIERS.get(), true);
                }

                player.sendSystemMessage(Component.translatable("chat.casting.information.leggings_toggle"));

            }
        }

        if (armorSlot == 4) {
            boolean magnet =  player.getItemBySlot(EquipmentSlot.FEET).getComponents().keySet().contains(CastingDataComponents.MAGNET.get());
            boolean speed =  player.getItemBySlot(EquipmentSlot.FEET).getComponents().keySet().contains(CastingDataComponents.SPEED.get());
            boolean stepAssist =  player.getItemBySlot(EquipmentSlot.FEET).getComponents().keySet().contains(CastingDataComponents.STEP_ASSIST.get());
            boolean waterWalker =  player.getItemBySlot(EquipmentSlot.FEET).getComponents().keySet().contains(CastingDataComponents.WATER_WALKER.get());
            boolean lavaWalker =  player.getItemBySlot(EquipmentSlot.FEET).getComponents().keySet().contains(CastingDataComponents.LAVA_WALKER.get());
            boolean containsToggleableModifier = speed || magnet || stepAssist || waterWalker || lavaWalker;
            ItemStack armor = player.getItemBySlot(EquipmentSlot.FEET);

            if (containsToggleableModifier) {
                if (!armor.getComponents().has(CastingDataComponents.TOGGLEABLE_MODIFIERS.get())) {
                    armor.set(CastingDataComponents.TOGGLEABLE_MODIFIERS.get(), true);
                }
                else if (Boolean.TRUE.equals(armor.get(CastingDataComponents.TOGGLEABLE_MODIFIERS.get()))) {
                    armor.set(CastingDataComponents.TOGGLEABLE_MODIFIERS.get(), false);
                }
                else if (Boolean.FALSE.equals(armor.get(CastingDataComponents.TOGGLEABLE_MODIFIERS.get()))) {
                    armor.set(CastingDataComponents.TOGGLEABLE_MODIFIERS.get(), true);
                }
            }

            player.sendSystemMessage(Component.translatable("chat.casting.information.boots_toggle"));

        }
    }
}