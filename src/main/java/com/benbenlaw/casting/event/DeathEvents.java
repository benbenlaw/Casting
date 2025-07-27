package com.benbenlaw.casting.event;

import com.benbenlaw.casting.Casting;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.inventory.Slot;
import net.minecraft.world.item.ItemStack;
import net.neoforged.bus.api.SubscribeEvent;
import net.neoforged.fml.common.EventBusSubscriber;
import net.neoforged.neoforge.event.entity.living.LivingDeathEvent;
import net.neoforged.neoforge.event.entity.player.PlayerEvent;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static com.benbenlaw.casting.item.EquipmentModifier.SOULBOUND;

@EventBusSubscriber(modid = Casting.MOD_ID)
public class DeathEvents {

    public static Map<ItemStack, Integer> soulboundItems = new HashMap<>();

    @SubscribeEvent
    public static void getPlayerInventory(LivingDeathEvent event) {
        Entity entity = event.getEntity();

        if (entity instanceof ServerPlayer player) {


            List<ItemStack> main = player.getInventory().items;
            for (int i = 0; i < main.size(); i++) {
                ItemStack stack = main.get(i);
                if (!stack.isEmpty() && stack.getComponents().has(SOULBOUND.dataComponent.get())) {
                    soulboundItems.put(stack.copy(), i);
                    stack.setCount(0);
                }
            }

            List<ItemStack> armor = player.getInventory().armor;
            for (int i = 0; i < armor.size(); i++) {
                ItemStack stack = armor.get(i);
                if (!stack.isEmpty() && stack.getComponents().has(SOULBOUND.dataComponent.get())) {
                    soulboundItems.put(stack.copy(), 36 + i);
                    stack.setCount(0);
                }
            }

            List<ItemStack> offhand = player.getInventory().offhand;
            for (int i = 0; i < offhand.size(); i++) {
                ItemStack stack = offhand.get(i);
                if (!stack.isEmpty() && stack.getComponents().has(SOULBOUND.dataComponent.get())) {
                    soulboundItems.put(stack.copy(), 40 + i);
                    stack.setCount(0);
                }
            }
        }
    }

    @SubscribeEvent
    public static void addSoulboundItems(PlayerEvent.PlayerRespawnEvent event) {
        Entity entity = event.getEntity();

        if (entity instanceof ServerPlayer player) {
            for (Map.Entry<ItemStack, Integer> entry : soulboundItems.entrySet()) {
                ItemStack stack = entry.getKey();
                int slotIndex = entry.getValue();

                if (player.getInventory().getItem(slotIndex).isEmpty()) {
                    player.getInventory().setItem(slotIndex, stack);
                }
            }
            soulboundItems.clear();
        }
    }



}
