package com.benbenlaw.casting.event;

import com.benbenlaw.casting.Casting;
import com.benbenlaw.casting.config.EquipmentModifierConfig;
import com.benbenlaw.casting.item.CastingDataComponents;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.neoforged.bus.api.SubscribeEvent;
import net.neoforged.fml.common.EventBusSubscriber;
import net.neoforged.neoforge.event.entity.player.PlayerXpEvent;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.Random;

import static com.benbenlaw.casting.util.EquipmentModifierUtils.getExperienceModifierLevel;

@EventBusSubscriber(modid = Casting.MOD_ID)

public class ExperienceEvents {

    @SubscribeEvent
    public static void onPlayerTick(PlayerXpEvent.PickupXp event) {
        //System.out.println("XP Picked Up: " + event.getOrb().getValue());

        Player player = event.getEntity();
        Inventory inventory = player.getInventory();
        List<ItemStack> itemsWithExpComponent = new ArrayList<>();

        // Collect from inventory
        for (int i = 0; i < inventory.getContainerSize(); i++) {
            ItemStack stack = inventory.getItem(i);
            if (!stack.isEmpty() && stack.getComponents().keySet().contains(CastingDataComponents.EQUIPMENT_EXPERIENCE.get())) {
                itemsWithExpComponent.add(stack);
            }
        }

        int xpValue = event.getOrb().getValue();

        if (!itemsWithExpComponent.isEmpty() && xpValue > 0) {
            Random random = new Random();

            for (int i = 0; i < xpValue; i++) {
                ItemStack chosen = itemsWithExpComponent.get(random.nextInt(itemsWithExpComponent.size()));
                int currentExp = Optional.ofNullable(chosen.getComponents().get(CastingDataComponents.EQUIPMENT_EXPERIENCE.get())).orElse(0);

                currentExp += 1;

                int toolLevel = Optional.ofNullable(chosen.getComponents().get(CastingDataComponents.EQUIPMENT_LEVEL.get())).orElse(0);
                double modifierLevel = getExperienceModifierLevel(toolLevel);
                if (currentExp >= (int) (EquipmentModifierConfig.experiencePerLevelForEquipmentLevel.get() + (EquipmentModifierConfig.experiencePerLevelForEquipmentLevel.get() * modifierLevel))) {
                    currentExp = 0;


                    int currentLevel = Optional.ofNullable(chosen.getComponents().get(CastingDataComponents.EQUIPMENT_LEVEL.get())).orElse(0);
                    if (currentLevel < EquipmentModifierConfig.maxEquipmentLevel.get()) {
                        currentLevel += 1;
                    }

                    chosen.set(CastingDataComponents.EQUIPMENT_LEVEL.get(), currentLevel);
                }
                chosen.set(CastingDataComponents.EQUIPMENT_EXPERIENCE.get(), currentExp);
            }
        }
    }


}
