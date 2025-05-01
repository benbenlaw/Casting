package com.benbenlaw.casting.event;

import com.benbenlaw.casting.Casting;
import com.benbenlaw.casting.config.EquipmentModifierConfig;
import com.benbenlaw.casting.item.CastingDataComponents;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Holder;
import net.minecraft.core.particles.ParticleTypes;
import net.minecraft.core.registries.Registries;
import net.minecraft.resources.ResourceKey;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EquipmentSlot;
import net.minecraft.world.entity.ai.attributes.Attributes;
import net.minecraft.world.entity.item.ItemEntity;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.enchantment.Enchantment;
import net.minecraft.world.level.Level;
import net.minecraft.world.phys.AABB;
import net.minecraft.world.phys.Vec3;
import net.neoforged.bus.api.SubscribeEvent;
import net.neoforged.fml.common.EventBusSubscriber;
import net.neoforged.neoforge.event.entity.living.LivingDamageEvent;
import net.neoforged.neoforge.event.tick.PlayerTickEvent;

import java.util.List;
import java.util.Objects;

@EventBusSubscriber(modid = Casting.MOD_ID)

public class ArmorEvents {


    @SubscribeEvent
    public static void onPlayerTick(PlayerTickEvent.Pre event) {

        Player player = event.getEntity();
        Level level = player.level();

        if (level.isClientSide()) return;
        if (player.tickCount % 20 != 0) return;

        boolean isStepAssist = player.getItemBySlot(EquipmentSlot.FEET).getComponents().keySet().contains(CastingDataComponents.STEP_ASSIST.get());

        //Step Assist
        if (isStepAssist) {
            int stepAssistLevel = player.getItemBySlot(EquipmentSlot.FEET).getComponents().getOrDefault(CastingDataComponents.STEP_ASSIST.get(), 0);
            Objects.requireNonNull(player.getAttribute(Attributes.STEP_HEIGHT)).setBaseValue(stepAssistLevel);
        } else {
            Objects.requireNonNull(player.getAttribute(Attributes.STEP_HEIGHT)).setBaseValue(0.6D);
        }


        //Magnet
        for (ItemStack armorItem : player.getArmorSlots()) {
            if (armorItem.getComponents().keySet().contains(CastingDataComponents.MAGNET.get())) {

                int range = armorItem.getComponents().getOrDefault(CastingDataComponents.MAGNET.get(), 0);

                AABB box = player.getBoundingBox().inflate(range);
                List<ItemEntity> items = level.getEntitiesOfClass(ItemEntity.class, box, item ->
                        !item.hasPickUpDelay() && item.getItem().getCount() > 0);

                for (ItemEntity itemEntity : items) {
                    if (itemEntity.hasPickUpDelay())
                        continue;

                    ItemStack stack = itemEntity.getItem();

                    boolean success = player.getInventory().add(stack);
                    if (success || stack.isEmpty()) {
                        itemEntity.remove(Entity.RemovalReason.DISCARDED);
                        if (level instanceof ServerLevel serverLevel) {
                            serverLevel.sendParticles(ParticleTypes.END_ROD, itemEntity.getX(), itemEntity.getY(), itemEntity.getZ(), 10, 0.1D, 0.1D, 0.1D, 0.1D);
                        }
                    } else {
                        itemEntity.setItem(stack);
                    }
                }

            }
        }



    }


    @SubscribeEvent
    public static void onPlayerDamage(LivingDamageEvent.Pre event) {
        Entity entity = event.getEntity();
        if (!(entity instanceof Player player)) return;

        Level level = player.level();
        if (level.isClientSide()) return;

        float originalDamage = event.getOriginalDamage();
        float totalReduction = 0f;

        for (EquipmentSlot slot : EquipmentSlot.values()) {
            if (slot.getType() != EquipmentSlot.Type.HUMANOID_ARMOR) continue;

            ItemStack armor = player.getItemBySlot(slot);
            if (armor.isEmpty()) continue;

            if (armor.getComponents().keySet().contains(CastingDataComponents.PROTECTION.get())) {
                int protectionLevel = armor.getComponents().getOrDefault(CastingDataComponents.PROTECTION.get(), 0);
                totalReduction += protectionLevel * EquipmentModifierConfig.percentageOfProtectionDamagePerProtectionLevel.get();
            }
        }

        //(max 80% reduction)
        totalReduction = Math.min(totalReduction, 0.8f);
        event.setNewDamage(originalDamage * (1.0f - totalReduction));
    }

    @SubscribeEvent
    public static void onPlayerDamage(LivingDamageEvent.Post event) {
        Entity entity = event.getEntity();
        if (!(entity instanceof Player player)) return;

        Level level = player.level();
        if (level.isClientSide()) return;

        for (EquipmentSlot slot : EquipmentSlot.values()) {
            if (slot.getType() != EquipmentSlot.Type.HUMANOID_ARMOR) continue;

            ItemStack armor = player.getItemBySlot(slot);
            if (armor.isEmpty()) continue;

            // --- Unbreaking ---
            boolean isUnbreaking = armor.getComponents().keySet().contains(CastingDataComponents.UNBREAKING.get());
            if (isUnbreaking) {
                int unbreakingLevel = armor.getComponents().getOrDefault(CastingDataComponents.UNBREAKING.get(), 0);
                float chance = unbreakingLevel * 0.1f;

                if (level.getRandom().nextFloat() < chance) {
                    armor.setDamageValue(armor.getDamageValue() - 1);
                }
            }
        }
    }





    //From In World Recipes // Move to BBL Core in the future
    public static void popOutTheItem(Level level, BlockPos blockPos, ItemStack itemStack) {

        Vec3 vec3 = Vec3.atLowerCornerWithOffset(blockPos, 0.5, 1.1, 0.5).offsetRandom(level.random, 0.7F);
        ItemStack itemstack1 = itemStack.copy();
        ItemEntity itementity = new ItemEntity(level, vec3.x(), vec3.y(), vec3.z(), itemstack1);
        itementity.setDefaultPickUpDelay();
        level.addFreshEntity(itementity);
    }


    public static Holder<Enchantment> toHolder(Level level, ResourceKey<Enchantment> enchantment) {
        return level.registryAccess().registryOrThrow(Registries.ENCHANTMENT).getHolderOrThrow(enchantment);
    }

}
