package com.benbenlaw.casting.event;

import com.benbenlaw.casting.Casting;
import com.benbenlaw.casting.config.EquipmentModifierConfig;
import com.benbenlaw.casting.item.EquipmentModifier;
import com.benbenlaw.casting.util.CastingTags;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Holder;
import net.minecraft.core.particles.ParticleTypes;
import net.minecraft.core.registries.Registries;
import net.minecraft.resources.ResourceKey;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.damagesource.*;
import net.minecraft.world.effect.MobEffectInstance;
import net.minecraft.world.effect.MobEffects;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EquipmentSlot;
import net.minecraft.world.entity.ai.attributes.Attributes;
import net.minecraft.world.entity.item.ItemEntity;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.enchantment.Enchantment;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.phys.AABB;
import net.minecraft.world.phys.Vec3;
import net.neoforged.bus.api.SubscribeEvent;
import net.neoforged.fml.common.EventBusSubscriber;
import net.neoforged.neoforge.event.entity.living.LivingDamageEvent;
import net.neoforged.neoforge.event.tick.PlayerTickEvent;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import static com.benbenlaw.casting.item.EquipmentModifier.*;

@EventBusSubscriber(modid = Casting.MOD_ID)

public class ArmorEvents {

    @SubscribeEvent
    public static void onPlayerTick(PlayerTickEvent.Pre event) {

        Player player = event.getEntity();
        Level level = player.level();

        if (level.isClientSide()) return;

        boolean isStepAssist = player.getItemBySlot(EquipmentSlot.FEET).getComponents().keySet().contains(STEP_ASSIST.dataComponent.get());

        //Speed

        int totalSpeedLevel = 0;

        if (player.getItemBySlot(EquipmentSlot.FEET).getComponents().keySet().contains(SPEED.dataComponent.get())  && isToggleableModifierActive(player.getItemBySlot(EquipmentSlot.FEET))) {
            totalSpeedLevel += (int) player.getItemBySlot(EquipmentSlot.FEET).getComponents().getOrDefault(SPEED.dataComponent.get(), 0);
        }

        if (player.getItemBySlot(EquipmentSlot.LEGS).getComponents().keySet().contains(SPEED.dataComponent.get()) && isToggleableModifierActive(player.getItemBySlot(EquipmentSlot.LEGS))) {
            totalSpeedLevel += (int) player.getItemBySlot(EquipmentSlot.LEGS).getComponents().getOrDefault(SPEED.dataComponent.get(), 0);
        }

        if (totalSpeedLevel > 0) {
            player.addEffect(new MobEffectInstance(MobEffects.MOVEMENT_SPEED, 22, totalSpeedLevel, false, false));
        }

        //Step Assist
        if (isStepAssist && !player.isShiftKeyDown() && isToggleableModifierActive(player.getItemBySlot(EquipmentSlot.FEET))) {
            int stepAssistLevel = (int) player.getItemBySlot(EquipmentSlot.FEET).getComponents().getOrDefault(STEP_ASSIST.dataComponent.get(), 0);
            Objects.requireNonNull(player.getAttribute(Attributes.STEP_HEIGHT)).setBaseValue(stepAssistLevel);
        } else {
            Objects.requireNonNull(player.getAttribute(Attributes.STEP_HEIGHT)).setBaseValue(0.6D);
        }

        //Magnet
        for (ItemStack armorItem : player.getArmorSlots()) {
            if (armorItem.getComponents().keySet().contains(MAGNET.dataComponent.get()) && isToggleableModifierActive(armorItem)) {

                int range = (int) armorItem.getComponents().getOrDefault(MAGNET.dataComponent.get(), 0);

                AABB box = player.getBoundingBox().inflate(range);
                List<ItemEntity> items = level.getEntitiesOfClass(ItemEntity.class, box, item ->
                        !item.hasPickUpDelay() && item.getItem().getCount() > 0);

                for (ItemEntity itemEntity : items) {
                    if (itemEntity.hasPickUpDelay())
                        continue;

                    ItemStack stack = itemEntity.getItem();

                    boolean success = player.getInventory().add(stack);
                    if (success) {
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

        //Night Vision
        boolean isNightVision = (boolean) player.getItemBySlot(EquipmentSlot.HEAD).getOrDefault(NIGHT_VISION.dataComponent.get(), false);
        if (isNightVision && isToggleableModifierActive(player.getItemBySlot(EquipmentSlot.HEAD))) {
            player.addEffect(new MobEffectInstance(MobEffects.NIGHT_VISION, 22, 0, false, false));
            if (player.tickCount % EquipmentModifierConfig.timeForDamageOnNightVision.get() == 0) {
                player.getItemBySlot(EquipmentSlot.HEAD).hurtAndBreak(1, player, EquipmentSlot.HEAD);
            }
        }

        //Water Breathing
        boolean isWaterBreathing = (boolean) player.getItemBySlot(EquipmentSlot.HEAD).getOrDefault(WATER_BREATHING.dataComponent.get(), false);
        if (isWaterBreathing && player.isUnderWater()) {
            player.addEffect(new MobEffectInstance(MobEffects.WATER_BREATHING, 22, 0, false, false));
            if (player.tickCount % EquipmentModifierConfig.timeForDamageOnWaterBreathing.get() == 0) {
                player.getItemBySlot(EquipmentSlot.HEAD).hurtAndBreak(1, player, EquipmentSlot.HEAD);
            }
        }
    }

    @SubscribeEvent
    public static void onPlayerTick(PlayerTickEvent.Post event) {
        Player player = event.getEntity();
        Level level = player.level();

        if (!player.level().isClientSide()) {
            boolean isFlight = (boolean) player.getItemBySlot(EquipmentSlot.CHEST).getOrDefault(FLIGHT.dataComponent.get(), false);
            if (isFlight && isToggleableModifierActive(player.getItemBySlot(EquipmentSlot.CHEST))) {

                if (!player.isCreative() && !player.isSpectator() && !player.getAbilities().mayfly) {
                    player.addTag("casting_flight");
                    player.getAbilities().mayfly = true;
                    player.onUpdateAbilities();
                }

            } else {
                if (!player.isCreative() && !player.isSpectator() && player.getAbilities().mayfly && player.getTags().contains("casting_flight")) {
                    player.removeTag("casting_flight");
                    player.getAbilities().mayfly = false;
                    player.onUpdateAbilities();
                }

            }
        }

        // Water Walker
        boolean isWaterWalker = (boolean) player.getItemBySlot(EquipmentSlot.FEET).getComponents().getOrDefault
                (EquipmentModifier.WATER_WALKER.dataComponent.get(), false);

        float playerBob = player.bob;

        if (isWaterWalker && !player.isShiftKeyDown() && isToggleableModifierActive(player.getItemBySlot(EquipmentSlot.FEET))) {
            BlockPos pos = player.blockPosition();
            BlockState currentBlock = level.getBlockState(pos);
            BlockState aboveBlock = level.getBlockState(pos.above());

            boolean isAtWaterSurface = currentBlock.is(CastingTags.Blocks.EFFECTED_BY_WATER_WALKER) && !aboveBlock.is(CastingTags.Blocks.EFFECTED_BY_WATER_WALKER);

            if (isAtWaterSurface) {
                double surfaceY = pos.getY() + 1.0;
                double playerY = player.getY();
                double desiredNewY = playerY + (surfaceY - playerY) * 0.1;
                AABB futureBox = player.getBoundingBox().move(0, desiredNewY - playerY, 0);

                if (!level.noCollision(futureBox)) {
                    double step = 0.05;
                    double maxSafeY = playerY;
                    for (double testY = playerY; testY <= surfaceY; testY += step) {
                        AABB testBox = player.getBoundingBox().move(0, testY - playerY, 0);
                        if (level.noCollision(testBox)) {
                            maxSafeY = testY;
                        } else {
                            break;
                        }
                    }

                    player.setPos(player.getX(), maxSafeY, player.getZ());
                    player.setDeltaMovement(player.getDeltaMovement().x, 0.0, player.getDeltaMovement().z);

                    if (maxSafeY >= surfaceY - 0.5) {
                        player.setOnGround(true);
                    }
                    return;
                }

                player.setPos(player.getX(), desiredNewY, player.getZ());

                boolean isBob = false;

                if (desiredNewY >= surfaceY - 0.5 && desiredNewY < surfaceY) {
                    isBob = true;
                    player.setDeltaMovement(player.getDeltaMovement().x, 0.0, player.getDeltaMovement().z);
                    player.setOnGround(true);
                }

                if (isBob) {
                    float f = Math.min(0.1F, (float) player.getDeltaMovement().horizontalDistance());
                    playerBob += (f - playerBob) * 0.7F;
                    player.bob = playerBob;
                }
            }
        }

        // Lava Walker
        boolean isLavaWalker = (boolean) player.getItemBySlot(EquipmentSlot.FEET).getComponents()
                .getOrDefault(LAVA_WALKER.dataComponent.get(), false);

        if (isLavaWalker && !player.isShiftKeyDown() && isToggleableModifierActive(player.getItemBySlot(EquipmentSlot.FEET))) {
            BlockPos pos = player.blockPosition();
            BlockState currentBlock = level.getBlockState(pos);
            BlockState aboveBlock = level.getBlockState(pos.above());

            boolean isAtLavaSurface = currentBlock.is(CastingTags.Blocks.EFFECTED_BY_LAVA_WALKER) && !aboveBlock.is(CastingTags.Blocks.EFFECTED_BY_LAVA_WALKER);

            if (isAtLavaSurface) {
                double surfaceY = pos.getY() + 1.0;
                double playerY = player.getY();
                double desiredNewY = playerY + (surfaceY - playerY) * 0.1;
                AABB futureBox = player.getBoundingBox().move(0, desiredNewY - playerY, 0);

                if (!level.noCollision(futureBox)) {
                    double step = 0.05;
                    double maxSafeY = playerY;

                    for (double testY = playerY; testY <= surfaceY; testY += step) {
                        AABB testBox = player.getBoundingBox().move(0, testY - playerY, 0);
                        if (level.noCollision(testBox)) {
                            maxSafeY = testY;
                        } else {
                            break;
                        }
                    }

                    player.setPos(player.getX(), maxSafeY, player.getZ());
                    player.setDeltaMovement(player.getDeltaMovement().x, 0.0, player.getDeltaMovement().z);

                    if (maxSafeY >= surfaceY - 0.5) {
                        player.setOnGround(true);
                    }

                    return;
                }

                player.setPos(player.getX(), desiredNewY, player.getZ());

                boolean isBob = false;

                if (desiredNewY >= surfaceY - 0.5 && desiredNewY < surfaceY) {
                    isBob = true;
                    player.setDeltaMovement(player.getDeltaMovement().x, 0.0, player.getDeltaMovement().z);
                    player.setOnGround(true);
                }

                if (isBob) {
                    float f = Math.min(0.1F, (float) player.getDeltaMovement().horizontalDistance());
                    playerBob += (f - playerBob) * 0.7F;
                    player.bob = playerBob;
                }
            }
        }

        //Jets
        int totalJetLevel = 0;

        for (EquipmentSlot slot : new EquipmentSlot[]{EquipmentSlot.FEET, EquipmentSlot.LEGS, EquipmentSlot.CHEST, EquipmentSlot.HEAD}) {
            ItemStack armorPiece = player.getItemBySlot(slot);

            if (armorPiece.getComponents().has(JETS.dataComponent.get()) && isToggleableModifierActive(armorPiece)) {
                totalJetLevel += (int) armorPiece.getComponents().getOrDefault(JETS.dataComponent.get(), 0);
            }
        }

        boolean jumpHeld = player.getPersistentData().getBoolean("casting_is_jumping");

        if (totalJetLevel > 0 && jumpHeld && !player.isInWaterOrBubble()) {
            player.addEffect(new MobEffectInstance(MobEffects.LEVITATION, 1, totalJetLevel, false, false));
            player.setOnGround(false);
            player.fallDistance = 0;

            if (level instanceof ServerLevel serverLevel) {
                serverLevel.sendParticles(ParticleTypes.FLAME, player.getX(), player.getY() + 0.25, player.getZ(),
                        5, 0.2, 0.2, 0.2, 0.001);
            }

            if (player.tickCount % 100 == 0) {
                List<EquipmentSlot> damagedSlots = new ArrayList<>();

                for (EquipmentSlot slot : new EquipmentSlot[]{EquipmentSlot.FEET, EquipmentSlot.LEGS, EquipmentSlot.CHEST, EquipmentSlot.HEAD}) {
                    ItemStack armorPiece = player.getItemBySlot(slot);
                    if (armorPiece.getComponents().has(JETS.dataComponent.get()) && isToggleableModifierActive(armorPiece)) {
                        damagedSlots.add(slot);
                    }
                }

                if (!damagedSlots.isEmpty()) {
                    EquipmentSlot slotToDamage = damagedSlots.get(player.getRandom().nextInt(damagedSlots.size()));
                    ItemStack armorPiece = player.getItemBySlot(slotToDamage);
                    armorPiece.hurtAndBreak(1, player, slotToDamage);
                }
            }
        }
    }

    @SubscribeEvent
    public static void onPlayerDamage(LivingDamageEvent.Pre event) {
        if (!(event.getEntity() instanceof Player player)) return;
        if (player.level().isClientSide()) return;

        float originalDamage = event.getOriginalDamage();
        DamageSource source = event.getSource();

        int armorValue = player.getArmorValue();
        float armorToughness = (float) player.getAttributeValue(Attributes.ARMOR_TOUGHNESS);
        float damageAfterArmor = CombatRules.getDamageAfterAbsorb(player, originalDamage, source, armorValue, armorToughness);

        float totalCustomReduction = 0f;

        for (EquipmentSlot slot : EquipmentSlot.values()) {
            if (slot.getType() != EquipmentSlot.Type.HUMANOID_ARMOR) continue;

            ItemStack armor = player.getItemBySlot(slot);
            if (armor.isEmpty()) continue;

            if (armor.getComponents().has(PROTECTION.dataComponent.get())) {
                int protectionLevel = (int) armor.getComponents().getOrDefault(PROTECTION.dataComponent.get(), 0);
                totalCustomReduction += protectionLevel * EquipmentModifierConfig.percentageOfProtectionDamagePerProtectionLevel.get();
            }
        }

        totalCustomReduction = Math.min(totalCustomReduction, 0.8f);

        float finalDamage = damageAfterArmor * (1.0f - totalCustomReduction);

        if (event.getSource().is(DamageTypes.FALL)) {
            ItemStack boots = player.getItemBySlot(EquipmentSlot.FEET);
            if (!boots.isEmpty() && boots.getComponents().keySet().contains(FEATHER_FALLING.dataComponent.get())) {
                int featherFallingLevel = (int) boots.getComponents().getOrDefault(FEATHER_FALLING.dataComponent.get(), 0);
                float fallReduction = featherFallingLevel * 0.1f;

                fallReduction = Math.min(fallReduction, 0.8f);
                finalDamage *= (1.0f - fallReduction);

                boots.hurtAndBreak(featherFallingLevel, player, EquipmentSlot.FEET);
            }
        }

        event.setNewDamage(finalDamage);
    }


    public static boolean isToggleableModifierActive(ItemStack tool) {
        if (!tool.getComponents().has(EquipmentModifier.TOGGLEABLE_MODIFIERS.get())) {
            return true;
        }
        if (tool.getComponents().keySet().contains(TOGGLEABLE_MODIFIERS.get())) {
            if (Boolean.TRUE.equals(tool.getComponents().get(TOGGLEABLE_MODIFIERS.get()))) {
                return true;
            }
            if (Boolean.FALSE.equals(tool.getComponents().get(TOGGLEABLE_MODIFIERS.get()))) {
                return false;
            }
        }
        return false;
    }
}
