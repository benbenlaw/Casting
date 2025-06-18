package com.benbenlaw.casting.mixin;

import com.benbenlaw.casting.item.CastingDataComponents;
import net.minecraft.util.RandomSource;
import net.minecraft.world.entity.EquipmentSlot;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.item.ItemStack;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

@Mixin(ItemStack.class)
public class ItemStackMixin {

    @Inject(
            method = "hurtAndBreak(ILnet/minecraft/world/entity/LivingEntity;Lnet/minecraft/world/entity/EquipmentSlot;)V",
            at = @At("HEAD"),
            cancellable = true
    )
    private void casting$customDurabilityHandling(
            int amount,
            LivingEntity entity,
            EquipmentSlot slot,
            CallbackInfo ci
    ) {
        ItemStack self = (ItemStack)(Object)this;

        if (!self.isDamageableItem()) return;

        if (self.getComponents().has(CastingDataComponents.UNBREAKING.get())) {
            int level = self.getOrDefault(CastingDataComponents.UNBREAKING.get(), 0);
            float chance = level * 0.1f;

            RandomSource random = (entity != null) ? entity.getRandom() : RandomSource.create();

            if (random.nextFloat() < chance) {
                ci.cancel(); // Cancel the durability loss
            }
        }
    }
}