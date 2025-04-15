package com.benbenlaw.casting.event;

import com.benbenlaw.casting.Casting;
import com.benbenlaw.casting.item.CastingDataComponents;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Holder;
import net.minecraft.core.HolderLookup;
import net.minecraft.core.Registry;
import net.minecraft.core.registries.Registries;
import net.minecraft.resources.ResourceKey;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.EquipmentSlot;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.enchantment.Enchantment;
import net.minecraft.world.item.enchantment.EnchantmentInstance;
import net.minecraft.world.item.enchantment.Enchantments;
import net.minecraft.world.item.enchantment.ItemEnchantments;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.Blocks;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.storage.loot.LootContext;
import net.minecraft.world.level.storage.loot.LootParams;
import net.minecraft.world.level.storage.loot.parameters.LootContextParams;
import net.minecraft.world.phys.Vec3;
import net.neoforged.api.distmarker.Dist;
import net.neoforged.bus.api.SubscribeEvent;
import net.neoforged.fml.common.EventBusSubscriber;
import net.neoforged.neoforge.event.entity.player.PlayerEvent;
import net.neoforged.neoforge.event.level.BlockEvent;
import net.neoforged.neoforge.event.tick.PlayerTickEvent;

import java.util.List;
import java.util.Objects;

@EventBusSubscriber(modid = Casting.MOD_ID)

public class ToolEvents {

    @SubscribeEvent

    public static void onBlockBreak(BlockEvent.BreakEvent event) {

        Player player = event.getPlayer();
        Level level = player.level();
        BlockPos pos = event.getPos();
        BlockState state = event.getState();
        ItemStack tool = player.getMainHandItem();
        List<ItemStack> drops = List.of();

        if (!level.isClientSide() && Boolean.TRUE.equals(tool.getComponents().get(CastingDataComponents.SILK_TOUCH.get()))) {
            event.setCanceled(true);

            ItemStack fakeItemStack = tool.copy();

            fakeItemStack.enchant(toHolder(level, Enchantments.SILK_TOUCH), 1);


            LootParams.Builder lootParams = new LootParams.Builder((ServerLevel) level)
                    .withParameter(LootContextParams.ORIGIN, Vec3.atCenterOf(pos))
                    .withParameter(LootContextParams.TOOL, fakeItemStack)
                    .withParameter(LootContextParams.BLOCK_ENTITY, null)
                    .withParameter(LootContextParams.THIS_ENTITY, player)
                    .withParameter(LootContextParams.BLOCK_STATE, state);

            drops = state.getDrops(lootParams);


        }

        else if (!level.isClientSide() && Boolean.TRUE.equals(tool.getComponents().keySet().contains(CastingDataComponents.FORTUNE.get()))) {
            event.setCanceled(true);

            ItemStack fakeItemStack = tool.copy();
            int fortuneLevel = tool.getComponents().get(CastingDataComponents.FORTUNE.get());

            fakeItemStack.enchant(toHolder(level, Enchantments.FORTUNE), fortuneLevel);


            LootParams.Builder lootParams = new LootParams.Builder((ServerLevel) level)
                    .withParameter(LootContextParams.ORIGIN, Vec3.atCenterOf(pos))
                    .withParameter(LootContextParams.TOOL, fakeItemStack)
                    .withParameter(LootContextParams.BLOCK_ENTITY, null)
                    .withParameter(LootContextParams.THIS_ENTITY, player)
                    .withParameter(LootContextParams.BLOCK_STATE, state);

            drops = state.getDrops(lootParams);

        }

        //FIXME - ALWAYS TAKES A POINT OIF DAMAGE AT THE START WHEN FULLY REPAIRED DUE TO THE EVENT TRIGGERING IN THE ITEM-STACK

        if (drops.isEmpty()) {
            LootParams.Builder lootParams = new LootParams.Builder((ServerLevel) level)
                    .withParameter(LootContextParams.ORIGIN, Vec3.atCenterOf(pos))
                    .withParameter(LootContextParams.TOOL, tool)
                    .withParameter(LootContextParams.BLOCK_ENTITY, null)
                    .withParameter(LootContextParams.THIS_ENTITY, player)
                    .withParameter(LootContextParams.BLOCK_STATE, state);

            drops = state.getDrops(lootParams);
        }

        for (ItemStack drop : drops) {
            Block.popResource(level, pos, drop);
        }

        level.setBlock(pos, Blocks.AIR.defaultBlockState(), 3);

        boolean shouldTakeDamage = true;

        if (!level.isClientSide() && tool.getComponents().keySet().contains(CastingDataComponents.UNBREAKING.get())) {
            event.setCanceled(true);


            int unbreakingLevel = tool.get(CastingDataComponents.UNBREAKING.get());
            shouldTakeDamage = level.getRandom().nextFloat() >= (unbreakingLevel * 0.1f);


        }

        if (shouldTakeDamage) {
            tool.hurtAndBreak(1, player, EquipmentSlot.MAINHAND);

        }
    }



    @SubscribeEvent
    public static void onBreakingBlock(PlayerEvent.BreakSpeed event) {
        Player player = event.getEntity();
        Level level = player.level();
        BlockState state = level.getBlockState(event.getPosition().get());
        ItemStack tool = player.getMainHandItem();

        if (tool.getComponents().keySet().contains(CastingDataComponents.EFFICIENCY.get())) {
            Integer efficiencyLevel = tool.getComponents().get(CastingDataComponents.EFFICIENCY.get());

            if (efficiencyLevel != null && efficiencyLevel > 0) {
                // Check if the tool is effective on this block
                if (tool.isCorrectToolForDrops(state)) {
                    float baseSpeed = event.getOriginalSpeed();
                    float bonus = efficiencyLevel * efficiencyLevel + 1;
                    event.setNewSpeed(baseSpeed + bonus);
                }
            }
        }
    }


    @SubscribeEvent
    public static void onPlayerTick(PlayerTickEvent.Post event) {
        Inventory inventory = event.getEntity().getInventory();

        for (int i = 0; i < inventory.getContainerSize(); i++) {
            ItemStack stack = inventory.getItem(i);
            if (stack.isEmpty()) continue;

            if (stack.getComponents().keySet().contains(CastingDataComponents.REPAIRING.get())) {
                int repairingLevel = stack.getComponents().get(CastingDataComponents.REPAIRING.get());
                int repairTickTime = getRepairTickTime(repairingLevel);

                if (event.getEntity().tickCount % repairTickTime == 0) {
                    int currentDamage = stack.getDamageValue();
                    int newDamage = Math.max(currentDamage - 1, 0);

                    stack.setDamageValue(newDamage);
                }
            }
        }
    }

    private static int getRepairTickTime(int repairingLevel) {
        int repairTickTime = 220;

        switch (repairingLevel) {
            case 1 -> repairTickTime = 200;
            case 2 -> repairTickTime = 180;
            case 3 -> repairTickTime = 160;
            case 4 -> repairTickTime = 140;
            case 5 -> repairTickTime = 120;
            case 6 -> repairTickTime = 100;
            case 7 -> repairTickTime = 80;
            case 8 -> repairTickTime = 60;
            case 9 -> repairTickTime = 40;
            case 10 -> repairTickTime = 20;
        }
        return repairTickTime;
    }


    public static Holder<Enchantment> toHolder(Level level, ResourceKey<Enchantment> enchantment) {
        return level.registryAccess().registryOrThrow(Registries.ENCHANTMENT).getHolderOrThrow(enchantment);
    }

}
