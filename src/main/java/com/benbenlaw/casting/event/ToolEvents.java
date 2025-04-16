package com.benbenlaw.casting.event;

import com.benbenlaw.casting.Casting;
import com.benbenlaw.casting.item.CastingDataComponents;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.core.Holder;
import net.minecraft.core.registries.Registries;
import net.minecraft.resources.ResourceKey;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.SimpleContainer;
import net.minecraft.world.entity.EquipmentSlot;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.crafting.*;
import net.minecraft.world.item.enchantment.Enchantment;
import net.minecraft.world.item.enchantment.Enchantments;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.Blocks;
import net.minecraft.world.level.block.TorchBlock;
import net.minecraft.world.level.block.WallTorchBlock;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.storage.loot.LootParams;
import net.minecraft.world.level.storage.loot.parameters.LootContextParams;
import net.minecraft.world.phys.Vec3;
import net.neoforged.bus.api.SubscribeEvent;
import net.neoforged.fml.common.EventBusSubscriber;
import net.neoforged.neoforge.common.util.RecipeMatcher;
import net.neoforged.neoforge.event.entity.player.PlayerEvent;
import net.neoforged.neoforge.event.entity.player.PlayerInteractEvent;
import net.neoforged.neoforge.event.level.BlockEvent;
import net.neoforged.neoforge.event.tick.PlayerTickEvent;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

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
        ItemStack fakeItemStack = tool.copy();
        boolean shouldTakeDamage = true;

        if (!level.isClientSide()) {

            event.setCanceled(true);

            //Silk Touch
            if (Boolean.TRUE.equals(tool.getComponents().get(CastingDataComponents.SILK_TOUCH.get()))) {
                fakeItemStack.enchant(toHolder(level, Enchantments.SILK_TOUCH), 1);
                drops = getLootDrops(state, pos, player, fakeItemStack, level);
            }

            //Fortune
            else if (Boolean.TRUE.equals(tool.getComponents().keySet().contains(CastingDataComponents.FORTUNE.get()))) {

                int fortuneLevel = tool.getComponents().getOrDefault(CastingDataComponents.FORTUNE.get(), 0);
                fakeItemStack.enchant(toHolder(level, Enchantments.FORTUNE), fortuneLevel);
                drops = getLootDrops(state, pos, player, fakeItemStack, level);

            }

            //Default Drops not Silk or Fortune
            if (drops.isEmpty()) {
                drops = getLootDrops(state, pos, player, fakeItemStack, level);
            }

            //Auto Smelt
            if (Boolean.TRUE.equals(tool.getComponents().keySet().contains(CastingDataComponents.AUTO_SMELT.get()))) {
                List<ItemStack> smeltingDrops = new ArrayList<>();

                for (ItemStack drop : drops) {

                    SingleRecipeInput container = new SingleRecipeInput(drop);
                    List<RecipeHolder<SmeltingRecipe>> smeltingRecipe = level.getRecipeManager().getRecipesFor(RecipeType.SMELTING, container, level);

                    if (!smeltingRecipe.isEmpty()) {
                        SmeltingRecipe recipe = smeltingRecipe.getFirst().value();
                        ItemStack result = recipe.getResultItem(level.registryAccess());

                        ItemStack smeltedStack = result.copy();
                        smeltedStack.setCount(drop.getCount());
                        smeltingDrops.add(smeltedStack);
                    } else {
                        smeltingDrops.add(drop);
                    }
                }

                drops = smeltingDrops;
            }


            //Drop Resources
            for (ItemStack drop : drops) {
                Block.popResource(level, pos, drop);
            }

            //Remove Block
            level.destroyBlock(pos, false, player);

            //Unbreaking check
            if (tool.getComponents().keySet().contains(CastingDataComponents.UNBREAKING.get())) {
                int unbreakingLevel = tool.getOrDefault(CastingDataComponents.UNBREAKING.get(), 0);
                shouldTakeDamage = level.getRandom().nextFloat() >= (unbreakingLevel * 0.1f);
            }

            //Actually damage the item
            if (shouldTakeDamage) {
                tool.hurtAndBreak(1, player, EquipmentSlot.MAINHAND);
            }
        }
    }

    public static List<ItemStack> getLootDrops(BlockState state, BlockPos pos, Player player, ItemStack tool, Level level) {
        LootParams.Builder lootParams = new LootParams.Builder((ServerLevel) level)
                .withParameter(LootContextParams.ORIGIN, Vec3.atCenterOf(pos))
                .withParameter(LootContextParams.TOOL, tool)
                .withParameter(LootContextParams.BLOCK_ENTITY, null)
                .withParameter(LootContextParams.THIS_ENTITY, player)
                .withParameter(LootContextParams.BLOCK_STATE, state);

        return state.getDrops(lootParams);
    }


    @SubscribeEvent
    public static void onBreakingBlock(PlayerEvent.BreakSpeed event) {
        Player player = event.getEntity();
        Level level = player.level();
        BlockState state = level.getBlockState(event.getPosition().orElse(BlockPos.ZERO));
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
                int repairingLevel = stack.getComponents().getOrDefault(CastingDataComponents.REPAIRING.get(), 0);
                int repairTickTime = getRepairTickTime(repairingLevel);

                if (event.getEntity().tickCount % repairTickTime == 0) {
                    int currentDamage = stack.getDamageValue();

                    if (currentDamage > 0) {
                        int newDamage = Math.max(currentDamage - 1, 0);
                        stack.setDamageValue(newDamage);
                    }
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

    @SubscribeEvent
    public static void onRightClick(PlayerInteractEvent.RightClickBlock event) {

        Level level = event.getLevel();
        Player player = event.getEntity();
        BlockPos pos = event.getPos();
        InteractionHand hand = event.getHand();
        ItemStack tool = player.getItemInHand(hand);

        if (!level.isClientSide()) {

            if (tool.getComponents().keySet().contains(CastingDataComponents.TORCH_PLACING.get())) {
                Direction face = event.getFace();
                assert face != null;
                BlockPos placePos = pos.relative(face);

                if (!level.getBlockState(placePos).canBeReplaced()) {
                    return;
                }

                if (face == Direction.UP) {
                    if (level.getBlockState(pos).isFaceSturdy(level, pos, Direction.UP)) {
                        level.setBlockAndUpdate(placePos, Blocks.TORCH.defaultBlockState());
                        tool.hurtAndBreak(1, player, EquipmentSlot.MAINHAND);
                    }
                } else if (face.getAxis().isHorizontal()) {
                    if (level.getBlockState(pos).isFaceSturdy(level, pos, face)) {
                        BlockState wallTorch = Blocks.WALL_TORCH.defaultBlockState()
                                .setValue(WallTorchBlock.FACING, face);
                        level.setBlockAndUpdate(placePos, wallTorch);
                        tool.hurtAndBreak(1, player, EquipmentSlot.MAINHAND);
                    }
                }
            }
        }
    }







    public static Holder<Enchantment> toHolder(Level level, ResourceKey<Enchantment> enchantment) {
        return level.registryAccess().registryOrThrow(Registries.ENCHANTMENT).getHolderOrThrow(enchantment);
    }

}
