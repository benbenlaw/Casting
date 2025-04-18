package com.benbenlaw.casting.event;

import com.benbenlaw.casting.Casting;
import com.benbenlaw.casting.config.ToolModifierConfig;
import com.benbenlaw.casting.item.CastingDataComponents;
import com.benbenlaw.casting.util.BeheadingHeadMap;
import com.benbenlaw.casting.util.CastingTags;
import com.benbenlaw.core.util.FakePlayerUtil;
import com.mojang.authlib.GameProfile;
import net.minecraft.advancements.critereon.EntityPredicate;
import net.minecraft.advancements.critereon.EntityTypePredicate;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.core.Holder;
import net.minecraft.core.Vec3i;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.core.registries.Registries;
import net.minecraft.resources.ResourceKey;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.SimpleContainer;
import net.minecraft.world.damagesource.DamageSource;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.EquipmentSlot;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.item.ItemEntity;
import net.minecraft.world.entity.monster.Enemy;
import net.minecraft.world.entity.monster.Monster;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.*;
import net.minecraft.world.item.crafting.*;
import net.minecraft.world.item.enchantment.*;
import net.minecraft.world.item.enchantment.effects.AddValue;
import net.minecraft.world.level.ClipContext;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.*;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.storage.loot.LootContext;
import net.minecraft.world.level.storage.loot.LootParams;
import net.minecraft.world.level.storage.loot.LootTable;
import net.minecraft.world.level.storage.loot.parameters.LootContextParamSets;
import net.minecraft.world.level.storage.loot.parameters.LootContextParams;
import net.minecraft.world.level.storage.loot.predicates.LootItemEntityPropertyCondition;
import net.minecraft.world.phys.BlockHitResult;
import net.minecraft.world.phys.Vec3;
import net.neoforged.bus.api.SubscribeEvent;
import net.neoforged.fml.common.EventBusSubscriber;
import net.neoforged.neoforge.common.util.FakePlayer;
import net.neoforged.neoforge.common.util.RecipeMatcher;
import net.neoforged.neoforge.event.entity.EntityEvent;
import net.neoforged.neoforge.event.entity.living.LivingDamageEvent;
import net.neoforged.neoforge.event.entity.living.LivingDeathEvent;
import net.neoforged.neoforge.event.entity.living.LivingDropsEvent;
import net.neoforged.neoforge.event.entity.player.PlayerEvent;
import net.neoforged.neoforge.event.entity.player.PlayerInteractEvent;
import net.neoforged.neoforge.event.level.BlockEvent;
import net.neoforged.neoforge.event.tick.PlayerTickEvent;
import org.jetbrains.annotations.NotNull;

import java.util.*;

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

        boolean isSilkTouch = tool.getComponents().keySet().contains(CastingDataComponents.SILK_TOUCH.get());
        boolean isFortune = tool.getComponents().keySet().contains(CastingDataComponents.FORTUNE.get());
        boolean isAutoSmelt = tool.getComponents().keySet().contains(CastingDataComponents.AUTO_SMELT.get());
        boolean isUnbreaking = tool.getComponents().keySet().contains(CastingDataComponents.UNBREAKING.get());
        boolean isExcavation = tool.getComponents().keySet().contains(CastingDataComponents.EXCAVATION.get());

        boolean requiresCastingOverrides = isExcavation || isSilkTouch || isFortune || isAutoSmelt || isUnbreaking;

        if (!level.isClientSide() && requiresCastingOverrides) {
            event.setCanceled(true);

            //Excavation
            if (isExcavation) {
                int excavationLevel = tool.getComponents().getOrDefault(CastingDataComponents.EXCAVATION.get(), 0);
                breakArea(event, player, tool, level, pos, state, excavationLevel);
                return;
            }

            //Silk Touch
            if (isSilkTouch) {
                fakeItemStack.enchant(toHolder(level, Enchantments.SILK_TOUCH), 1);
                drops = getLootDrops(state, pos, player, fakeItemStack, level);
            }

            //Fortune
            else if (isFortune) {

                int fortuneLevel = tool.getComponents().getOrDefault(CastingDataComponents.FORTUNE.get(), 0);
                fakeItemStack.enchant(toHolder(level, Enchantments.FORTUNE), fortuneLevel);
                drops = getLootDrops(state, pos, player, fakeItemStack, level);

            }

            //Default Drops not Silk or Fortune
            if (drops.isEmpty()) {
                drops = getLootDrops(state, pos, player, fakeItemStack, level);
            }

            //Auto Smelt
            if (isAutoSmelt) {
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
            if (isUnbreaking) {
                int unbreakingLevel = tool.getOrDefault(CastingDataComponents.UNBREAKING.get(), 0);
                shouldTakeDamage = level.getRandom().nextFloat() >= (unbreakingLevel * 0.1f);
            }

            //Actually damage the item
            if (shouldTakeDamage) {
                tool.hurtAndBreak(1, player, EquipmentSlot.MAINHAND);
            }
        }
    }


    //Get loot drops from a block
    public static List<ItemStack> getLootDrops(BlockState state, BlockPos pos, Player player, ItemStack tool, Level level) {
        LootParams.Builder lootParams = new LootParams.Builder((ServerLevel) level)
                .withParameter(LootContextParams.ORIGIN, Vec3.atCenterOf(pos))
                .withParameter(LootContextParams.TOOL, tool)
                .withParameter(LootContextParams.BLOCK_ENTITY, null)
                .withParameter(LootContextParams.THIS_ENTITY, player)
                .withParameter(LootContextParams.BLOCK_STATE, state);

        return state.getDrops(lootParams);
    }

    //Using this to break an area of blocks
    public static void breakArea(BlockEvent.BreakEvent event, Player player, ItemStack tool, Level level, BlockPos origin, BlockState originalState, int excavationLevel) {
        int radius = excavationLevel;
        Vec3 lookVec = player.getLookAngle();
        Direction face = Direction.getNearest(lookVec.x, lookVec.y, lookVec.z);

        for (int dx = -radius; dx <= radius; dx++) {
            for (int dy = -radius; dy <= radius; dy++) {
                BlockPos targetPos;

                if (face.getAxis() == Direction.Axis.Y) {
                    targetPos = origin.offset(dx, 0, dy);
                } else if (face.getAxis() == Direction.Axis.X) {
                    targetPos = origin.offset(0, dx, dy);
                } else {
                    targetPos = origin.offset(dx, dy, 0);
                }

                BlockState targetState = level.getBlockState(targetPos);
                if (targetState.isAir() || targetState.getDestroySpeed(level, targetPos) < 0) continue;

                if (!tool.isCorrectToolForDrops(targetState)) {
                    continue;
                }

                ItemStack fakeItemStack = tool.copy();
                List<ItemStack> drops;

                if (Boolean.TRUE.equals(tool.getComponents().get(CastingDataComponents.SILK_TOUCH.get()))) {
                    fakeItemStack.enchant(toHolder(level, Enchantments.SILK_TOUCH), 1);
                } else if (tool.getComponents().keySet().contains(CastingDataComponents.FORTUNE.get())) {
                    int fortuneLevel = tool.getOrDefault(CastingDataComponents.FORTUNE.get(), 0);
                    fakeItemStack.enchant(toHolder(level, Enchantments.FORTUNE), fortuneLevel);
                }

                drops = getLootDrops(targetState, targetPos, player, fakeItemStack, level);

                if (Boolean.TRUE.equals(tool.getComponents().get(CastingDataComponents.AUTO_SMELT.get()))) {
                    List<ItemStack> smelted = new ArrayList<>();
                    for (ItemStack drop : drops) {
                        SingleRecipeInput container = new SingleRecipeInput(drop);
                        List<RecipeHolder<SmeltingRecipe>> recipes = level.getRecipeManager().getRecipesFor(RecipeType.SMELTING, container, level);
                        if (!recipes.isEmpty()) {
                            ItemStack result = recipes.getFirst().value().getResultItem(level.registryAccess()).copy();
                            result.setCount(drop.getCount());
                            smelted.add(result);
                        } else {
                            smelted.add(drop);
                        }
                    }
                    drops = smelted;
                }

                for (ItemStack drop : drops) {
                    Block.popResource(level, targetPos, drop);
                }

                level.destroyBlock(targetPos, false, player);

                boolean shouldTakeDamage = true;
                if (tool.getComponents().keySet().contains(CastingDataComponents.UNBREAKING.get())) {
                    int unbreakingLevel = tool.getOrDefault(CastingDataComponents.UNBREAKING.get(), 0);
                    shouldTakeDamage = level.getRandom().nextFloat() >= (unbreakingLevel * 0.1f);
                }

                if (shouldTakeDamage) {
                    tool.hurtAndBreak(1, player, EquipmentSlot.MAINHAND);
                }
            }
        }
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

            //Repairing
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

    @SubscribeEvent
    public static void onPreLivingDamage(LivingDamageEvent.Pre event) {
        Level level = event.getEntity().level();
        Entity target = event.getEntity();
        Entity sourceEntity = event.getSource().getEntity();
        if (!(sourceEntity instanceof Player attacker)) return;
        ItemStack weapon = attacker.getMainHandItem();
        if (weapon.isEmpty()) return;

        boolean isSharpness = weapon.getComponents().keySet().contains(CastingDataComponents.SHARPNESS.get());
        boolean isLifesteal = weapon.getComponents().keySet().contains(CastingDataComponents.LIFESTEAL.get());
        boolean isKnockback = weapon.getComponents().keySet().contains(CastingDataComponents.KNOCKBACK.get());
        boolean isIgnite = weapon.getComponents().keySet().contains(CastingDataComponents.IGNITE.get());

        boolean requiresCastingOverrides = isIgnite || isSharpness || isLifesteal || isKnockback;

        if (!level.isClientSide() && requiresCastingOverrides) {

            //Sharpness
            if (isSharpness) {
                int sharpnessLevel = weapon.getComponents().getOrDefault(CastingDataComponents.SHARPNESS.get(), 0);
                if (sharpnessLevel <= 0) return;

                // Vanilla is: 0.5 * level + 0.5
                float bonusDamage = ToolModifierConfig.additionalMultiplierForSharpness.get() * sharpnessLevel + ToolModifierConfig.additionalAdditionForSharpness.get();
                event.setNewDamage(event.getNewDamage() + bonusDamage);
            }

            //Lifesteal
            if (isLifesteal) {
                if (target instanceof Player) return;
                if (target.isInvulnerable()) return;

                if (target instanceof Enemy) {
                    int lifestealLevel = weapon.getComponents().getOrDefault(CastingDataComponents.LIFESTEAL.get(), 0);
                    int healingAmount = (int) (event.getNewDamage() * 0.1f * lifestealLevel);
                    if (healingAmount > 0) {
                        attacker.heal(healingAmount);
                    }
                }
            }

            //Knockback
            if (isKnockback) {
                int knockbackLevel = weapon.getComponents().getOrDefault(CastingDataComponents.KNOCKBACK.get(), 0);
                if (knockbackLevel > 0) {
                    double dx = target.getX() - attacker.getX();
                    double dz = target.getZ() - attacker.getZ();
                    double distance = Math.sqrt(dx * dx + dz * dz);
                    if (distance > 0) {
                        dx /= distance;
                        dz /= distance;
                        target.setDeltaMovement(
                                target.getDeltaMovement().add(dx * (double) knockbackLevel, 0.1, dz * (double) knockbackLevel)
                        );
                        target.hurtMarked = true;
                    }
                }
            }

            //Ignite
            if (isIgnite) {
                int igniteLevel = weapon.getComponents().getOrDefault(CastingDataComponents.IGNITE.get(), 0);
                target.igniteForSeconds(1 + igniteLevel);

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

    @SubscribeEvent
    public static void onEntityDrops(LivingDropsEvent event) {

        Level level = event.getEntity().level();
        LivingEntity deadEntity = event.getEntity();
        DamageSource damageSource = event.getSource();
        Entity killer = event.getEntity().getKillCredit();

        if (killer instanceof Player) {

            ItemStack stack = killer.getWeaponItem();
            if (stack.isEmpty()) return;

            List<ItemStack> loot = new ArrayList<>();

            boolean isLooting = stack.getComponents().keySet().contains(CastingDataComponents.LOOTING.get());
            boolean isBeheading = stack.getComponents().keySet().contains(CastingDataComponents.BEHEADING.get());

            boolean requiresCastingOverrides = isLooting || isBeheading;

            if (!level.isClientSide() && requiresCastingOverrides) {

                //Looting
                if (isLooting) {
                    event.setCanceled(true);
                    LootTable lootTable = Objects.requireNonNull(level.getServer()).reloadableRegistries().getLootTable(deadEntity.getLootTable());
                    int lootingLevel = stack.getComponents().getOrDefault(CastingDataComponents.LOOTING.get(), 0);
                    ItemStack fakeItemStack = stack.copy();
                    fakeItemStack.enchant(toHolder(level, Enchantments.LOOTING), lootingLevel);

                    loot = getMobLootDrops(deadEntity, (Player) killer, damageSource, fakeItemStack, lootTable, level);
                }

                //Beheading
                if (isBeheading) {
                    Optional<ItemStack> customHead = BeheadingHeadMap.getHeadForEntity(deadEntity);
                    customHead.ifPresent(loot::add);
                }

                for (ItemStack itemStack : loot) {
                    popOutTheItem(level, deadEntity.blockPosition(), itemStack);
                }
            }
        }
    }

    public static List<ItemStack> getMobLootDrops(Entity deadEntity, Player player, DamageSource damageSource, ItemStack stack,
                                                  LootTable lootTable, Level level) {

        FakePlayer fakePlayer = FakePlayerUtil.createFakePlayer((ServerLevel) level, "FakePlayerForLooting");
        fakePlayer.setItemInHand(InteractionHand.MAIN_HAND, stack);

        LootParams.Builder lootParams = new LootParams.Builder((ServerLevel) level)
                .withParameter(LootContextParams.THIS_ENTITY, deadEntity)
                .withParameter(LootContextParams.ORIGIN, Vec3.atCenterOf(deadEntity.getOnPos()))
                .withParameter(LootContextParams.DAMAGE_SOURCE, damageSource)
                .withParameter(LootContextParams.ATTACKING_ENTITY, fakePlayer);

        LootParams lootParamsFinal = lootParams.create(LootContextParamSets.ENTITY);
        return lootTable.getRandomItems(lootParamsFinal);
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
