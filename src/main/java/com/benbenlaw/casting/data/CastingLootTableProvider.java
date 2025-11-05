package com.benbenlaw.casting.data;

import com.benbenlaw.casting.block.CastingBlocks;
import com.benbenlaw.casting.item.CastingDataComponents;
import it.unimi.dsi.fastutil.objects.ReferenceOpenHashSet;
import net.minecraft.core.HolderLookup;
import net.minecraft.data.loot.packs.VanillaBlockLoot;
import net.minecraft.network.chat.FormattedText;
import net.minecraft.world.item.Items;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.levelgen.structure.pieces.StructurePieceType;
import net.minecraft.world.level.storage.loot.LootPool;
import net.minecraft.world.level.storage.loot.LootTable;
import net.minecraft.world.level.storage.loot.entries.LootItem;
import net.minecraft.world.level.storage.loot.functions.CopyComponentsFunction;
import net.minecraft.world.level.storage.loot.parameters.LootContextParamSets;
import net.minecraft.world.level.storage.loot.parameters.LootContextParams;
import net.minecraft.world.level.storage.loot.providers.number.ConstantValue;
import org.jetbrains.annotations.NotNull;

import javax.naming.Context;
import java.util.Set;

public class CastingLootTableProvider extends VanillaBlockLoot {

    public CastingLootTableProvider(HolderLookup.Provider p_344962_) {
        super(p_344962_);
    }
    @Override
    protected void generate() {

        this.dropSelf(CastingBlocks.BLACK_BRICKS.get());
        dropWithFluidComponent(CastingBlocks.MULTIBLOCK_CONTROLLER.get());
        dropWithFluidComponent(CastingBlocks.MULTIBLOCK_FUEL_TANK.get());
        dropWithFluidComponent(CastingBlocks.MULTIBLOCK_COOLANT_TANK.get());
        this.dropSelf(CastingBlocks.MULTIBLOCK_SOLIDIFIER.get());
        this.dropSelf(CastingBlocks.MULTIBLOCK_VALVE.get());
        this.dropSelf(CastingBlocks.MULTIBLOCK_MIXER.get());
        this.dropSelf(CastingBlocks.BLACK_BRICK_GLASS.get());
        this.dropSelf(CastingBlocks.MULTIBLOCK_REGULATOR.get());

        //OG Casting
        dropWithFluidComponent(CastingBlocks.CONTROLLER.get());
        dropWithFluidComponent(CastingBlocks.SOLIDIFIER.get());
        dropWithFluidComponent(CastingBlocks.MIXER.get());
        dropWithFluidComponent(CastingBlocks.TANK.get());
        dropWithFluidComponent(CastingBlocks.EQUIPMENT_MODIFIER.get());

    }

    private void dropWithFluidComponent(Block block) {
        this.add(block, LootTable.lootTable()
                .withPool(LootPool.lootPool()
                        .setRolls(ConstantValue.exactly(1))
                        .add(LootItem.lootTableItem(block)
                                .apply(CopyComponentsFunction.copyComponentsFromBlockEntity(LootContextParams.BLOCK_ENTITY)
                                        .include(CastingDataComponents.FLUIDS.get())))));
    }


    @Override
    protected void add(@NotNull Block block, @NotNull LootTable.Builder table) {
        //Overwrite the core register method to add to our list of known blocks
        super.add(block, table);
        knownBlocks.add(block);
    }
    private final Set<Block> knownBlocks = new ReferenceOpenHashSet<>();

    @NotNull
    @Override
    protected Iterable<Block> getKnownBlocks() {
        return knownBlocks;
    }
}
