package com.benbenlaw.casting.data;

import com.benbenlaw.casting.block.CastingBlocks;
import it.unimi.dsi.fastutil.objects.ReferenceOpenHashSet;
import net.minecraft.core.HolderLookup;
import net.minecraft.data.loot.packs.VanillaBlockLoot;
import net.minecraft.world.item.Items;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.storage.loot.LootTable;
import org.jetbrains.annotations.NotNull;

import java.util.Set;

public class CastingLootTableProvider extends VanillaBlockLoot {

    public CastingLootTableProvider(HolderLookup.Provider p_344962_) {
        super(p_344962_);
    }
    @Override
    protected void generate() {

        this.dropSelf(CastingBlocks.BLACK_BRICKS.get());
        this.dropSelf(CastingBlocks.MULTIBLOCK_CONTROLLER.get());
        this.dropOther(CastingBlocks.MULTIBLOCK_FUEL_TANK.get(), Items.AIR);
        this.dropOther(CastingBlocks.MULTIBLOCK_COOLANT_TANK.get(), Items.AIR);
        this.dropOther(CastingBlocks.MULTIBLOCK_SOLIDIFIER.get(), Items.AIR);
        this.dropOther(CastingBlocks.MULTIBLOCK_VALVE.get(), Items.AIR);
        this.dropOther(CastingBlocks.MULTIBLOCK_MIXER.get(), Items.AIR);
        this.dropSelf(CastingBlocks.BLACK_BRICK_GLASS.get());

        //OG Casting
        this.dropOther(CastingBlocks.CONTROLLER.get(), Items.AIR);
        this.dropOther(CastingBlocks.TANK.get(), Items.AIR);
        this.dropOther(CastingBlocks.SOLIDIFIER.get(), Items.AIR);
        this.dropOther(CastingBlocks.MIXER.get(), Items.AIR);
        this.dropSelf(CastingBlocks.MIXER_WHISK.get());
        this.dropOther(CastingBlocks.EQUIPMENT_MODIFIER.get(), Items.AIR);

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
