package com.benbenlaw.casting.data;

import com.benbenlaw.casting.block.ModBlocks;
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


        this.dropOther(ModBlocks.CONTROLLER.get(), Items.AIR);
        this.dropOther(ModBlocks.TANK.get(), Items.AIR);
        this.dropOther(ModBlocks.SOLIDIFIER.get(), Items.AIR);
        this.dropSelf(ModBlocks.BLACK_BRICKS.get());
        this.dropOther(ModBlocks.MIXER.get(), Items.AIR);
        this.dropSelf(ModBlocks.MIXER_WHISK.get());
        this.dropOther(ModBlocks.TOOL_MODIFIER.get(), Items.AIR);

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
