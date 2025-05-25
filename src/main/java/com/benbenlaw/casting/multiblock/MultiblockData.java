package com.benbenlaw.casting.multiblock;

import com.mojang.datafixers.util.Pair;
import net.minecraft.core.BlockPos;
import net.minecraft.core.HolderLookup;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.Tag;
import net.neoforged.neoforge.common.util.INBTSerializable;

import java.util.ArrayList;
import java.util.List;
//Inspired from productive lib multiblock data

public final class MultiblockData implements INBTSerializable
    {
        private BlockPos controllerPos;
        private Pair<BlockPos, BlockPos> topCorners;
        private List<BlockPos> multiblockExtraBlocks;
        private int height;
        private int volume;

        public MultiblockData(BlockPos controllerPos, Pair<BlockPos, BlockPos> topCorners, List<BlockPos> extraBlocks, int height, int volume) {
            this.controllerPos = controllerPos;
            this.topCorners = topCorners;
            this.multiblockExtraBlocks = extraBlocks;
            this.height = height;
            this.volume = volume;
        }

        @Override
        public Tag serializeNBT(HolderLookup.Provider provider) {
            CompoundTag tag = new CompoundTag();
            tag.putInt("height", height());
            tag.putInt("volume", volume());
            tag.putLong("controller", controllerPos().asLong());
            tag.putLong("corner1", topCorners().getFirst().asLong());
            tag.putLong("corner2", topCorners().getSecond().asLong());
            tag.putInt("extraBlocksCount", extraBlocks().size());
            int i = 0;
            for (BlockPos blockPos : extraBlocks()) {
                tag.putLong("e" + i, blockPos.asLong());
                i++;
            }
            return tag;
        }

        @Override
        public void deserializeNBT(HolderLookup.Provider provider, Tag tag) {
            if (tag instanceof CompoundTag compoundTag) {
                this.height = compoundTag.getInt("height");
                this.volume = compoundTag.getInt("volume");
                this.controllerPos = BlockPos.of(compoundTag.getLong("controller"));
                this.topCorners = Pair.of(BlockPos.of(compoundTag.getLong("corner1")), BlockPos.of(compoundTag.getLong("corner2")));
                List<BlockPos> ps = new ArrayList<>();
                for (int i = 0; i < compoundTag.getInt("extraBlocksCount"); i++) {
                    ps.add(BlockPos.of(compoundTag.getLong("e" + i)));
                }
                this.multiblockExtraBlocks = ps;
            }
        }

        public BlockPos controllerPos() {
            return controllerPos;
        }

        public Pair<BlockPos, BlockPos> topCorners() {
            return topCorners;
        }

        public List<BlockPos> extraBlocks() {
            return multiblockExtraBlocks;
        }

        public int height() {
            return height;
        }

        public int volume() {
            return volume;
        }
    }