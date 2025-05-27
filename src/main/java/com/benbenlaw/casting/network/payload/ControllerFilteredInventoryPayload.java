package com.benbenlaw.casting.network.payload;

import com.benbenlaw.casting.Casting;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Registry;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.network.RegistryFriendlyByteBuf;
import net.minecraft.network.codec.ByteBufCodecs;
import net.minecraft.network.codec.StreamCodec;
import net.minecraft.network.protocol.common.custom.CustomPacketPayload;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import org.jetbrains.annotations.NotNull;

public record ControllerFilteredInventoryPayload(int slot, ItemStack item, BlockPos controllerPos) implements CustomPacketPayload {

    public static final Type<ControllerFilteredInventoryPayload> TYPE = new Type<>(ResourceLocation.fromNamespaceAndPath(Casting.MOD_ID, "controller_filtered_inventory"));

    @Override
    public @NotNull Type<ControllerFilteredInventoryPayload> type() {
        return TYPE;
    }

    public static final StreamCodec<RegistryFriendlyByteBuf, ControllerFilteredInventoryPayload> STREAM_CODEC = StreamCodec.composite(
            ByteBufCodecs.INT, ControllerFilteredInventoryPayload::slot,
            ItemStack.OPTIONAL_STREAM_CODEC, ControllerFilteredInventoryPayload::item,
            BlockPos.STREAM_CODEC, ControllerFilteredInventoryPayload::controllerPos,
            ControllerFilteredInventoryPayload::new
    );

}
