package com.benbenlaw.casting.network.payload;

import com.benbenlaw.casting.Casting;
import net.minecraft.core.BlockPos;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.network.codec.ByteBufCodecs;
import net.minecraft.network.codec.StreamCodec;
import net.minecraft.network.protocol.common.custom.CustomPacketPayload;
import net.minecraft.resources.ResourceLocation;
@Deprecated(since = "2.0.0")
public record ClearTankPayload(BlockPos blockPos, boolean isShiftDown, int tankID) implements CustomPacketPayload {

    public static final Type<ClearTankPayload> TYPE = new Type<>(ResourceLocation.fromNamespaceAndPath(Casting.MOD_ID, "clear_tank"));

    @Override
    public Type<ClearTankPayload> type() {
        return TYPE;
    }

    public static final StreamCodec<FriendlyByteBuf, ClearTankPayload> STREAM_CODEC = StreamCodec.composite(
            BlockPos.STREAM_CODEC, ClearTankPayload::blockPos,
            ByteBufCodecs.BOOL, ClearTankPayload::isShiftDown,
            ByteBufCodecs.INT, ClearTankPayload::tankID,
            ClearTankPayload::new
    );
}
