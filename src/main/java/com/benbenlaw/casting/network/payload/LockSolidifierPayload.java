package com.benbenlaw.casting.network.payload;

import com.benbenlaw.casting.Casting;
import net.minecraft.core.BlockPos;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.network.codec.StreamCodec;
import net.minecraft.network.protocol.common.custom.CustomPacketPayload;
import net.minecraft.resources.ResourceLocation;
@Deprecated(since = "2.0.0")
public record LockSolidifierPayload(BlockPos blockPos) implements CustomPacketPayload {

    public static final Type<LockSolidifierPayload> TYPE = new Type<>(ResourceLocation.fromNamespaceAndPath(Casting.MOD_ID, "lock_solidifier"));

    @Override
    public Type<LockSolidifierPayload> type() {
        return TYPE;
    }

    public static final StreamCodec<FriendlyByteBuf, LockSolidifierPayload> STREAM_CODEC = StreamCodec.composite(
            BlockPos.STREAM_CODEC, LockSolidifierPayload::blockPos,
            LockSolidifierPayload::new
    );
}
