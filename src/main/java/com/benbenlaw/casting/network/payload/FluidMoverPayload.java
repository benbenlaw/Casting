package com.benbenlaw.casting.network.payload;

import com.benbenlaw.casting.Casting;
import net.minecraft.core.BlockPos;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.network.codec.ByteBufCodecs;
import net.minecraft.network.codec.StreamCodec;
import net.minecraft.network.protocol.common.custom.CustomPacketPayload;
import net.minecraft.resources.ResourceLocation;
@Deprecated(since = "2.0.0")
public record FluidMoverPayload(BlockPos blockPos, int tankID) implements CustomPacketPayload {

    public static final Type<FluidMoverPayload> TYPE = new Type<>(ResourceLocation.fromNamespaceAndPath(Casting.MOD_ID, "fluid_mover_payload"));

    @Override
    public Type<FluidMoverPayload> type() {
        return TYPE;
    }

    public static final StreamCodec<FriendlyByteBuf, FluidMoverPayload> STREAM_CODEC = StreamCodec.composite(
            BlockPos.STREAM_CODEC, FluidMoverPayload::blockPos,
            ByteBufCodecs.INT, FluidMoverPayload::tankID,
            FluidMoverPayload::new
    );
}
