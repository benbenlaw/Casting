package com.benbenlaw.casting.network.payload;

import com.benbenlaw.casting.Casting;
import net.minecraft.core.BlockPos;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.network.codec.ByteBufCodecs;
import net.minecraft.network.codec.StreamCodec;
import net.minecraft.network.protocol.common.custom.CustomPacketPayload;
import net.minecraft.resources.ResourceLocation;
import org.jetbrains.annotations.NotNull;

public record ValveSelectedFluidPayload(String fluid, BlockPos pos) implements CustomPacketPayload {

    public static final Type<ValveSelectedFluidPayload> TYPE = new Type<>(ResourceLocation.fromNamespaceAndPath(Casting.MOD_ID, "valve_selected_fluid"));

    @Override
    public @NotNull Type<ValveSelectedFluidPayload> type() {
        return TYPE;
    }

    public static final StreamCodec<FriendlyByteBuf, ValveSelectedFluidPayload> STREAM_CODEC = StreamCodec.composite(
            ByteBufCodecs.STRING_UTF8, ValveSelectedFluidPayload::fluid,
            BlockPos.STREAM_CODEC, ValveSelectedFluidPayload::pos,
            ValveSelectedFluidPayload::new
    );
}
