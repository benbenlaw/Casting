package com.benbenlaw.casting.network.payload;

import com.benbenlaw.casting.Casting;
import net.minecraft.core.BlockPos;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.network.codec.ByteBufCodecs;
import net.minecraft.network.codec.StreamCodec;
import net.minecraft.network.protocol.common.custom.CustomPacketPayload;
import net.minecraft.resources.ResourceLocation;
import org.jetbrains.annotations.NotNull;

public record MixerSelectedFluidPayload(String fluid, BlockPos pos) implements CustomPacketPayload {

    public static final Type<MixerSelectedFluidPayload> TYPE = new Type<>(ResourceLocation.fromNamespaceAndPath(Casting.MOD_ID, "mixer_selected_fluid"));

    @Override
    public @NotNull Type<MixerSelectedFluidPayload> type() {
        return TYPE;
    }

    public static final StreamCodec<FriendlyByteBuf, MixerSelectedFluidPayload> STREAM_CODEC = StreamCodec.composite(
            ByteBufCodecs.STRING_UTF8, MixerSelectedFluidPayload::fluid,
            BlockPos.STREAM_CODEC, MixerSelectedFluidPayload::pos,
            MixerSelectedFluidPayload::new
    );
}
