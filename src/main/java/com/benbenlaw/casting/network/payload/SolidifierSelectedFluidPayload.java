package com.benbenlaw.casting.network.payload;

import com.benbenlaw.casting.Casting;
import net.minecraft.core.BlockPos;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.network.codec.ByteBufCodecs;
import net.minecraft.network.codec.StreamCodec;
import net.minecraft.network.protocol.common.custom.CustomPacketPayload;
import net.minecraft.resources.ResourceLocation;
import org.jetbrains.annotations.NotNull;

public record SolidifierSelectedFluidPayload(String fluid, BlockPos pos) implements CustomPacketPayload {

    public static final Type<SolidifierSelectedFluidPayload> TYPE = new Type<>(ResourceLocation.fromNamespaceAndPath(Casting.MOD_ID, "solidifier_selected_fluid"));

    @Override
    public @NotNull Type<SolidifierSelectedFluidPayload> type() {
        return TYPE;
    }

    public static final StreamCodec<FriendlyByteBuf, SolidifierSelectedFluidPayload> STREAM_CODEC = StreamCodec.composite(
            ByteBufCodecs.STRING_UTF8, SolidifierSelectedFluidPayload::fluid,
            BlockPos.STREAM_CODEC, SolidifierSelectedFluidPayload::pos,
            SolidifierSelectedFluidPayload::new
    );
}
