package com.benbenlaw.casting.network.payload;

import com.benbenlaw.casting.Casting;
import net.minecraft.core.BlockPos;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.network.codec.ByteBufCodecs;
import net.minecraft.network.codec.StreamCodec;
import net.minecraft.network.protocol.common.custom.CustomPacketPayload;
import net.minecraft.resources.ResourceLocation;
import org.jetbrains.annotations.NotNull;

public record ToggleArmorModifiersPayload(int armorSlot) implements CustomPacketPayload {

    public static final Type<ToggleArmorModifiersPayload> TYPE = new Type<>(ResourceLocation.fromNamespaceAndPath(Casting.MOD_ID, "armor_modifiers_toggle"));

    @Override
    public @NotNull Type<ToggleArmorModifiersPayload> type() {
        return TYPE;
    }

    public static final StreamCodec<FriendlyByteBuf, ToggleArmorModifiersPayload> STREAM_CODEC = StreamCodec.composite(
            ByteBufCodecs.INT, ToggleArmorModifiersPayload::armorSlot,
            ToggleArmorModifiersPayload::new
    );
}
