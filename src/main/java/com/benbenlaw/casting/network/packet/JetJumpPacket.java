package com.benbenlaw.casting.network.packet;

import com.benbenlaw.casting.Casting;
import net.minecraft.network.RegistryFriendlyByteBuf;
import net.minecraft.network.codec.ByteBufCodecs;
import net.minecraft.network.codec.StreamCodec;
import net.minecraft.network.protocol.common.custom.CustomPacketPayload;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;
import net.neoforged.neoforge.network.handling.IPayloadHandler;

public record JetJumpPacket(boolean isJumping) implements CustomPacketPayload {

    public static final Type<JetJumpPacket> TYPE = new Type<>(ResourceLocation.fromNamespaceAndPath(Casting.MOD_ID, "jet_jump_packet"));

    public static final IPayloadHandler<JetJumpPacket> HANDLER = (pkt, ctx) -> {
        ServerPlayer player = (ServerPlayer) ctx.player();
        player.getPersistentData().putBoolean("casting_is_jumping", pkt.isJumping());
    };

    public static final StreamCodec<RegistryFriendlyByteBuf, JetJumpPacket> STREAM_CODEC = StreamCodec.composite(
            ByteBufCodecs.BOOL, JetJumpPacket::isJumping,
            JetJumpPacket::new
    );

    @Override
    public Type<? extends CustomPacketPayload> type() {
        return TYPE;
    }

}
