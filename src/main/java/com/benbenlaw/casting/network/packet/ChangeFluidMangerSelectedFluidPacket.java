package com.benbenlaw.casting.network.packet;

import com.benbenlaw.casting.Casting;
import com.benbenlaw.casting.item.CastingDataComponents;
import com.benbenlaw.casting.item.FluidMoverItem;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.network.codec.ByteBufCodecs;
import net.minecraft.network.codec.StreamCodec;
import net.minecraft.network.protocol.common.custom.CustomPacketPayload;
import net.minecraft.resources.Identifier;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.item.ItemStack;
import net.neoforged.neoforge.network.handling.IPayloadContext;
import net.neoforged.neoforge.network.handling.IPayloadHandler;
import org.jetbrains.annotations.NotNull;

public record ChangeFluidMangerSelectedFluidPacket(int selectedFluid) implements CustomPacketPayload {

    public static final Type<ChangeFluidMangerSelectedFluidPacket> TYPE = new Type<>(Identifier.fromNamespaceAndPath(Casting.MOD_ID, "change_fluid_manager_selected_fluid"));


    public static final StreamCodec<FriendlyByteBuf, ChangeFluidMangerSelectedFluidPacket> STREAM_CODEC = StreamCodec.composite(
            ByteBufCodecs.INT, ChangeFluidMangerSelectedFluidPacket::selectedFluid,
            ChangeFluidMangerSelectedFluidPacket::new
    );

    public static final IPayloadHandler<ChangeFluidMangerSelectedFluidPacket> HANDLER = (packet, context) -> {
        context.enqueueWork(() -> {
            ServerPlayer player = (ServerPlayer) context.player();
            ItemStack item = player.getMainHandItem();
            if (item.getItem() instanceof FluidMoverItem) {
                item.set(CastingDataComponents.FLUID_MANAGER_SELECTED_FLUID, packet.selectedFluid);
            }
        });


    };

    @Override
    public @NotNull Type<? extends CustomPacketPayload> type() {
        return TYPE;
    }
}

