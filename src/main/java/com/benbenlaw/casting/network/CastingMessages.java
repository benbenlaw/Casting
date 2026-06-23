package com.benbenlaw.casting.network;

import com.benbenlaw.casting.Casting;
import com.benbenlaw.casting.network.packet.ChangeFluidMangerSelectedFluidPacket;
import com.benbenlaw.casting.network.packet.ChangeMoldPagePacket;
import net.neoforged.neoforge.network.event.RegisterPayloadHandlersEvent;
import net.neoforged.neoforge.network.registration.PayloadRegistrar;

public class CastingMessages {

    public static void registerNetworking(final RegisterPayloadHandlersEvent event) {
        final PayloadRegistrar registrar = event.registrar(Casting.MOD_ID);

        //Client -> Server
        registrar.playToServer(ChangeMoldPagePacket.TYPE, ChangeMoldPagePacket.STREAM_CODEC, ChangeMoldPagePacket.HANDLER);
        registrar.playToServer(ChangeFluidMangerSelectedFluidPacket.TYPE, ChangeFluidMangerSelectedFluidPacket.STREAM_CODEC, ChangeFluidMangerSelectedFluidPacket.HANDLER);
    }
}
