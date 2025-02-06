package com.benbenlaw.casting.networking;

import com.benbenlaw.casting.Casting;
import com.benbenlaw.casting.networking.packets.ClearTankPacket;
import com.benbenlaw.casting.networking.packets.FluidMoverPacket;
import com.benbenlaw.casting.networking.packets.LockSolidifierPacket;
import com.benbenlaw.casting.networking.payload.ClearTankPayload;
import com.benbenlaw.casting.networking.payload.FluidMoverPayload;
import com.benbenlaw.casting.networking.payload.LockSolidifierPayload;
import net.neoforged.neoforge.network.event.RegisterPayloadHandlersEvent;
import net.neoforged.neoforge.network.registration.PayloadRegistrar;

public class CastingModMessages {


    public static void registerNetworking(final RegisterPayloadHandlersEvent event) {
        final PayloadRegistrar registrar = event.registrar(Casting.MOD_ID);

        //To Server From Client
        registrar.playToServer(ClearTankPayload.TYPE, ClearTankPayload.STREAM_CODEC, ClearTankPacket.get()::handle);
        registrar.playToServer(FluidMoverPayload.TYPE, FluidMoverPayload.STREAM_CODEC, FluidMoverPacket.get()::handle);
        registrar.playToServer(LockSolidifierPayload.TYPE, LockSolidifierPayload.STREAM_CODEC, LockSolidifierPacket.get()::handle);
    }

}
