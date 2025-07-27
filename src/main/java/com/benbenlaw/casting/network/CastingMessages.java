package com.benbenlaw.casting.network;

import com.benbenlaw.casting.Casting;
import com.benbenlaw.casting.network.packet.*;
import com.benbenlaw.casting.network.payload.*;
import net.neoforged.neoforge.network.event.RegisterPayloadHandlersEvent;
import net.neoforged.neoforge.network.registration.PayloadRegistrar;

public class CastingMessages {


    public static void registerNetworking(final RegisterPayloadHandlersEvent event) {
        final PayloadRegistrar registrar = event.registrar(Casting.MOD_ID);

        //To Server From Client
        registrar.playToServer(SolidifierSelectedFluidPayload.TYPE, SolidifierSelectedFluidPayload.STREAM_CODEC, SolidifierSelectedFluidPacket.get()::handle);
        registrar.playToServer(ValveSelectedFluidPayload.TYPE, ValveSelectedFluidPayload.STREAM_CODEC, ValveSelectedFluidPacket.get()::handle);
        registrar.playToServer(MixerSelectedFluidPayload.TYPE, MixerSelectedFluidPayload.STREAM_CODEC, MixerSelectedFluidPacket.get()::handle);
        registrar.playToServer(OnOffButtonPayload.TYPE, OnOffButtonPayload.STREAM_CODEC, OnOffButtonPacket.get()::handle);
        registrar.playToServer(ControllerFilteredInventoryPayload.TYPE, ControllerFilteredInventoryPayload.STREAM_CODEC, ControllerFilteredInventoryPacket.get()::handle);
        registrar.playToServer(ToggleArmorModifiersPayload.TYPE, ToggleArmorModifiersPayload.STREAM_CODEC, ToggleArmorModifiersPacket.get()::handle);
        registrar.playToServer(JetJumpPacket.TYPE, JetJumpPacket.STREAM_CODEC, JetJumpPacket.HANDLER);

        //OG Casting
        //To Server From Client
        registrar.playToServer(ClearTankPayload.TYPE, ClearTankPayload.STREAM_CODEC, ClearTankPacket.get()::handle);
        registrar.playToServer(FluidMoverPayload.TYPE, FluidMoverPayload.STREAM_CODEC, FluidMoverPacket.get()::handle);
        registrar.playToServer(LockSolidifierPayload.TYPE, LockSolidifierPayload.STREAM_CODEC, LockSolidifierPacket.get()::handle);

    }

}
