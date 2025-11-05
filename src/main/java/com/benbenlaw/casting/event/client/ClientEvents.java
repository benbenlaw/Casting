package com.benbenlaw.casting.event.client;

import com.benbenlaw.casting.Casting;
import com.benbenlaw.casting.network.packet.JetJumpPacket;
import com.benbenlaw.casting.network.packet.ToggleArmorModifiersPacket;
import com.benbenlaw.casting.network.payload.MixerSelectedFluidPayload;
import com.benbenlaw.casting.network.payload.ToggleArmorModifiersPayload;
import com.benbenlaw.casting.util.KeyBinds;
import net.minecraft.client.KeyMapping;
import net.minecraft.client.Minecraft;
import net.neoforged.api.distmarker.Dist;
import net.neoforged.api.distmarker.OnlyIn;
import net.neoforged.bus.api.SubscribeEvent;
import net.neoforged.fml.common.EventBusSubscriber;
import net.neoforged.neoforge.client.event.ClientTickEvent;
import net.neoforged.neoforge.client.event.InputEvent;
import net.neoforged.neoforge.client.event.RegisterKeyMappingsEvent;
import net.neoforged.neoforge.client.network.ClientPacketDistributor;
import net.neoforged.neoforge.network.PacketDistributor;

@OnlyIn(Dist.CLIENT)
@EventBusSubscriber(modid = Casting.MOD_ID ,value = Dist.CLIENT)
public class ClientEvents {

    private static boolean lastState = false;

    @SubscribeEvent
    public static void onClientJumping(ClientTickEvent.Pre event) {

        KeyMapping jumpKey = Minecraft.getInstance().options.keyJump;
        boolean isJumping = jumpKey.isDown();

        if (isJumping != lastState) {
            ClientPacketDistributor.sendToServer(new JetJumpPacket(isJumping));
            lastState = isJumping;
        }
    }

    @SubscribeEvent
    public static void onHotKeyPress(InputEvent.Key event) {

        int armorSlot = 0;

        if (KeyBinds.HELMET_HOTKEY.consumeClick()) {
            armorSlot = 1;
        }
        else if (KeyBinds.CHESTPLATE_HOTKEY.consumeClick()) {
            armorSlot = 2;
        } else if (KeyBinds.LEGGINGS_HOTKEY.consumeClick()) {
            armorSlot = 3;
        } else if (KeyBinds.BOOTS_HOTKEY.consumeClick()) {
            armorSlot = 4;
        }

        if (armorSlot == 0) {
            return;
        }

        ClientPacketDistributor.sendToServer(new ToggleArmorModifiersPayload(armorSlot));

    }

}
