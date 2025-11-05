package com.benbenlaw.casting.event.client;

import com.benbenlaw.casting.Casting;
import com.benbenlaw.casting.event.ToolEvents;
import com.mojang.blaze3d.vertex.PoseStack;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.LevelRenderer;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.RenderType;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.phys.AABB;
import net.minecraft.world.phys.BlockHitResult;
import net.minecraft.world.phys.HitResult;
import net.minecraft.world.phys.Vec3;
import net.neoforged.api.distmarker.Dist;
import net.neoforged.api.distmarker.OnlyIn;
import net.neoforged.bus.api.SubscribeEvent;
import net.neoforged.fml.common.EventBusSubscriber;
import net.neoforged.neoforge.client.event.ExtractBlockOutlineRenderStateEvent;

import java.util.List;

import static com.benbenlaw.casting.event.ToolEvents.isToggleableModifierActive;
import static com.benbenlaw.casting.item.EquipmentModifier.EXCAVATION;


/*
@OnlyIn(Dist.CLIENT)
@EventBusSubscriber(modid = Casting.MOD_ID ,value = Dist.CLIENT)
public class ExcavationOutlineRenderer {

    @SubscribeEvent
    public static void onBlockHighlight(ExtractBlockOutlineRenderStateEvent event) {
        Minecraft mc = Minecraft.getInstance();
        if (mc.player == null || mc.level == null) return;

        ItemStack tool = mc.player.getMainHandItem();
        if (!isExcavationTool(tool)) return;

        BlockHitResult target = event.getHitResult();

        BlockPos origin = target.getBlockPos();
        Direction face = target.getDirection();
        int excavationLevel = (int) tool.getComponents().getOrDefault(EXCAVATION.dataComponent.get(), 0);

        List<BlockPos> excavationPlane = ToolEvents.getExcavationPlane(origin, face, excavationLevel);

        PoseStack poseStack = event.getPoseStack();
        MultiBufferSource buffer = event.getMultiBufferSource();
        Vec3 cameraPos = event.getCamera().getPosition();

        for (BlockPos pos : excavationPlane) {
            BlockState state = mc.level.getBlockState(pos);

            if (state.isAir()) continue; // Skip air blocks

            AABB aabb = new AABB(pos).move(-cameraPos.x, -cameraPos.y, -cameraPos.z);

            LevelRenderer.renderLineBox(
                poseStack,
                buffer.getBuffer(RenderType.lines()),
                aabb,
                0, 0, 0, 0.5f
            );
        }

        // Prevent default highlight box
        event.setCanceled(true);
    }
    private static boolean isExcavationTool(ItemStack stack) {
        return stack.getComponents().has(EXCAVATION.dataComponent.get()) && isToggleableModifierActive(stack);
    }


}

 */