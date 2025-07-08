package com.benbenlaw.casting.event.client;

import com.benbenlaw.casting.Casting;
import com.benbenlaw.casting.event.ToolEvents;
import com.benbenlaw.casting.item.CastingDataComponents;
import com.mojang.blaze3d.vertex.BufferUploader;
import com.mojang.blaze3d.vertex.PoseStack;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.LevelRenderer;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.RenderType;
import net.minecraft.client.renderer.block.BlockRenderDispatcher;
import net.minecraft.client.renderer.texture.TextureAtlas;
import net.minecraft.client.renderer.texture.TextureAtlasSprite;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.phys.AABB;
import net.minecraft.world.phys.BlockHitResult;
import net.minecraft.world.phys.HitResult;
import net.minecraft.world.phys.Vec3;
import net.neoforged.api.distmarker.Dist;
import net.neoforged.api.distmarker.OnlyIn;
import net.neoforged.bus.api.SubscribeEvent;
import net.neoforged.fml.common.EventBusSubscriber;
import net.neoforged.neoforge.client.event.RenderHighlightEvent;
import net.neoforged.neoforge.client.event.RenderLevelStageEvent;

import java.util.List;
import java.util.Map;
import java.util.UUID;

import static com.benbenlaw.casting.event.ToolEvents.isToggleableModifierActive;
import static com.benbenlaw.casting.event.ToolEvents.lastHitDirectionMap;

@OnlyIn(Dist.CLIENT)
@EventBusSubscriber(modid = Casting.MOD_ID ,value = Dist.CLIENT)
public class ExcavationOutlineRenderer {

    @SubscribeEvent
    public static void onBlockHighlight(RenderHighlightEvent.Block event) {
        Minecraft mc = Minecraft.getInstance();
        if (mc.player == null || mc.level == null) return;

        ItemStack tool = mc.player.getMainHandItem();
        if (!isExcavationTool(tool)) return;

        HitResult target = event.getTarget();
        if (!(target instanceof BlockHitResult blockHit)) return;

        BlockPos origin = blockHit.getBlockPos();
        Direction face = blockHit.getDirection();
        int excavationLevel = tool.getComponents().getOrDefault(CastingDataComponents.EXCAVATION.get(), 0);

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
        return stack.getComponents().has(CastingDataComponents.EXCAVATION.get()) && isToggleableModifierActive(stack);
    }


}