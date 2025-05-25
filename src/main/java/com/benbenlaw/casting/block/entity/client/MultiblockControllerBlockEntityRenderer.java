package com.benbenlaw.casting.block.entity.client;

import com.benbenlaw.casting.block.entity.multiblock.MultiblockControllerBlockEntity;
import com.benbenlaw.casting.util.MultiFluidTankSharedCapacity;
import com.benbenlaw.core.util.RenderUtil;
import com.mojang.blaze3d.vertex.PoseStack;
import com.mojang.blaze3d.vertex.VertexConsumer;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.Sheets;
import net.minecraft.client.renderer.blockentity.BlockEntityRenderer;
import net.minecraft.client.renderer.blockentity.BlockEntityRendererProvider;
import net.minecraft.client.renderer.texture.TextureAtlasSprite;
import net.minecraft.core.Direction;
import net.minecraft.world.inventory.InventoryMenu;
import net.minecraft.world.level.block.entity.BlockEntity;
import net.minecraft.world.level.material.Fluid;
import net.neoforged.neoforge.client.extensions.common.IClientFluidTypeExtensions;
import net.neoforged.neoforge.fluids.FluidStack;

//todo, this is f****** rubbish and doesnt work at all completely rework this
public class MultiblockControllerBlockEntityRenderer implements BlockEntityRenderer<MultiblockControllerBlockEntity> {

    public MultiblockControllerBlockEntityRenderer(BlockEntityRendererProvider.Context context) {
    }

    @Override
    public void render(MultiblockControllerBlockEntity entity, float pPartialTick, PoseStack pPoseStack,
                       MultiBufferSource pBufferSource, int pPackedLight, int pPackedOverlay) {
        MultiFluidTankSharedCapacity fluidTank = entity.fluidHandler;
        if (!fluidTank.getFluids().isEmpty()) {
            FluidStack fluidStack = fluidTank.getFluids().getFirst();
            VertexConsumer buffer = pBufferSource.getBuffer(Sheets.translucentCullBlockSheet());
            PoseStack.Pose last = pPoseStack.last();

            // Calculate fill amount based on the tank capacity
            float fillAmount = fluidStack.getAmount() / (float) fluidTank.getTankCapacity(1);

            // Render fluid inside the 3x3x3 tank
            renderFluid(last, buffer, entity, fluidStack.getFluid(), fillAmount, pPackedLight);
        }
    }

    private static void renderFluid(PoseStack.Pose pose, VertexConsumer consumer, BlockEntity entity, Fluid fluid, float fillAmount, int packedLight) {
        int color = IClientFluidTypeExtensions.of(fluid).getTintColor(fluid.defaultFluidState(), entity.getLevel(), entity.getBlockPos());
        renderFluid(pose, consumer, fluid, fillAmount, color, packedLight);
    }

    public static void renderFluid(PoseStack.Pose pose, VertexConsumer consumer, Fluid fluid, float fillAmount, int color, int packedLight) {
        // Get fluid texture
        IClientFluidTypeExtensions props = IClientFluidTypeExtensions.of(fluid);
        TextureAtlasSprite texture = Minecraft.getInstance().getTextureAtlas(InventoryMenu.BLOCK_ATLAS).apply(props.getStillTexture());

        // Define the size of the tank (3x3x3)
        float tankSize = 3.0f; // 3x3x3 tank, 3 units in each dimension
        float fluidHeight = fillAmount * tankSize; // Adjust fluid height based on fillAmount

        // Inset is the margin between fluid and tank walls, adjust for a 3x3x3 tank
        float inset = 0.1F; // slightly larger inset for a more natural look
        float faceSize = 1.0F / 3.0F; // Each fluid face should cover a 1x1 area within the 3x3 tank

        // Move the fluid up by 4 blocks (in the Y direction)
        float moveUp = 4.0f; // 4 blocks up (for testing)

        // Render fluid in the 3x3 base area
        // Iterate through the 3x3 grid and render fluid faces at each position
        for (int x = 0; x < 3; x++) {
            for (int z = 0; z < 3; z++) {
                // Calculate the position of each fluid face in the 3x3 grid
                float posX = inset + (x * faceSize); // X position for fluid face
                float posZ = inset + (z * faceSize); // Z position for fluid face

                // Render the fluid face on the bottom (in the 3x3 base area)
                RenderUtil.renderFace(Direction.UP, pose, consumer, texture, posX, posZ + moveUp, inset + fluidHeight, faceSize, faceSize, color, packedLight);
            }
        }

        // Render fluid on the sides (if necessary, for the 3x3 walls)
        // You can add side rendering if you want to show fluid inside the walls (North, South, East, West).
        RenderUtil.renderFace(Direction.SOUTH, pose, consumer, texture, inset, inset + moveUp, inset, faceSize, fluidHeight, color, packedLight);
        RenderUtil.renderFace(Direction.NORTH, pose, consumer, texture, inset, inset + moveUp, inset, faceSize, fluidHeight, color, packedLight);
        RenderUtil.renderFace(Direction.EAST, pose, consumer, texture, inset, inset + moveUp, inset, faceSize, fluidHeight, color, packedLight);
        RenderUtil.renderFace(Direction.WEST, pose, consumer, texture, inset, inset + moveUp, inset, faceSize, fluidHeight, color, packedLight);

        // Render top (up) face, slightly adjusted based on fluid height
        RenderUtil.renderFace(Direction.UP, pose, consumer, texture, inset, inset + moveUp, inset + fluidHeight, faceSize, faceSize, color, packedLight);

        // Render bottom (down) face, adjusting based on moveUp offset
        RenderUtil.renderFace(Direction.DOWN, pose, consumer, texture, inset, inset + moveUp, 1 - inset, faceSize, faceSize, color, packedLight);
    }



}
