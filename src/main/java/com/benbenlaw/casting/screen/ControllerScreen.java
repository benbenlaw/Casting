package com.benbenlaw.casting.screen;

import com.benbenlaw.casting.Casting;
import com.benbenlaw.casting.block.entity.ControllerBlockEntity;
import com.benbenlaw.core.screen.util.FluidRenderingUtils;
import net.minecraft.client.gui.GuiGraphicsExtractor;
import net.minecraft.client.gui.screens.inventory.AbstractContainerScreen;
import net.minecraft.client.renderer.RenderPipelines;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.Identifier;
import net.minecraft.world.entity.player.Inventory;

import java.util.Objects;

public class ControllerScreen extends AbstractContainerScreen<ControllerMenu> {

    private static final Identifier TEXTURE = Casting.identifier("textures/gui/controller_gui.png");
    private static final Identifier PROGRESS_ARROW = Casting.identifier("controller_progress");

    public ControllerScreen(ControllerMenu menu, Inventory inventory, Component title) {
        super(menu, inventory, title);
    }

    @Override
    public void extractBackground(GuiGraphicsExtractor guiGraphics, int mouseX, int mouseY, float a) {
        super.extractBackground(guiGraphics, mouseX, mouseY, a);

        int x = (width - imageWidth) / 2;
        int y = (height - imageHeight) / 2;

        guiGraphics.blit(RenderPipelines.GUI_TEXTURED, TEXTURE, x, y, 0, 0, imageWidth, imageHeight, 256, 256);


        if (menu.isCrafting()) {
            for (int i = 0; i < 15; i++) {
                int scaledHeight = menu.getScaledProgress(i);

                if (scaledHeight > 0) {
                    int row = i / 5;
                    int col = i % 5;
                    int slotX = x + 8 + (col * 19);
                    int slotY = y + 16 + (row * 19);
                    int yOffset = 16 - scaledHeight;

                    guiGraphics.blitSprite(RenderPipelines.GUI_TEXTURED, PROGRESS_ARROW, slotX, slotY + yOffset, 16, scaledHeight);

                    int xPos = 8 + (i * 10);
                    if (i >= 5) xPos += 5;
                    if (i >= 10) xPos += 5;

                    //guiGraphics.blitSprite(RenderPipelines.GUI_TEXTURED, Core.identifier("duration_icon"), x + xPos, y - 5, 10, 10);
                }
            }
        }
    }

    @Override
    public void extractRenderState(GuiGraphicsExtractor guiGraphics, int mouseX, int mouseY, float partialTick) {
        super.extractRenderState(guiGraphics, mouseX, mouseY, partialTick);

        int x = (width - imageWidth) / 2;
        int y = (height - imageHeight) / 2;

        if (ControllerBlockEntity.getActiveFuelTank(menu.level, menu.blockPos) != null) {
            FluidRenderingUtils.renderFluid(guiGraphics, Objects.requireNonNull(ControllerBlockEntity.getActiveFuelTank(menu.level, menu.blockPos)).getFluidHandler(), 0, x, y,
                    110, 51, 16, 16, mouseX, mouseY, Component.translatable("tooltip.casting.no_fuel"));
        }

        FluidRenderingUtils.renderFluid(guiGraphics, menu.blockEntity.getFluidHandler(), 0, x, y,
                134, 18, 23, 16, mouseX, mouseY, Component.translatable("tooltip.casting.empty"));

        FluidRenderingUtils.renderFluid(guiGraphics, menu.blockEntity.getFluidHandler(), 1, x, y,
                152, 18, 23, 16, mouseX, mouseY, Component.translatable("tooltip.casting.empty"));

        FluidRenderingUtils.renderFluid(guiGraphics, menu.blockEntity.getFluidHandler(), 2, x, y,
                134, 45, 23, 16, mouseX, mouseY, Component.translatable("tooltip.casting.empty"));

        FluidRenderingUtils.renderFluid(guiGraphics, menu.blockEntity.getFluidHandler(), 3, x, y,
                134, 45, 23, 16, mouseX, mouseY, Component.translatable("tooltip.casting.empty"));
    }
}
