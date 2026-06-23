package com.benbenlaw.casting.screen;

import com.benbenlaw.casting.Casting;
import com.benbenlaw.core.Core;
import com.benbenlaw.core.screen.util.DurationTooltip;
import com.benbenlaw.core.screen.util.FluidRenderingUtils;
import net.minecraft.client.gui.GuiGraphicsExtractor;
import net.minecraft.client.gui.screens.inventory.AbstractContainerScreen;
import net.minecraft.client.gui.screens.inventory.tooltip.ClientTooltipComponent;
import net.minecraft.client.gui.screens.inventory.tooltip.DefaultTooltipPositioner;
import net.minecraft.client.renderer.RenderPipelines;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.Identifier;
import net.minecraft.world.entity.player.Inventory;
import net.neoforged.neoforge.transfer.fluid.FluidStacksResourceHandler;
import net.neoforged.neoforge.transfer.fluid.FluidUtil;
import net.neoforged.neoforge.transfer.fluid.FluidResource;

import java.util.List;

public class MixerScreen extends AbstractContainerScreen<MixerMenu> {

    private static final Identifier TEXTURE = Casting.identifier("textures/gui/mixer_gui.png");
    private static final Identifier PROGRESS_ARROW = Core.identifier("progress_arrow");

    public MixerScreen(MixerMenu menu, Inventory inventory, Component component) {
        super(menu, inventory, component);
    }

    @Override
    public void extractBackground(GuiGraphicsExtractor guiGraphics, int mouseX, int mouseY, float a) {
        super.extractBackground(guiGraphics, mouseX, mouseY, a);

        int x = (width - imageWidth) / 2;
        int y = (height - imageHeight) / 2;

        guiGraphics.blit(RenderPipelines.GUI_TEXTURED, TEXTURE, x, y, 0, 0, imageWidth, imageHeight, 256, 256);

        if (menu.isCrafting()) {
            guiGraphics.blitSprite(RenderPipelines.GUI_TEXTURED, PROGRESS_ARROW, 24, 16, 0, 0, x + 117, y + 34, menu.getScaledProgress() + 1, 16);
        }
    }

    @Override
    public void extractRenderState(GuiGraphicsExtractor guiGraphics, int mouseX, int mouseY, float partialTick) {
        super.extractRenderState(guiGraphics, mouseX, mouseY, partialTick);

        int x = (width - imageWidth) / 2;
        int y = (height - imageHeight) / 2;

        DurationTooltip.renderDurationTooltip(guiGraphics, mouseX, mouseY, x, y, 161, 5, menu.data.get(0), menu.data.get(1));

        //Fluid Tanks
        for (int i = 0; i < 4; i++) {
            FluidRenderingUtils.renderFluid(guiGraphics, menu.blockEntity.getFluidHandler(), i, x, y,
                     8 + (i * 27), 44, 23, 16, mouseX, mouseY, Component.translatable("tooltip.casting.empty"));
        }

        FluidRenderingUtils.renderFluid(guiGraphics, menu.blockEntity.getFluidHandler(), 4, x, y,
                152, 20, 47, 16, mouseX, mouseY, Component.translatable("tooltip.casting.empty"));


        //Filter Tanks
        for (int i = 0; i < 4; i++) {
            FluidRenderingUtils.renderFluid(guiGraphics, menu.blockEntity.getFilterFluidHandler(), i, x, y,
                    8 + (i * 27), 20, 16, 16, mouseX, mouseY, Component.translatable("tooltip.casting.empty_filter"));
        }
    }
}