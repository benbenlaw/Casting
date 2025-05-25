package com.benbenlaw.casting.screen.util;

import com.benbenlaw.casting.block.entity.ControllerBlockEntity;
import com.benbenlaw.casting.block.entity.SolidifierBlockEntity;
import com.benbenlaw.core.screen.util.CoreWidget;
import com.benbenlaw.core.util.MouseUtil;
import com.mojang.blaze3d.systems.RenderSystem;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.Font;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.narration.NarrationElementOutput;
import net.minecraft.client.gui.screens.Screen;
import net.minecraft.client.renderer.texture.AbstractTexture;
import net.minecraft.client.renderer.texture.TextureAtlas;
import net.minecraft.client.renderer.texture.TextureAtlasSprite;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.util.FastColor;
import net.minecraft.util.FormattedCharSequence;
import net.minecraft.world.level.block.entity.BlockEntity;
import net.neoforged.neoforge.client.extensions.common.IClientFluidTypeExtensions;
import net.neoforged.neoforge.fluids.FluidStack;
import net.neoforged.neoforge.fluids.capability.templates.FluidTank;

import java.util.Arrays;

@Deprecated(since = "2.0.0")
public class FuelTankFluidStackWidget extends CoreWidget {
    private final Screen screen;
    private final FluidTank getFluid;
    private BlockEntity blockEntity;

    public FuelTankFluidStackWidget(Screen screen, FluidTank getFluid, BlockEntity blockEntity, int pX, int pY, int pWidth, int pHeight) {
        super(pX, pY, pWidth, pHeight);
        this.screen = screen;
        this.getFluid = getFluid;
        this.blockEntity = blockEntity;
    }
    public void renderWidget(GuiGraphics guiGraphics, int mouseX, int mouseY, float partialTick) {
        Minecraft minecraft = Minecraft.getInstance();
        RenderSystem.defaultBlendFunc();
        RenderSystem.enableDepthTest();
        FluidTank fluidTank = this.getFluid;
        if (!fluidTank.getFluid().isEmpty()) {
            FluidStack fluidStack = fluidTank.getFluid();
            IClientFluidTypeExtensions props = IClientFluidTypeExtensions.of(fluidStack.getFluid());
            ResourceLocation still = props.getStillTexture(fluidStack);
            if (still != null) {
                AbstractTexture texture = minecraft.getTextureManager().getTexture(TextureAtlas.LOCATION_BLOCKS);
                if (texture instanceof TextureAtlas) {
                    TextureAtlas atlas = (TextureAtlas)texture;
                    TextureAtlasSprite sprite = atlas.getSprite(still);
                    int color = props.getTintColor();
                    RenderSystem.setShaderColor((float) FastColor.ARGB32.red(color) / 255.0F, (float) FastColor.ARGB32.green(color) / 255.0F, (float) FastColor.ARGB32.blue(color) / 255.0F, (float) FastColor.ARGB32.alpha(color) / 255.0F);
                    RenderSystem.enableBlend();
                    int stored = fluidTank.getFluidAmount();
                    float capacity = (float)fluidTank.getCapacity();
                    float filledVolume = (float)stored / capacity;
                    int renderableHeight = (int)(filledVolume * (float)this.height);
                    int atlasWidth = (int)((float)sprite.contents().width() / (sprite.getU1() - sprite.getU0()));
                    int atlasHeight = (int)((float)sprite.contents().height() / (sprite.getV1() - sprite.getV0()));
                    guiGraphics.pose().pushPose();
                    guiGraphics.pose().translate(0.0F, (float)(this.height - 16), 0.0F);

                    for(int i = 0; (double)i < Math.ceil((double)((float)renderableHeight / 16.0F)); ++i) {
                        int drawingHeight = Math.min(16, renderableHeight - 16 * i);
                        int notDrawingHeight = 16 - drawingHeight;
                        guiGraphics.blit(TextureAtlas.LOCATION_BLOCKS, this.x, this.y + notDrawingHeight, 0, sprite.getU0() * (float)atlasWidth, sprite.getV0() * (float)atlasHeight + (float)notDrawingHeight, this.width, drawingHeight, atlasWidth, atlasHeight);
                        guiGraphics.pose().translate(0.0F, -16.0F, 0.0F);
                    }

                    RenderSystem.setShaderColor(1.0F, 1.0F, 1.0F, 1.0F);
                    guiGraphics.pose().popPose();
                }
            }

            this.renderToolTip(guiGraphics, mouseX, mouseY);
        }

        RenderSystem.disableDepthTest();
    }

    protected void updateWidgetNarration(NarrationElementOutput pNarrationElementOutput) {
    }

    public void renderToolTip(GuiGraphics guiGraphics, int mouseX, int mouseY) {
        if (MouseUtil.isMouseAboveArea(mouseX, mouseY, this.x, this.y, 0, 0, 16, 16)) {
            Font var10001 = this.screen.getMinecraft().font;

            if (blockEntity instanceof ControllerBlockEntity controllerBlockEntity) {

                // Create an array to hold the tooltip lines
                FormattedCharSequence[] tooltipLines = new FormattedCharSequence[3];

                tooltipLines[0] = this.getFluid.getFluid().getHoverName().getVisualOrderText();
                tooltipLines[1] = Component.literal("" + this.getFluid.getFluidAmount() + "mB / " + this.getFluid.getCapacity() + "mB").getVisualOrderText();

                tooltipLines[2] = Component.literal("Fuel Temp: " + controllerBlockEntity.fuelTemp).getVisualOrderText();
            //    tooltipLines[3] = Component.literal("Ticks Per Recipe: " + controllerBlockEntity.maxProgress).getVisualOrderText();

                guiGraphics.renderTooltip(var10001, Arrays.asList(tooltipLines), mouseX, mouseY);

            }

            if (blockEntity instanceof SolidifierBlockEntity solidifierBlockEntity) {

                // Create an array to hold the tooltip lines
                FormattedCharSequence[] tooltipLines = new FormattedCharSequence[4];

                tooltipLines[0] = this.getFluid.getFluid().getHoverName().getVisualOrderText();
                tooltipLines[1] = Component.literal("" + this.getFluid.getFluidAmount() + "mB / " + this.getFluid.getCapacity() + "mB").getVisualOrderText();

                tooltipLines[2] = Component.literal("Fuel Temp: " + solidifierBlockEntity.fuelTemp).getVisualOrderText();
                tooltipLines[3] = Component.literal("Ticks Per Recipe: " + solidifierBlockEntity.maxProgress).getVisualOrderText();

                guiGraphics.renderTooltip(var10001, Arrays.asList(tooltipLines), mouseX, mouseY);

            }
        }
    }

}