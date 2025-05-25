package com.benbenlaw.casting.screen.util;

import com.benbenlaw.casting.util.MultiFluidTankSharedCapacity;
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
import net.neoforged.neoforge.client.extensions.common.IClientFluidTypeExtensions;
import net.neoforged.neoforge.fluids.FluidStack;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.List;

//TODO Move to core
public class MultiFluidStackWidget extends CoreWidget {
    private final Screen screen;
    private final MultiFluidTankSharedCapacity multiFluidTank;

    public MultiFluidStackWidget(Screen screen, MultiFluidTankSharedCapacity multiFluidTank, int pX, int pY, int pWidth, int pHeight) {
        super(pX, pY, pWidth, pHeight);
        this.screen = screen;
        this.multiFluidTank = multiFluidTank;
    }

    public void renderWidget(@NotNull GuiGraphics guiGraphics, int mouseX, int mouseY, float partialTick) {
        Minecraft minecraft = Minecraft.getInstance();
        RenderSystem.defaultBlendFunc();
        RenderSystem.enableDepthTest();

        if (multiFluidTank == null) {
            return;
        }

        List<FluidStack> fluids = multiFluidTank.getFluids();

        this.renderToolTip(guiGraphics, mouseX, mouseY);

        int totalCapacity = multiFluidTank.getTankCapacity(1);
        int yOffset = this.height;

        for (FluidStack fluidStack : fluids) {
            if (fluidStack.isEmpty()) continue;

            IClientFluidTypeExtensions props = IClientFluidTypeExtensions.of(fluidStack.getFluid());
            ResourceLocation still = props.getStillTexture(fluidStack);
            AbstractTexture texture = minecraft.getTextureManager().getTexture(TextureAtlas.LOCATION_BLOCKS);

            if (!(texture instanceof TextureAtlas atlas)) continue;

            TextureAtlasSprite sprite = atlas.getSprite(still);
            int color = props.getTintColor();
            RenderSystem.setShaderColor(
                    FastColor.ARGB32.red(color) / 255.0F,
                    FastColor.ARGB32.green(color) / 255.0F,
                    FastColor.ARGB32.blue(color) / 255.0F,
                    FastColor.ARGB32.alpha(color) / 255.0F
            );
            RenderSystem.enableBlend();

            int fluidAmount = fluidStack.getAmount();
            float filledRatio = (float) fluidAmount / totalCapacity;
            int renderableHeight = Math.max(1, (int)(filledRatio * this.height));

            int atlasWidth = (int)((float)sprite.contents().width() / (sprite.getU1() - sprite.getU0()));
            int atlasHeight = (int)((float)sprite.contents().height() / (sprite.getV1() - sprite.getV0()));

            guiGraphics.pose().pushPose();
            guiGraphics.pose().translate(0.0F, 0.0F, 0.0F);
            int remainingHeight = renderableHeight;
            while (remainingHeight > 0) {
                int drawHeight = Math.min(16, remainingHeight);
                int drawY = yOffset - drawHeight;
                int notDrawn = 16 - drawHeight;

                // Repeating the texture horizontally based on the width
                int drawWidth = this.width;
                int repeatCount = (int) Math.ceil((double) drawWidth / 16.0); // how many times the texture should repeat
                int drawnWidth = 0; // Track the total width we've drawn so far

                for (int i = 0; i < repeatCount; i++) {
                    int segmentWidth = Math.min(16, drawWidth - drawnWidth); // Only draw the remaining width
                    int uOffset = i * 16; // offset for each tile (16px wide)

                    // Only draw until we've filled the width
                    if (drawnWidth < drawWidth) {
                        guiGraphics.blit(
                                TextureAtlas.LOCATION_BLOCKS,
                                this.x + drawnWidth, // x position incremented per segment
                                this.y + drawY,
                                0,
                                (sprite.getU0() + i * 16) * atlasWidth, // adjusted U coordinate to repeat
                                sprite.getV0() * atlasHeight + notDrawn,
                                segmentWidth, // the width we actually want to draw
                                drawHeight,
                                atlasWidth,
                                atlasHeight
                        );
                        drawnWidth += segmentWidth; // Increment the drawn width
                    }
                }

                yOffset -= drawHeight;
                remainingHeight -= drawHeight;
            }

            guiGraphics.pose().popPose();
        }

        RenderSystem.setShaderColor(1.0F, 1.0F, 1.0F, 1.0F);
        RenderSystem.disableDepthTest();
    }


    protected void updateWidgetNarration(NarrationElementOutput pNarrationElementOutput) {
    }

    public void renderToolTip(GuiGraphics guiGraphics, int mouseX, int mouseY) {
        if (MouseUtil.isMouseAboveArea(mouseX, mouseY, this.x, this.y, 0, -1, this.width, this.height)) {
            Font font = this.screen.getMinecraft().font;
            List<FormattedCharSequence> tooltipText = new ArrayList<>();

            List<FluidStack> fluids = multiFluidTank.getFluids();
            int totalCapacity = multiFluidTank.getTankCapacity(1);
            int usedAmount = fluids.stream().mapToInt(FluidStack::getAmount).sum();
            int yOffset = this.y + this.height;
            boolean foundFluid = false;

            if (fluids.isEmpty()) {
                tooltipText.add(Component.literal("Air").getVisualOrderText());
                tooltipText.add(Component.literal("0mB / " + totalCapacity + "mB").getVisualOrderText());
            } else {
                for (FluidStack fluid : fluids) {
                    if (fluid.isEmpty()) continue;

                    int fluidAmount = fluid.getAmount();
                    float ratio = (float) fluidAmount / totalCapacity;
                    int height = Math.max(1, (int)(ratio * this.height));

                    int top = yOffset - height;

                    if (mouseY >= top && mouseY <= yOffset) {
                        tooltipText.add(fluid.getHoverName().getVisualOrderText());
                        tooltipText.add(Component.literal(fluidAmount + "mB").getVisualOrderText());
                        foundFluid = true;
                        break;
                    }

                    yOffset -= height;
                }

                // If mouse is in the tank but not over any fluid (i.e., above all rendered fluids)
                if (!foundFluid) {
                    tooltipText.add(Component.literal("Capacity").getVisualOrderText());
                    tooltipText.add(Component.literal(usedAmount + "mB / " + totalCapacity + "mB").getVisualOrderText());
                }
            }

            if (!tooltipText.isEmpty()) {
                guiGraphics.renderTooltip(font, tooltipText, mouseX, mouseY);
            }
        }
    }



}