package com.benbenlaw.casting.screen;

import com.benbenlaw.casting.Casting;
import com.benbenlaw.casting.block.CastingBlocks;
import com.benbenlaw.casting.item.CastingItems;
import com.benbenlaw.casting.network.payload.ClearTankPayload;
import com.benbenlaw.casting.network.payload.FluidMoverPayload;
import com.benbenlaw.casting.screen.util.FluidStackWidgetBigTank;
import com.benbenlaw.core.util.MouseUtil;
import com.mojang.blaze3d.systems.RenderSystem;
import net.minecraft.ChatFormatting;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.screens.inventory.AbstractContainerScreen;
import net.minecraft.client.renderer.GameRenderer;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.Level;
import net.neoforged.neoforge.network.PacketDistributor;

public class MixerScreen extends AbstractContainerScreen<MixerMenu> {

    Level level;
    private static final ResourceLocation TEXTURE =
            ResourceLocation.fromNamespaceAndPath(Casting.MOD_ID, "textures/gui/mixer_gui.png");

    public MixerScreen(MixerMenu menu, Inventory inventory, Component component) {
        super(menu, inventory, component);
        this.level = menu.level;
    }

    @Override
    protected void init() {
        super.init();
        addFluidWidgets();
    }


    public void addFluidWidgets() {
        addRenderableOnly(new FluidStackWidgetBigTank(this, getMenu().blockEntity.TANK_1, this.leftPos + 9, this.topPos + 15, 14, 56));
        addRenderableOnly(new FluidStackWidgetBigTank(this, getMenu().blockEntity.TANK_2, this.leftPos + 27, this.topPos + 15, 14, 56));
        addRenderableOnly(new FluidStackWidgetBigTank(this, getMenu().blockEntity.TANK_3, this.leftPos + 45, this.topPos + 15, 14, 56));
        addRenderableOnly(new FluidStackWidgetBigTank(this, getMenu().blockEntity.TANK_4, this.leftPos + 63, this.topPos + 15, 14, 56));
        addRenderableOnly(new FluidStackWidgetBigTank(this, getMenu().blockEntity.TANK_5, this.leftPos + 81, this.topPos + 15, 14, 56));
        addRenderableOnly(new FluidStackWidgetBigTank(this, getMenu().blockEntity.TANK_6, this.leftPos + 99, this.topPos + 15, 14, 56));

        addRenderableOnly(new FluidStackWidgetBigTank(this, getMenu().blockEntity.OUTPUT_TANK, this.leftPos + 153, this.topPos + 15, 14, 56));
    }

    @Override
    protected void renderBg(GuiGraphics guiGraphics, float partialTicks, int mouseX, int mouseY) {
        RenderSystem.setShader(GameRenderer::getPositionTexShader);
        RenderSystem.setShaderColor(1.0F, 1.0F, 1.0F, 1.0F);
        RenderSystem.setShaderTexture(0, TEXTURE);

        renderProgressBars(guiGraphics);  // Draw progress bars over the item stacks

        int x = (width - imageWidth) / 2;
        int y = (height - imageHeight) / 2;

        guiGraphics.blit(TEXTURE, x, y, 0, 0, imageWidth, imageHeight);
    }

    @Override
    public void render(GuiGraphics guiGraphics, int mouseX, int mouseY, float partialTicks) {

        renderBackground(guiGraphics, mouseX, mouseY, partialTicks);
        super.render(guiGraphics, mouseX, mouseY, partialTicks);
        renderProgressBars(guiGraphics);
        renderTooltip(guiGraphics, mouseX, mouseY);
        renderWarning(guiGraphics, mouseX, mouseY);

        renderNoWhisks(guiGraphics, mouseX, mouseY, this.leftPos, this.topPos);
        renderWhisks(guiGraphics, mouseX, mouseY, this.leftPos, this.topPos);
    }

    private void renderNoWhisks(GuiGraphics guiGraphics, int mouseX, int mouseY, int x, int y) {
        int whiskTotal = this.menu.blockEntity.totalWhisksAbove;
        if (MouseUtil.isMouseAboveArea(mouseX, mouseY, x, y, 126,  55, 16, 16) && whiskTotal == 0) {
            guiGraphics.renderTooltip(this.font, Component.literal("Place Mixer Whisks above the mixer to speed up!").withStyle(ChatFormatting.RED), mouseX, mouseY);
        }
    }

    private void renderWhisks(GuiGraphics guiGraphics, int mouseX, int mouseY, int x, int y) {
        int whiskTotal = this.menu.blockEntity.totalWhisksAbove;

        if (MouseUtil.isMouseAboveArea(mouseX, mouseY, x, y, 126,  55, 16, 16) && whiskTotal > 0) {
            guiGraphics.renderTooltip(this.font, Component.literal("Ticks Per Recipe: " + this.menu.blockEntity.maxProgress).withStyle(ChatFormatting.WHITE), mouseX, mouseY);
        }

        guiGraphics.renderItemDecorations(this.font, new ItemStack(CastingBlocks.MIXER_WHISK.get(), whiskTotal), x + 126, y + 55);
        guiGraphics.renderFakeItem(new ItemStack(CastingBlocks.MIXER_WHISK.get(), whiskTotal), this.leftPos + 126, this.topPos + 55);
    }

    private void renderWhiskAmount(GuiGraphics guiGraphics, int mouseX, int mouseY, int x, int y) {
        if (MouseUtil.isMouseAboveArea(mouseX, mouseY, x, y, 126, 55, 16, 16)) {


            guiGraphics.drawString(this.font, this.menu.blockEntity.maxProgress + " ticks", this.leftPos + 126,
                    this.topPos + 27, 0x3F3F3F, false);
        }
    }

    private void renderProgressBars(GuiGraphics guiGraphics) {
        RenderSystem.setShader(GameRenderer::getPositionTexShader);
        RenderSystem.setShaderColor(1.0F, 1.0F, 1.0F, 1.0F);
        RenderSystem.setShaderTexture(0, TEXTURE);

        int x = (width - imageWidth) / 2;
        int y = (height - imageHeight) / 2;

        guiGraphics.blit(TEXTURE, x + 122 , y + 34 , 176, 30, menu.getScaledProgress(), 16);
    }

    @Override
    public boolean mouseClicked(double mouseX, double mouseY, int mouseButton) {
        boolean handled = super.mouseClicked(mouseX, mouseY, mouseButton);

        // Get the item in the player's hand
        ItemStack heldItem = menu.getCarried();
        boolean isHoldingBucket = heldItem.is(CastingItems.FLUID_MOVER);

        int tankWidth = 14;
        int tankHeight = 56;

        // Handle Tanks 1–6
        for (int i = 1; i <= 6; i++) {
            int tankX = leftPos + 9 + (i - 1) * 18;  // Calculate X position for each tank
            int tankY = topPos + 15;

            if (MouseUtil.isMouseOver(mouseX, mouseY, tankX, tankY, tankWidth, tankHeight)) {
                boolean hasShiftDown = SolidifierScreen.hasShiftDown();

                if (isHoldingBucket) {
                    PacketDistributor.sendToServer(new FluidMoverPayload(menu.blockEntity.getBlockPos(), i)); // Send the packet to fill bucket
                } else {
                    PacketDistributor.sendToServer(new ClearTankPayload(menu.blockEntity.getBlockPos(), hasShiftDown, i));
                }
            }
        }

        // Handle Tank 7, which has a specific position (153, 15)
        int tank7X = leftPos + 153;
        int tank7Y = topPos + 15;

        if (MouseUtil.isMouseOver(mouseX, mouseY, tank7X, tank7Y, tankWidth, tankHeight)) {
            boolean hasShiftDown = SolidifierScreen.hasShiftDown();

            if (isHoldingBucket) {
                PacketDistributor.sendToServer(new FluidMoverPayload(menu.blockEntity.getBlockPos(), 7)); // Tank ID for Tank 7 is 7
            } else {
                PacketDistributor.sendToServer(new ClearTankPayload(menu.blockEntity.getBlockPos(), hasShiftDown, 7));
            }
        }

        return handled;
    }


    private void renderWarning(GuiGraphics guiGraphics, int mouseX, int mouseY) {

        int tankX_1 = leftPos + 9;
        int tankY_1 = topPos + 15;
        int tankX_2 = leftPos + 27;
        int tankY_2 = topPos + 15;
        int tankX_3 = leftPos + 45;
        int tankY_3 = topPos + 15;
        int tankX_4 = leftPos + 63;
        int tankY_4 = topPos + 15;
        int tankX_5 = leftPos + 81;
        int tankY_5 = topPos + 15;
        int tankX_6 = leftPos + 99;
        int tankY_6 = topPos + 15;
        int tankX_7 = leftPos + 153;
        int tankY_7 = topPos + 15;
        int tankWidth = 14;
        int tankHeight = 56;

        if (MouseUtil.isMouseOver(mouseX, mouseY, tankX_1, tankY_1, tankWidth, tankHeight) && SolidifierScreen.hasShiftDown()) {
            guiGraphics.renderTooltip(this.font, Component.translatable("gui.casting.buttons.dump_fluid").withStyle(ChatFormatting.RED), mouseX, mouseY - 14);
        }
        if (MouseUtil.isMouseOver(mouseX, mouseY, tankX_2, tankY_2, tankWidth, tankHeight) && SolidifierScreen.hasShiftDown()) {
            guiGraphics.renderTooltip(this.font, Component.translatable("gui.casting.buttons.dump_fluid").withStyle(ChatFormatting.RED), mouseX, mouseY - 14);
        }
        if (MouseUtil.isMouseOver(mouseX, mouseY, tankX_3, tankY_3, tankWidth, tankHeight) && SolidifierScreen.hasShiftDown()) {
            guiGraphics.renderTooltip(this.font, Component.translatable("gui.casting.buttons.dump_fluid").withStyle(ChatFormatting.RED), mouseX, mouseY - 14);
        }
        if (MouseUtil.isMouseOver(mouseX, mouseY, tankX_4, tankY_4, tankWidth, tankHeight) && SolidifierScreen.hasShiftDown()) {
            guiGraphics.renderTooltip(this.font, Component.translatable("gui.casting.buttons.dump_fluid").withStyle(ChatFormatting.RED), mouseX, mouseY - 14);
        }
        if (MouseUtil.isMouseOver(mouseX, mouseY, tankX_5, tankY_5, tankWidth, tankHeight) && SolidifierScreen.hasShiftDown()) {
            guiGraphics.renderTooltip(this.font, Component.translatable("gui.casting.buttons.dump_fluid").withStyle(ChatFormatting.RED), mouseX, mouseY - 14);
        }
        if (MouseUtil.isMouseOver(mouseX, mouseY, tankX_6, tankY_6, tankWidth, tankHeight) && SolidifierScreen.hasShiftDown()) {
            guiGraphics.renderTooltip(this.font, Component.translatable("gui.casting.buttons.dump_fluid").withStyle(ChatFormatting.RED), mouseX, mouseY - 14);
        }
        if (MouseUtil.isMouseOver(mouseX, mouseY, tankX_7, tankY_7, tankWidth, tankHeight) && SolidifierScreen.hasShiftDown()) {
            guiGraphics.renderTooltip(this.font, Component.translatable("gui.casting.buttons.dump_fluid").withStyle(ChatFormatting.RED), mouseX, mouseY - 14);
        }


        if (MouseUtil.isMouseOver(mouseX, mouseY, tankX_1, tankY_1, tankWidth, tankHeight) && !SolidifierScreen.hasShiftDown()) {
            guiGraphics.renderTooltip(this.font, Component.translatable("gui.casting.buttons.shift_dump_fluid").withStyle(ChatFormatting.RED), mouseX, mouseY - 14);
        }
        if (MouseUtil.isMouseOver(mouseX, mouseY, tankX_2, tankY_2, tankWidth, tankHeight) && !SolidifierScreen.hasShiftDown()) {
            guiGraphics.renderTooltip(this.font, Component.translatable("gui.casting.buttons.shift_dump_fluid").withStyle(ChatFormatting.RED), mouseX, mouseY - 14);
        }
        if (MouseUtil.isMouseOver(mouseX, mouseY, tankX_3, tankY_3, tankWidth, tankHeight) && !SolidifierScreen.hasShiftDown()) {
            guiGraphics.renderTooltip(this.font, Component.translatable("gui.casting.buttons.shift_dump_fluid").withStyle(ChatFormatting.RED), mouseX, mouseY - 14);
        }
        if (MouseUtil.isMouseOver(mouseX, mouseY, tankX_4, tankY_4, tankWidth, tankHeight) && !SolidifierScreen.hasShiftDown()) {
            guiGraphics.renderTooltip(this.font, Component.translatable("gui.casting.buttons.shift_dump_fluid").withStyle(ChatFormatting.RED), mouseX, mouseY - 14);
        }
        if (MouseUtil.isMouseOver(mouseX, mouseY, tankX_5, tankY_5, tankWidth, tankHeight) && !SolidifierScreen.hasShiftDown()) {
            guiGraphics.renderTooltip(this.font, Component.translatable("gui.casting.buttons.shift_dump_fluid").withStyle(ChatFormatting.RED), mouseX, mouseY - 14);
        }
        if (MouseUtil.isMouseOver(mouseX, mouseY, tankX_6, tankY_6, tankWidth, tankHeight) && !SolidifierScreen.hasShiftDown()) {
            guiGraphics.renderTooltip(this.font, Component.translatable("gui.casting.buttons.shift_dump_fluid").withStyle(ChatFormatting.RED), mouseX, mouseY - 14);
        }
        if (MouseUtil.isMouseOver(mouseX, mouseY, tankX_7, tankY_7, tankWidth, tankHeight) && !SolidifierScreen.hasShiftDown()) {
            guiGraphics.renderTooltip(this.font, Component.translatable("gui.casting.buttons.shift_dump_fluid").withStyle(ChatFormatting.RED), mouseX, mouseY - 14);
        }
    }
}
