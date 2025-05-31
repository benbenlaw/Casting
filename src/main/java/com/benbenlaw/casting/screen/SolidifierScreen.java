package com.benbenlaw.casting.screen;

import com.benbenlaw.casting.Casting;
import com.benbenlaw.casting.block.entity.TankBlockEntity;
import com.benbenlaw.casting.item.CastingItems;
import com.benbenlaw.casting.network.payload.ClearTankPayload;
import com.benbenlaw.casting.network.payload.FluidMoverPayload;
import com.benbenlaw.casting.network.payload.LockSolidifierPayload;
import com.benbenlaw.casting.screen.util.FluidStackWidgetBigTank;
import com.benbenlaw.casting.screen.util.FuelTankFluidStackWidget;
import com.benbenlaw.core.util.MouseUtil;
import com.mojang.blaze3d.systems.RenderSystem;
import net.minecraft.ChatFormatting;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.screens.inventory.AbstractContainerScreen;
import net.minecraft.client.renderer.GameRenderer;
import net.minecraft.core.Direction;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.entity.BlockEntity;
import net.neoforged.neoforge.fluids.capability.templates.FluidTank;
import net.neoforged.neoforge.network.PacketDistributor;

public class SolidifierScreen extends AbstractContainerScreen<SolidifierMenu> {

    private Level level;
    private BlockEntity fuelTankEntity;
    private static final ResourceLocation TEXTURE =
            ResourceLocation.fromNamespaceAndPath(Casting.MOD_ID, "textures/gui/solidifier_gui.png");

    public SolidifierScreen(SolidifierMenu menu, Inventory inventory, Component component) {
        super(menu, inventory, component);
        this.level = menu.level;

        for (Direction direction : Direction.values()) {
            BlockEntity adjacentEntity = level.getBlockEntity(menu.blockEntity.getBlockPos().relative(direction));
            if (adjacentEntity instanceof TankBlockEntity tankBlockEntity) {
                fuelTankEntity = level.getBlockEntity(tankBlockEntity.getBlockPos());
                break;
            } else {
                fuelTankEntity = null;
            }
        }
    }

    @Override
    protected void init() {
        super.init();
        addFluidWidgets();
    }

    private void addFluidWidgets() {
        addRenderableOnly(new FluidStackWidgetBigTank(this, getMenu().blockEntity.TANK, leftPos + 27, topPos + 15, 14, 56));


        FluidTank fuelTank = new FluidTank(0);
        if (fuelTankEntity instanceof TankBlockEntity tankBlockEntity) {
            fuelTank = tankBlockEntity.FLUID_TANK;
        }

        if (fuelTank.getCapacity() != 0 && fuelTank.getFluidAmount() != 0) {
            addRenderableOnly(new FuelTankFluidStackWidget(this, fuelTank, this.menu.blockEntity, this.leftPos + 107, this.topPos + 55, 16, 16));
        }

    }


    @Override
    protected void renderBg(GuiGraphics guiGraphics, float partialTicks, int mouseX, int mouseY) {
        RenderSystem.setShader(GameRenderer::getPositionTexShader);
        RenderSystem.setShaderColor(1.0F, 1.0F, 1.0F, 1.0F);
        RenderSystem.setShaderTexture(0, TEXTURE);

        renderProgressBars(guiGraphics);  // Draw progress bars over the item stacks

        int x = leftPos;
        int y = topPos;

        guiGraphics.blit(TEXTURE, x, y, 0, 0, imageWidth, imageHeight);
    }

    @Override
    public void render(GuiGraphics guiGraphics, int mouseX, int mouseY, float partialTicks) {

        int x = (width - imageWidth) / 2;
        int y = (height - imageHeight) / 2;

        renderBackground(guiGraphics, mouseX, mouseY, partialTicks);

        super.render(guiGraphics, mouseX, mouseY, partialTicks);
        renderProgressBars(guiGraphics);
        //    renderTickRate(guiGraphics, mouseX, mouseY, x, y);

        FluidTank fuelTank = new FluidTank(0);
        if (fuelTankEntity instanceof TankBlockEntity tankBlockEntity) {
            fuelTank = tankBlockEntity.FLUID_TANK;
        }

        if (fuelTank.getCapacity() == 0) {
            renderNoTank(guiGraphics, mouseX, mouseY, x, y);
        }
        else if (fuelTank.getFluidAmount() == 0) {
            renderEmptyTank(guiGraphics, mouseX, mouseY, x, y);
        }

        renderWarning(guiGraphics, mouseX, mouseY);
        renderTooltip(guiGraphics, mouseX, mouseY);
        renderFilteredFluid(guiGraphics, mouseX, mouseY);
    }


    private void renderProgressBars(GuiGraphics guiGraphics) {
        int x = leftPos;
        int y = topPos;
        guiGraphics.blit(TEXTURE, x + 104, y + 34, 176, 30, menu.getScaledProgress(), 16);
    }

    private void renderTickRate(GuiGraphics guiGraphics, int mouseX, int mouseY, int x, int y) {
        if (MouseUtil.isMouseAboveArea(mouseX, mouseY, x, y, 80, 36, 90, 16)) {
            guiGraphics.drawString(this.font, this.menu.getMaxProgress() + " ticks", this.leftPos + 120,
                    this.topPos + 68, 0x3F3F3F, false);
        }
    }

    @Override
    public boolean mouseClicked(double mouseX, double mouseY, int mouseButton) {
        boolean handled = super.mouseClicked(mouseX, mouseY, mouseButton);



        ItemStack heldItem = menu.getCarried();
        boolean isHoldingBucket = heldItem.is(CastingItems.FLUID_MOVER);

        int tankX = leftPos + 27;
        int tankY = topPos + 15;
        int tankWidth = 14;
        int tankHeight = 56;

        if (MouseUtil.isMouseOver(mouseX, mouseY, tankX, tankY, tankWidth, tankHeight)) {

            int tank = this.menu.blockEntity.TANK.getTanks();

            boolean hasShiftDown = SolidifierScreen.hasShiftDown();

            if (isHoldingBucket) {
                PacketDistributor.sendToServer(new FluidMoverPayload(menu.blockEntity.getBlockPos(), tank)); // Send the packet to fill bucket
            } else {
                PacketDistributor.sendToServer(new ClearTankPayload(menu.blockEntity.getBlockPos(), hasShiftDown, tank));
            }
        }

        if (MouseUtil.isMouseOver(mouseX, mouseY, leftPos + 6, topPos + 36, 16, 16)) {

            PacketDistributor.sendToServer(new LockSolidifierPayload(menu.blockEntity.getBlockPos()));

        }


        return handled;
    }
    private void renderWarning(GuiGraphics guiGraphics, int mouseX, int mouseY) {

        int tankX = leftPos + 27;
        int tankY = topPos + 15;
        int tankWidth = 14;
        int tankHeight = 56;

        if (MouseUtil.isMouseOver(mouseX, mouseY, tankX, tankY, tankWidth, tankHeight) && SolidifierScreen.hasShiftDown()) {
            guiGraphics.renderTooltip(this.font, Component.translatable("gui.casting.buttons.dump_fluid").withStyle(ChatFormatting.RED), mouseX, mouseY - 14);
        }
        if (MouseUtil.isMouseOver(mouseX, mouseY, tankX, tankY, tankWidth, tankHeight) && !SolidifierScreen.hasShiftDown()) {
            guiGraphics.renderTooltip(this.font, Component.translatable("gui.casting.buttons.shift_dump_fluid").withStyle(ChatFormatting.RED), mouseX, mouseY - 14);
        }
    }


    private void renderFilteredFluid(GuiGraphics guiGraphics, int mouseX, int mouseY) {
        if (MouseUtil.isMouseOver(mouseX, mouseY, leftPos + 6, topPos + 36, 16, 16)) {
            boolean limitMode = this.menu.blockEntity.getLimitMode();

            if (limitMode) {
                guiGraphics.renderTooltip(this.font, Component.literal("Locking in 100mb of Fluid! Click to toggle").withStyle(ChatFormatting.GREEN), mouseX, mouseY);
            } else {
                guiGraphics.renderTooltip(this.font, Component.literal("Not Filtering Fluid! Click to toggle").withStyle(ChatFormatting.RED), mouseX, mouseY);
            }
        }
    }

    private void renderNoTank(GuiGraphics guiGraphics, int mouseX, int mouseY, int x, int y) {
        if (MouseUtil.isMouseAboveArea(mouseX, mouseY, x, y, 107,  55, 16, 16)) {
            guiGraphics.renderTooltip(this.font, Component.literal("Place a Tank adjacent to the Solidifier to increase speed!").withStyle(ChatFormatting.RED), mouseX, mouseY);
        }
    }

    private void renderEmptyTank (GuiGraphics guiGraphics, int mouseX, int mouseY, int x, int y) {
        if (MouseUtil.isMouseAboveArea(mouseX, mouseY, x, y, 107,  55, 16, 16)) {
            guiGraphics.renderTooltip(this.font, Component.literal("Use cooler fuels in tank to speed up the Solidifier!").withStyle(ChatFormatting.RED), mouseX, mouseY);
        }
    }

}