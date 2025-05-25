package com.benbenlaw.casting.screen.multiblock;

import com.benbenlaw.casting.Casting;
import com.benbenlaw.casting.screen.util.FluidStackStackWidget;
import com.mojang.blaze3d.systems.RenderSystem;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.screens.inventory.AbstractContainerScreen;
import net.minecraft.client.renderer.GameRenderer;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.entity.BlockEntity;

public class MultiblockFuelTankScreen extends AbstractContainerScreen<MultiblockFuelTankMenu> {

    private Level level;
    private BlockEntity fuelTankEntity;
    private static final ResourceLocation TEXTURE =
            ResourceLocation.fromNamespaceAndPath(Casting.MOD_ID, "textures/gui/controller_tank_gui.png");

    public MultiblockFuelTankScreen(MultiblockFuelTankMenu menu, Inventory inventory, Component component) {
        super(menu, inventory, component);
        this.level = menu.level;

        //for (Direction direction : Direction.values()) {
        //    BlockEntity adjacentEntity = level.getBlockEntity(menu.blockEntity.getBlockPos().relative(direction));
        //    if (adjacentEntity instanceof TankBlockEntity tankBlockEntity) {
        //        fuelTankEntity = level.getBlockEntity(tankBlockEntity.getBlockPos());
        //        break;
        //    } else {
        //        fuelTankEntity = null;
        //    }
        //}
    }

    @Override
    protected void init() {
        super.init();
        addFluidWidgets();
    }

    private void addFluidWidgets() {
        addRenderableOnly(new FluidStackStackWidget(this, getMenu().blockEntity.fluidHandler,
                this.leftPos + 62, this.topPos + 21, 16, 45));
    }


    @Override
    protected void renderBg(GuiGraphics guiGraphics, float partialTicks, int mouseX, int mouseY) {
        RenderSystem.setShader(GameRenderer::getPositionTexShader);
        RenderSystem.setShaderColor(1.0F, 1.0F, 1.0F, 1.0F);
        RenderSystem.setShaderTexture(0, TEXTURE);

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
        renderTooltip(guiGraphics, mouseX, mouseY);
    }

}
