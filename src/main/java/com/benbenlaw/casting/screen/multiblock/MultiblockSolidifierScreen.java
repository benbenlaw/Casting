package com.benbenlaw.casting.screen.multiblock;

import com.benbenlaw.casting.Casting;
import com.benbenlaw.casting.block.multiblock.MultiblockSolidifierBlock;
import com.benbenlaw.casting.network.payload.OnOffButtonPayload;
import com.benbenlaw.casting.network.payload.SolidifierSelectedFluidPayload;
import com.benbenlaw.casting.network.payload.ValveSelectedFluidPayload;
import com.benbenlaw.casting.screen.SolidifierScreen;
import com.benbenlaw.casting.screen.util.FluidStackStackWidget;
import com.benbenlaw.casting.screen.util.MultiFluidStackWidget;
import com.benbenlaw.core.screen.util.CoreButtons;
import com.benbenlaw.core.util.MouseUtil;
import com.mojang.blaze3d.systems.RenderSystem;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.components.ImageButton;
import net.minecraft.client.gui.components.WidgetSprites;
import net.minecraft.client.gui.screens.inventory.AbstractContainerScreen;
import net.minecraft.client.renderer.GameRenderer;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.Items;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.entity.BlockEntity;
import net.minecraft.world.level.material.Fluid;
import net.neoforged.neoforge.fluids.FluidStack;
import net.neoforged.neoforge.network.PacketDistributor;

import java.util.List;

public class MultiblockSolidifierScreen extends AbstractContainerScreen<MultiblockSolidifierMenu> {

    private Level level;
    private BlockEntity fuelTankEntity;
    private static final ResourceLocation TEXTURE =
            ResourceLocation.fromNamespaceAndPath(Casting.MOD_ID, "textures/gui/multiblock_solidifier_gui.png");

    private int selectedFluidIndex = 0;

    public MultiblockSolidifierScreen(MultiblockSolidifierMenu menu, Inventory inventory, Component component) {
        super(menu, inventory, component);
        this.level = menu.level;
    }

    @Override
    protected void init() {
        super.init();
        addFluidWidgets();
        addMenuButtons();
    }

    @Override
    protected void containerTick() {
        this.clearWidgets();
        addFluidWidgets();
        addMenuButtons();
    }

    private void addFluidWidgets() {

        //Render Main Tank
        if (getMenu().blockEntity.controller != null) {
            addRenderableOnly(new MultiFluidStackWidget(this, getMenu().blockEntity.controller.fluidHandler,
                    this.leftPos + 35, this.topPos + 21, 34, 45));
        }

        //Render Coolant Tank
        if (getMenu().blockEntity.coolantTank != null) {
            addRenderableOnly(new FluidStackStackWidget(this, getMenu().blockEntity.coolantTank,
                    this.leftPos + 8, this.topPos + 21, 16, 45));
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

        //Render Button Information
        guiGraphics.blit(TEXTURE, x + 21, y - 17, 177, 35, 20, 18);

        //Render Current Fluid
        renderBucket(guiGraphics, mouseX, mouseY, x, y);
        guiGraphics.blit(TEXTURE, x + 41, y - 17, 177, 53, 20, 18);


    }

    @Override
    public void render(GuiGraphics guiGraphics, int mouseX, int mouseY, float partialTicks) {

        int x = (width - imageWidth) / 2;
        int y = (height - imageHeight) / 2;

        renderBackground(guiGraphics, mouseX, mouseY, partialTicks);

        super.render(guiGraphics, mouseX, mouseY, partialTicks);
        renderProgressBars(guiGraphics);

        renderTooltip(guiGraphics, mouseX, mouseY);
        renderNoControllerTooltip(guiGraphics, mouseX, mouseY, x, y);
        renderSlotTooltips(guiGraphics, mouseX, mouseY, x, y);

    }

    private void renderNoControllerTooltip(GuiGraphics guiGraphics, int mouseX, int mouseY, int x, int y) {
        if (menu.blockEntity.controller == null) {
            if (MouseUtil.isMouseAboveArea(mouseX, mouseY, this.leftPos + 35, this.topPos + 21, 0, -1, 34, 45)) {
                guiGraphics.renderTooltip(this.font, Component.translatable("gui.casting.multiblock_controller.no_controller"), mouseX, mouseY);
            }
        }
    }


    private void renderProgressBars(GuiGraphics guiGraphics) {
        int x = leftPos;
        int y = topPos;
        guiGraphics.blit(TEXTURE, x + 121, y + 35, 176, 0, menu.getScaledProgress(), 16);
    }

    @Override
    public boolean mouseClicked(double mouseX, double mouseY, int mouseButton) {
        boolean handled = super.mouseClicked(mouseX, mouseY, mouseButton);

        int widgetWidth = 34;
        int widgetHeight = 45;

        if (MouseUtil.isMouseAboveArea((int) mouseX, (int) mouseY, leftPos + 41, topPos - 17, 0, 0, widgetWidth, widgetHeight)) {

            List<FluidStack> fluids = this.menu.blockEntity.controller.fluidHandler.getFluids();

            if (MultiblockSolidifierScreen.hasShiftDown()) {
                selectedFluidIndex = 0;
                PacketDistributor.sendToServer(new SolidifierSelectedFluidPayload("minecraft:empty", menu.blockEntity.getBlockPos()));
            }

            else if (!fluids.isEmpty()) {
                if (mouseButton == 0) {
                    selectedFluidIndex = (selectedFluidIndex + 1) % fluids.size();
                    String selectedFluid = fluids.get(selectedFluidIndex).getFluid().toString();
                    PacketDistributor.sendToServer(new SolidifierSelectedFluidPayload(selectedFluid, menu.blockEntity.getBlockPos()));
                }
                else if (mouseButton == 1) {
                    selectedFluidIndex = (selectedFluidIndex - 1 + fluids.size()) % fluids.size();
                    String selectedFluid = fluids.get(selectedFluidIndex).getFluid().toString();
                    PacketDistributor.sendToServer(new SolidifierSelectedFluidPayload(selectedFluid, menu.blockEntity.getBlockPos()));
                }
            }
        }
        return handled;
    }

    private void renderSlotTooltips(GuiGraphics guiGraphics, int mouseX, int mouseY, int x, int y) {

        //On Off Button Tooltip
        if (MouseUtil.isMouseAboveArea(mouseX, mouseY, x, y, 0, -17, 19, 18)) {
            guiGraphics.renderTooltip(this.font, Component.translatable("gui.casting.buttons.no_off"), mouseX, mouseY);
        }

        //Tick
        if (MouseUtil.isMouseAboveArea(mouseX, mouseY, x, y, 20, -17, 19, 18)) {

            String progressInfo = (this.menu.blockEntity.progress + "/" + this.menu.blockEntity.maxProgress);

            if (progressInfo.equals("0/0")) {
                guiGraphics.renderTooltip(this.font, Component.translatable("gui.casting.buttons.waiting_for_recipe"), mouseX, mouseY);
            } else {
                guiGraphics.renderTooltip(this.font, Component.literal(this.menu.blockEntity.progress + "/" +
                        this.menu.blockEntity.maxProgress), mouseX, mouseY);
            }
        }

        //Fluid
        if (MouseUtil.isMouseAboveArea(mouseX, mouseY, x, y, 40, -17, 19, 18)) {
            String selectedFluid = this.menu.blockEntity.selectedFluidString;
            Fluid fluid = BuiltInRegistries.FLUID.get(ResourceLocation.parse(selectedFluid));
            ItemStack bucketOfFluid = new ItemStack(fluid.getBucket());
            String bucketName = bucketOfFluid.getHoverName().getString();
            Component fluidName = Component.nullToEmpty(bucketName.replace(" Bucket", ""));

            if (fluidName.contains(Component.nullToEmpty("Air"))) {
                guiGraphics.renderTooltip(this.font, Component.translatable("gui.casting.buttons.fluid"), mouseX, mouseY);
            } else {
                guiGraphics.renderTooltip(this.font, Component.translatable("gui.casting.buttons.remove_fluid_right_click", fluidName), mouseX, mouseY);

            }

        }
    }

    private void renderBucket(GuiGraphics guiGraphics, int mouseX, int mouseY, int x, int y) {
        String selectedFluid = this.menu.blockEntity.selectedFluidString;
        Fluid fluid = BuiltInRegistries.FLUID.get(ResourceLocation.parse(selectedFluid));
        ItemStack bucketOfFluid = new ItemStack(fluid.getBucket());

        if (bucketOfFluid.isEmpty()) {
            guiGraphics.renderItem(new ItemStack(Items.BUCKET), x + 42, y - 16);;
        } else {
            guiGraphics.renderItem(bucketOfFluid, x + 42, y - 16);
        }
    }

    private void addMenuButtons() {
        int buttonX = this.leftPos;
        int buttonY = this.topPos - 17;

        if (this.menu.blockEntity != null) {
            WidgetSprites buttonTexture = this.menu.blockEntity.getBlockState().getValue(MultiblockSolidifierBlock.ENABLED)
                    ? CoreButtons.ON_BUTTONS
                    : CoreButtons.OFF_BUTTONS;
            this.addRenderableWidget(new ImageButton(buttonX, buttonY, 20, 18, buttonTexture, (pressed) ->
                    PacketDistributor.sendToServer(new OnOffButtonPayload(this.menu.blockEntity.getBlockPos()))));
        }

    }
}
