package com.benbenlaw.casting.screen.multiblock;

import com.benbenlaw.casting.Casting;
import com.benbenlaw.casting.block.CastingBlocks;
import com.benbenlaw.casting.block.entity.multiblock.MultiblockControllerBlockEntity;
import com.benbenlaw.casting.block.multiblock.MultiblockControllerBlock;
import com.benbenlaw.casting.network.payload.ControllerFilteredInventoryPayload;
import com.benbenlaw.casting.network.payload.OnOffButtonPayload;
import com.benbenlaw.casting.network.payload.SolidifierSelectedFluidPayload;
import com.benbenlaw.casting.screen.util.FluidStackStackWidget;
import com.benbenlaw.casting.screen.util.MultiFluidStackWidget;
import com.benbenlaw.casting.util.ConditionalSlotItemHandler;
import com.benbenlaw.core.screen.util.CoreButtons;
import com.benbenlaw.core.util.MouseUtil;
import com.mojang.blaze3d.systems.RenderSystem;
import net.minecraft.ChatFormatting;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.components.ImageButton;
import net.minecraft.client.gui.components.WidgetSprites;
import net.minecraft.client.gui.screens.inventory.AbstractContainerScreen;
import net.minecraft.client.renderer.GameRenderer;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.inventory.Slot;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.Items;
import net.minecraft.world.level.Level;
import net.neoforged.neoforge.fluids.FluidStack;
import net.neoforged.neoforge.network.PacketDistributor;
import org.jetbrains.annotations.Nullable;

import java.util.*;

public class MultiblockControllerScreen extends AbstractContainerScreen<MultiblockControllerMenu> {

    Level level;
    MultiblockControllerBlockEntity blockEntity;
    private boolean filteringEnabled = false;
    private static final ResourceLocation TEXTURE =
            ResourceLocation.fromNamespaceAndPath(Casting.MOD_ID, "textures/gui/multiblock_controller_gui.png");
    private static final ResourceLocation SLOT =
            ResourceLocation.fromNamespaceAndPath(Casting.MOD_ID, "textures/gui/slot.png");
    private static final ResourceLocation SLOT_DISABLED =
            ResourceLocation.fromNamespaceAndPath(Casting.MOD_ID, "textures/gui/slot_disabled.png");

    public MultiblockControllerScreen(MultiblockControllerMenu menu, Inventory inventory, Component component) {
        super(menu, inventory, component);
        this.level = menu.level;
        this.blockEntity = menu.blockEntity;
        this.imageHeight = 256;
        this.inventoryLabelY = 162;

    }


    @Override
    protected void init() {
        super.init();
        addFluidWidgets();
        addMenuButtons();
    }

    @Override
    public void containerTick() {
        this.clearWidgets();
        addFluidWidgets();
        addMenuButtons();
    }

    public void addFluidWidgets() {
        addRenderableOnly(new MultiFluidStackWidget(this, getMenu().blockEntity.fluidHandler,
                this.leftPos + 134, this.topPos + 114, 34, 45));

        if (getMenu().blockEntity.fuelTank != null) {
            addRenderableOnly(new FluidStackStackWidget(this, getMenu().blockEntity.fuelTank,
                    this.leftPos + 107, this.topPos + 114, 16, 45));
        }
    }


    @Override
    protected void renderBg(GuiGraphics guiGraphics, float partialTicks, int mouseX, int mouseY) {
        RenderSystem.setShader(GameRenderer::getPositionTexShader);
        RenderSystem.setShaderColor(1.0F, 1.0F, 1.0F, 1.0F);
        RenderSystem.setShaderTexture(0, TEXTURE);


        int x = (width - imageWidth) / 2;
        int y = (height - imageHeight) / 2;

        guiGraphics.blit(TEXTURE, x, y, 0, 0, imageWidth, imageHeight);

        //Draw the slots behind the texture
        for (Slot slot : menu.slots) {
            if (slot instanceof ConditionalSlotItemHandler conditionalSlotItemHandler) {

                if (conditionalSlotItemHandler.getContainerSlot() < menu.blockEntity.enabledSlots) {
                    guiGraphics.blit(SLOT, x + slot.x - 1, y + slot.y - 1, 0, 0, 18, 18, 18, 18);
                }
            }
        }

        //Render Error/ Working Buttons
        if (!menu.blockEntity.errorMessage.isEmpty()) {
          guiGraphics.blit(TEXTURE,x + 21, y - 17, 177, 18, 20, 18);
          renderErrorTooltip(guiGraphics, mouseX, mouseY, x, y);
        }
        if (menu.blockEntity.errorMessage.isEmpty()){
            guiGraphics.blit(TEXTURE, x + 21, y - 17, 177, 36, 20, 18);
            renderTickRate(guiGraphics, mouseX, mouseY, x, y);
        }

        //

        //Filter Button
        guiGraphics.blit(TEXTURE, x + 41, y - 17, 177, 54, 20, 18);
        renderFilterTooltip(guiGraphics, mouseX, mouseY, x, y);

        //Regulator Button
        guiGraphics.blit(TEXTURE, x + 61, y - 17, 177, 72, 20, 18);
        renderRegularTooltip(guiGraphics, mouseX, mouseY, x, y);

        //Progress bar
        renderProgressBars(guiGraphics, x, y);


    }

    @Override
    public void render(GuiGraphics guiGraphics, int mouseX, int mouseY, float partialTicks) {

        int x = (width - imageWidth) / 2;
        int y = (height - imageHeight) / 2;

        renderBackground(guiGraphics, mouseX, mouseY, partialTicks);
        super.render(guiGraphics, mouseX, mouseY, partialTicks);

        // Draw in front of menu (your grayed-out filtered items)
        for (Slot slot : menu.slots) {
            if (slot instanceof ConditionalSlotItemHandler conditionalSlotItemHandler) {

                if (conditionalSlotItemHandler.getContainerSlot() >= menu.blockEntity.enabledSlots) {
                    guiGraphics.blit(SLOT_DISABLED, x + slot.x - 1, y + slot.y - 1, 18, 18, 18, 18);
                }

                renderFilterInfo(guiGraphics, x, y, slot.getContainerSlot());
            }
        }

        if (getMenu().blockEntity.fuelTank == null) {
            renderNoTank(guiGraphics, mouseX, mouseY, x, y);
        }

        renderProgressBars(guiGraphics, x, y);
        renderOnMessage(guiGraphics, mouseX, mouseY, x, y);
        // *** IMPORTANT: Reset shader color before rendering tooltips ***
        RenderSystem.setShaderColor(1.0f, 1.0f, 1.0f, 1.0f);

        // Now render tooltips without gray shader applied
        renderTooltip(guiGraphics, mouseX, mouseY);
    }


    private void renderProgressBars(GuiGraphics guiGraphics, int x, int y) {
        RenderSystem.setShader(GameRenderer::getPositionTexShader);
        RenderSystem.setShaderColor(1.0F, 1.0F, 1.0F, 1.0F);
        RenderSystem.setShaderTexture(0, TEXTURE);

        for (Slot slot : menu.slots) {
            if (slot instanceof ConditionalSlotItemHandler conditionalSlotItemHandler) {
                int containerSlot = conditionalSlotItemHandler.getContainerSlot();
                int progress = menu.getScaledProgress(containerSlot); // vertical height in pixels

                if (progress > 0) {
                    int slotX = slot.x;
                    int slotY = slot.y;

                    // Draw from bottom up: adjust both texture V and screen Y
                    int fullHeight = 18; // or however tall your progress bar is
                    guiGraphics.blit(
                            TEXTURE,
                            x + slotX -1,
                            y + slotY + (fullHeight - progress) - 1, // destination Y
                            176,                         // texture U
                            fullHeight - progress,       // texture V
                            18,                          // width
                            progress                     // height
                    );

                }
            }
        }
    }

    private void addMenuButtons() {
        int buttonX = this.leftPos;
        int buttonY = this.topPos - 17;

        if (this.menu.blockEntity != null) {
            WidgetSprites buttonTexture = this.menu.blockEntity.getBlockState().getValue(MultiblockControllerBlock.ENABLED)
                    ? CoreButtons.ON_BUTTONS
                    : CoreButtons.OFF_BUTTONS;
            this.addRenderableWidget(new ImageButton(buttonX, buttonY, 20, 18, buttonTexture, (pressed) ->
                    PacketDistributor.sendToServer(new OnOffButtonPayload(this.menu.blockEntity.getBlockPos()))));
        }

    }

    private void renderFilterInfo(GuiGraphics guiGraphics, int x, int y, int containerSlot) {
        Item filteredItem = menu.blockEntity.getAllowedItem(containerSlot);

        if (filteredItem != null) {
            ItemStack fakeStack = new ItemStack(filteredItem);

            // Find the actual slot
            for (Slot slot : menu.slots) {
                // Only render filtered ghost if the slot is EMPTY
                if (slot.getItem().isEmpty()) {
                    if (slot instanceof ConditionalSlotItemHandler cSlot && cSlot.getContainerSlot() == containerSlot) {
                        RenderSystem.setShaderColor(0.7f, 0.7f, 0.7f, 0.5f);
                        guiGraphics.renderFakeItem(fakeStack, x + slot.x, y + slot.y);
                        RenderSystem.setShaderColor(1.0f, 1.0f, 1.0f, 1.0f);
                        break;
                    }
                }
            }
        }
    }

    @Nullable
    private void renderTickRate(GuiGraphics guiGraphics, int mouseX, int mouseY, int x, int y) {
        if (MouseUtil.isMouseAboveArea(mouseX, mouseY, x, y, 20, -17, 19, 18)) {
            int[] progressArray = this.menu.blockEntity.progress;
            int[] maxProgressArray = this.menu.blockEntity.maxProgress;

            int maxIndex = -1;
            int highestMax = -1;

            for (int i = 0; i < maxProgressArray.length; i++) {
                if (maxProgressArray[i] > highestMax) {
                    highestMax = maxProgressArray[i];
                    maxIndex = i;
                }
            }

            if (highestMax <= 0) {
                guiGraphics.renderTooltip(this.font, Component.translatable("gui.casting.buttons.waiting_for_recipe"), mouseX, mouseY);
            } else {
                int currentProgress = progressArray[maxIndex];
                guiGraphics.renderTooltip(this.font, Component.literal(currentProgress + " / " + highestMax), mouseX, mouseY);
            }
        }
    }

    private int getSlotX(int slot) {
        switch (slot) {
            case 0: case 5: case 10: return 12;
            case 1: case 6: case 11: return 31;
            case 2: case 7: case 12: return 50;
            case 3: case 8: case 13: return 69;
            case 4: case 9: case 14: return 88;
            default: return 0;
        }
    }

    private int getSlotY(int slot) {
        if (slot >= 0 && slot <= 4) return 20;
        if (slot >= 5 && slot <= 9) return 39;
        if (slot >= 10 && slot <= 14) return 58;
        return 0;
    }
    private void renderNoTank(GuiGraphics guiGraphics, int mouseX, int mouseY, int x, int y) {
        if (MouseUtil.isMouseAboveArea(mouseX, mouseY, x, y, 107,  114, 16, 45)) {
            guiGraphics.renderTooltip(this.font, Component.translatable("gui.casting.multiblock_controller.no_fuel_tank").withStyle(ChatFormatting.RED), mouseX, mouseY);
        }
    }

    private void renderOnMessage(GuiGraphics guiGraphics, int mouseX, int mouseY, int x, int y) {
        if (!this.menu.blockEntity.getBlockState().getValue(MultiblockControllerBlock.ENABLED)) {
            if (MouseUtil.isMouseAboveArea(mouseX, mouseY, x, y, 3, 3, 169, 158)) {
                guiGraphics.renderTooltip(this.font, Component.translatable("gui.casting.multiblock_controller.off"), mouseX, mouseY);
            }
        }
    }

    private void renderErrorTooltip(GuiGraphics guiGraphics, int mouseX, int mouseY, int x, int y) {
        if (MouseUtil.isMouseAboveArea(mouseX, mouseY, x, y, 20, -17, 19, 18)) {
            guiGraphics.renderTooltip(this.font, Component.translatable("gui.casting.buttons." + this.menu.blockEntity.errorMessage), mouseX, mouseY);
        }
    }

    private void renderFilterTooltip(GuiGraphics guiGraphics, int mouseX, int mouseY, int x, int y) {
        if (MouseUtil.isMouseAboveArea(mouseX, mouseY, x, y, 40, -17, 19, 18)) {
            List<Component> tooltipLines = new ArrayList<>();

            if (filteringEnabled) {
                tooltipLines.add(Component.translatable("gui.casting.buttons.filtering_enabled"));

                Map<Item, Integer> itemCounts = new HashMap<>();

                for (int slot = 0; slot < menu.blockEntity.enabledSlots; slot++) {
                    Item filteredItem = menu.blockEntity.getAllowedItem(slot);
                    if (filteredItem != null && filteredItem != Items.AIR) {
                        int count = menu.blockEntity.itemHandler.getStackInSlot(slot).getCount();
                        itemCounts.put(filteredItem, itemCounts.getOrDefault(filteredItem, 0) + count);
                    }
                }

                // Add aggregated items to tooltip
                for (Map.Entry<Item, Integer> entry : itemCounts.entrySet()) {
                    String itemName = entry.getKey().getName(new ItemStack(entry.getKey())).getString();
                    int totalCount = entry.getValue();
                    tooltipLines.add(Component.literal("- " + itemName + " × " + totalCount));
                }

            } else {
                tooltipLines.add(Component.translatable("gui.casting.buttons.filtering_disabled"));
            }

            guiGraphics.renderTooltip(this.font, tooltipLines, Optional.empty(), mouseX, mouseY);
        }
    }

    private void renderRegularTooltip(GuiGraphics guiGraphics, int mouseX, int mouseY, int x, int y) {
        if (MouseUtil.isMouseAboveArea(mouseX, mouseY, x, y, 60, -17, 19, 18)) {

            int regulatorCount = menu.blockEntity.regulatorCount;

            if (regulatorCount == 0) {
                guiGraphics.renderTooltip(this.font, Component.translatable("gui.casting.buttons.no_regulators").withStyle(ChatFormatting.RED), mouseX, mouseY);
            }
            else {
                int space = menu.blockEntity.fluidHandler.getEnabledCapacity() / regulatorCount;

                guiGraphics.renderTooltip(this.font,
                        Component.translatable("gui.casting.buttons.regulator_count", space, regulatorCount),
                        mouseX, mouseY);
            }
        }

        guiGraphics.renderItem(new ItemStack(CastingBlocks.MULTIBLOCK_REGULATOR.asItem()), x + 62, y - 16);;

    }



    @Override
    public boolean mouseClicked(double mouseX, double mouseY, int mouseButton) {
        boolean handled = super.mouseClicked(mouseX, mouseY, mouseButton);

        if (MouseUtil.isMouseAboveArea((int) mouseX, (int) mouseY, leftPos + 41, topPos - 17, 0, 0, 19, 18)) {
            if (!filteringEnabled) {
                for (int slot = 0; slot < menu.blockEntity.enabledSlots; slot++) {
                    ItemStack item = menu.blockEntity.itemHandler.getStackInSlot(slot);
                    if (!item.isEmpty()) {
                        PacketDistributor.sendToServer(new ControllerFilteredInventoryPayload(
                                slot, item, menu.blockEntity.getBlockPos()));
                    }
                }
                filteringEnabled = true;
            } else {
                for (int slot = 0; slot < menu.blockEntity.enabledSlots; slot++) {
                    PacketDistributor.sendToServer(new ControllerFilteredInventoryPayload(
                            slot, ItemStack.EMPTY, menu.blockEntity.getBlockPos()));
                }
                filteringEnabled = false;
            }
        }

        return handled;
    }


}