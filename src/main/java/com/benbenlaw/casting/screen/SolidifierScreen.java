package com.benbenlaw.casting.screen;

import com.benbenlaw.casting.Casting;
import com.benbenlaw.casting.network.packet.ChangeMoldPagePacket;
import com.benbenlaw.core.Core;
import com.benbenlaw.core.screen.util.DurationTooltip;
import com.benbenlaw.core.screen.util.FluidRenderingUtils;
import net.minecraft.client.gui.GuiGraphicsExtractor;
import net.minecraft.client.gui.components.Button;
import net.minecraft.client.gui.screens.inventory.AbstractContainerScreen;
import net.minecraft.client.renderer.RenderPipelines;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.Identifier;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.item.ItemStack;
import net.neoforged.neoforge.client.network.ClientPacketDistributor;
import net.neoforged.neoforge.transfer.ResourceHandler;
import net.neoforged.neoforge.transfer.item.ItemResource;
import net.neoforged.neoforge.transfer.item.ItemUtil;

import java.util.ArrayList;
import java.util.List;

public class SolidifierScreen extends AbstractContainerScreen<SolidifierMenu> {

    private static final Identifier TEXTURE = Casting.identifier("textures/gui/solidifier_gui.png");
    private static final Identifier PROGRESS_ARROW = Core.identifier("progress_arrow");

    private final List<ItemStack> molds = new ArrayList<>();
    private final List<Integer> moldSlotIndices = new ArrayList<>();
    private int page = 0;

    public SolidifierScreen(SolidifierMenu menu, Inventory inventory, Component title) {
        super(menu, inventory, title);
    }

    @Override
    protected void init() {
        super.init();

        int x = (width - imageWidth) / 2;
        int y = (height - imageHeight) / 2;

        populateMolds();
        ItemStack currentInput = ItemUtil.getStack(
                menu.blockEntity.getItemHandler(), 0);
        if (!currentInput.isEmpty()) {
            for (int i = 0; i < molds.size(); i++) {
                if (ItemStack.isSameItemSameComponents(molds.get(i), currentInput)) {
                    page = i;
                    break;
                }
            }
        }

        addRenderableWidget(Button.builder(Component.literal("<"), b -> {
            int max = menu.getMaxMoldPage();

            page--;
            if (page < 0) page = max;

            ClientPacketDistributor.sendToServer(
                    new ChangeMoldPagePacket(menu.blockEntity.getBlockPos(), page)
            );

            menu.setMoldPage(page);
        }).bounds(x + 32, y + 54, 10, 10).build());

        addRenderableWidget(Button.builder(Component.literal(">"), b -> {
            int max = menu.getMaxMoldPage();

            page++;
            if (page > max) page = 0;

            ClientPacketDistributor.sendToServer(
                    new ChangeMoldPagePacket(menu.blockEntity.getBlockPos(), page)
            );

            menu.setMoldPage(page);
        }).bounds(x + 134, y + 54, 10, 10).build());


    }

    public void populateMolds() {
        ResourceHandler<ItemResource> moldHandler = menu.blockEntity.getStoredMolds();

        molds.clear();
        moldSlotIndices.clear();

        for (int i = 0; i < moldHandler.size(); i++) {
            ItemStack stack = ItemUtil.getStack(moldHandler, i);
            if (!stack.isEmpty()) {
                molds.add(stack);
                moldSlotIndices.add(i);
            }
        }

        if (molds.isEmpty()) {
            page = 0;
            return;
        }

        if (page >= molds.size()) {
            page = 0;
        }
    }

    @Override
    public void extractBackground(GuiGraphicsExtractor guiGraphics, int mouseX, int mouseY, float a) {
        super.extractBackground(guiGraphics, mouseX, mouseY, a);

        int x = (width - imageWidth) / 2;
        int y = (height - imageHeight) / 2;

        guiGraphics.blit(RenderPipelines.GUI_TEXTURED, TEXTURE, x, y, 0, 0, imageWidth, imageHeight, 256, 256);

        if (menu.isCrafting()) {
            guiGraphics.blitSprite(RenderPipelines.GUI_TEXTURED, PROGRESS_ARROW, 24, 16, 0, 0,
                    x + 76, y + 19, menu.getScaledProgress() + 1, 16);
        }
    }

    @Override
    public void extractRenderState(GuiGraphicsExtractor guiGraphics, int mouseX, int mouseY, float partialTick) {
        super.extractRenderState(guiGraphics, mouseX, mouseY, partialTick);

        int x = (width - imageWidth) / 2;
        int y = (height - imageHeight) / 2;

        guiGraphics.text(font, Component.translatable("tooltip.casting.storage_molds"), x + 44, y + 40, 0xFF404040, false);

        DurationTooltip.renderDurationTooltip(guiGraphics, mouseX, mouseY, x, y, 161, 5,
                menu.data.get(0), menu.data.get(1));

        FluidRenderingUtils.renderFluid(guiGraphics, menu.blockEntity.getFluidHandler(), 0, x, y,
                8, 44, 23, 16, mouseX, mouseY, Component.translatable("tooltip.casting.empty"));

        FluidRenderingUtils.renderFluid(guiGraphics, menu.blockEntity.getFilterFluidHandler(), 0, x, y,
                8, 20, 16, 16, mouseX, mouseY, Component.translatable("tooltip.casting.empty_filter"));

        FluidRenderingUtils.renderFluid(guiGraphics, menu.blockEntity.fuelStack, 4000, x, y,
                152, 51, 16, 16, mouseX, mouseY, Component.translatable("tooltip.casting.no_coolant"));
    }
}