package com.benbenlaw.casting.integration.jei;

import com.benbenlaw.casting.Casting;
import com.benbenlaw.casting.block.CastingBlocks;
import com.benbenlaw.casting.recipe.MixingRecipe;
import com.benbenlaw.core.recipe.NoInventoryRecipe;
import mezz.jei.api.constants.VanillaTypes;
import mezz.jei.api.gui.builder.IRecipeLayoutBuilder;
import mezz.jei.api.gui.drawable.IDrawable;
import mezz.jei.api.helpers.IGuiHelper;
import mezz.jei.api.recipe.IFocusGroup;
import mezz.jei.api.recipe.RecipeIngredientRole;
import mezz.jei.api.recipe.RecipeType;
import mezz.jei.api.recipe.category.IRecipeCategory;
import net.minecraft.ChatFormatting;
import net.minecraft.client.Minecraft;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.crafting.RecipeHolder;
import net.minecraft.world.level.Level;
import net.neoforged.neoforge.fluids.FluidStack;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;

public class MixingRecipeCategory implements IRecipeCategory<MixingRecipe> {
    public final static ResourceLocation UID = ResourceLocation.fromNamespaceAndPath(Casting.MOD_ID, "mixing");
    public final static ResourceLocation TEXTURE =
            ResourceLocation.fromNamespaceAndPath(Casting.MOD_ID, "textures/gui/jei_mixer.png");

    public static final RecipeType<MixingRecipe> RECIPE_TYPE = RecipeType.create(Casting.MOD_ID, "mixing",
            MixingRecipe.class);

    private IDrawable background;
    private final IDrawable icon;
    private final IGuiHelper helper;

    public MixingRecipeCategory(IGuiHelper helper) {
        this.helper = helper;
        this.background = helper.createDrawable(TEXTURE, 0, 0, 159, 38);
        this.icon = helper.createDrawableIngredient(VanillaTypes.ITEM_STACK, new ItemStack(CastingBlocks.MULTIBLOCK_MIXER.get()));
    }

    @Override
    public @NotNull RecipeType<MixingRecipe> getRecipeType() {
        return JEICastingPlugin.MIXER_RECIPE;
    }

    @Override
    public @NotNull Component getTitle() {
        return Component.translatable("gui.casting.jei.mixing");
    }

    @Override
    public @NotNull IDrawable getBackground() {
        return this.background;
    }

    @Override
    public @NotNull IDrawable getIcon() {
        return this.icon;
    }

    @Override
    public @Nullable ResourceLocation getRegistryName(MixingRecipe recipe) {
        assert Minecraft.getInstance().level != null;
        return Minecraft.getInstance().level.getRecipeManager().getAllRecipesFor(MixingRecipe.Type.INSTANCE).stream()
                .filter(recipeHolder -> recipeHolder.value().equals(recipe))
                .map(RecipeHolder::id)
                .findFirst()
                .orElse(null);
    }

    @Override
    public void setRecipe(@NotNull IRecipeLayoutBuilder builder, MixingRecipe recipe, @NotNull IFocusGroup focusGroup) {

        Level level = Minecraft.getInstance().level;
        List<RecipeHolder<MixingRecipe>> recipeHolder = level.getRecipeManager().getRecipesFor(MixingRecipe.Type.INSTANCE, NoInventoryRecipe.INSTANCE, level);

        int size = recipe.fluids().size();
        int centerX = size > 0 ? 1 : 10;
        int centerY = size > 6 ? 2 : 11;
        int xOffset = 0;
        int yOffset = 0;
        int index = 0;

        for (int i = 0; i < size; i++) {
            xOffset = centerX + (i % 6) * 18;
            yOffset = centerY + ((i / 6) * 18);
            index = i;

            int finalIndex = index;
            builder.addSlot(RecipeIngredientRole.INPUT, 3 + xOffset, yOffset)
                    .addFluidStack(recipe.getAllFluids().get(i).getFluid(), recipe.getAllFluids().get(finalIndex).getAmount())
                    .addRichTooltipCallback((slot, tooltip) ->
                            tooltip.add(Component.literal(recipe.getAllFluids().get(finalIndex).getAmount() + " mB").withStyle(ChatFormatting.GOLD)))
                    .setBackground(JEICastingPlugin.slotDrawable, -1, -1);

            builder.addSlot(RecipeIngredientRole.RENDER_ONLY, 3 + xOffset, yOffset).addFluidStack(recipe.getAllFluids().get(i).getFluid(), 1000);
        }

        FluidStack output = recipe.outputFluid().copy();
        if (!output.isEmpty()) {
            builder.addSlot(RecipeIngredientRole.OUTPUT, 140, 11)
                    .addFluidStack(output.getFluid(), output.getAmount())
                    .addRichTooltipCallback((slot, tooltip) ->
                            tooltip.add(Component.literal(output.getAmount() + " mB").withStyle(ChatFormatting.GOLD)));

            builder.addSlot(RecipeIngredientRole.RENDER_ONLY, 140, 11).addFluidStack(output.getFluid(), 1000);
        }
    }

    /*

    @Override
    public void draw(MixingRecipe recipe, IRecipeSlotsView recipeSlotsView, GuiGraphics guiGraphics, double mouseX, double mouseY) {
        final Minecraft minecraft = Minecraft.getInstance();

        String amountText = null;

        // Define the slot positions and sizes
        int[][] slotAreas = {
                {4, 20, 16, 16},
                {23, 20, 16, 16},
                {42, 20, 16, 16},
                {61, 20, 16, 16},
                {80, 20, 16, 16},
                {99, 20, 16, 16},
                {142, 20, 16, 16}
        };

        List<FluidStack> fluids = new ArrayList<>(recipe.fluids());
        fluids.add(recipe.outputFluid());

        // Determine which slot the mouse is over and set the amount text accordingly
        for (int i = 0; i < slotAreas.length; i++) {
            int[] area = slotAreas[i];
            int slotX = area[0];
            int slotY = area[1];
            int slotWidth = area[2];
            int slotHeight = area[3];

            if (mouseX >= slotX && mouseX < slotX + slotWidth && mouseY >= slotY && mouseY < slotY + slotHeight) {
                FluidStack fluid = fluids.get(i);
                if (i == 6) { // Output fluid
                    if (!fluid.isEmpty()) {
                        int amount = fluid.getAmount();
                        amountText = "Fluid Produced: " + amount + "mB";
                    } else {
                        amountText = "No Output Fluid";
                    }
                } else { // Input fluids
                    if (!fluid.isEmpty()) {
                        int amount = fluid.getAmount();
                        amountText = "Fluid Required: " + amount + "mB";
                    } else {
                        amountText = "No Fluid Needed";
                    }
                }
                break;
            }
        }

        // Draw the text if there's something to display
        if (amountText != null) {
            int textX = 3; // X coordinate for the text
            int textY = 2; // Y coordinate for the text

            guiGraphics.drawString(minecraft.font.self(), Component.literal(amountText), textX, textY, Color.GRAY.getRGB(), false);
        }
    }

     */


}