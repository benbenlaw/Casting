package com.benbenlaw.casting.item;

import com.benbenlaw.casting.Casting;
import com.benbenlaw.casting.block.ModBlocks;
import com.benbenlaw.casting.fluid.CastingFluids;
import net.minecraft.core.registries.Registries;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceKey;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.item.CreativeModeTab;
import net.minecraft.world.item.CreativeModeTabs;
import net.neoforged.bus.api.IEventBus;
import net.neoforged.neoforge.registries.DeferredHolder;
import net.neoforged.neoforge.registries.DeferredRegister;

import java.util.function.Supplier;

public class ModCreativeModTab {

    public static final DeferredRegister<CreativeModeTab> CREATIVE_MODE_TABS = DeferredRegister.create(Registries.CREATIVE_MODE_TAB, Casting.MOD_ID);

    public static final Supplier<CreativeModeTab> TOOL_MODIFIERS_TAB = CREATIVE_MODE_TABS.register("tool_modifiers", () -> CreativeModeTab.builder()
            .icon(() -> ToolModifierItems.SILK_TOUCH.get().getDefaultInstance())
            .title(Component.translatable("itemGroup.tool_modifiers"))
            .withTabsBefore(CreativeModeTabs.COMBAT)
            .displayItems((parameters, output) -> {
                output.accept(ToolModifierItems.SILK_TOUCH);
                output.accept(ToolModifierItems.EFFICIENCY);
                output.accept(ToolModifierItems.FORTUNE);
                output.accept(ToolModifierItems.UNBREAKING);
                output.accept(ToolModifierItems.REPAIRING);
                output.accept(ToolModifierItems.TORCH_PLACING);
                output.accept(ToolModifierItems.AUTO_SMELT);

            }).build());

    public static final Supplier<CreativeModeTab> CASTING_TAB = CREATIVE_MODE_TABS.register("casting", () -> CreativeModeTab.builder()
            .icon(() -> ModItems.PLATE_MOLD.get().getDefaultInstance())
            .withTabsBefore(CreativeModeTabs.COMBAT, ResourceKey.create(Registries.CREATIVE_MODE_TAB, ResourceLocation.fromNamespaceAndPath(Casting.MOD_ID, "tool_modifiers")))
            .title(Component.translatable("itemGroup.casting"))
            .displayItems((parameters, output) -> {

                output.accept(ModItems.GEAR_MOLD);
                output.accept(ModItems.PLATE_MOLD);
                output.accept(ModItems.INGOT_MOLD);
                output.accept(ModItems.NUGGET_MOLD);
                output.accept(ModItems.ROD_MOLD);
                output.accept(ModItems.BLOCK_MOLD);
                output.accept(ModItems.GEM_MOLD);
                output.accept(ModItems.DUST_MOLD);
                output.accept(ModItems.BALL_MOLD);
                output.accept(ModItems.BLACK_BRICK);
                output.accept(ModBlocks.BLACK_BRICKS);
                output.accept(ModItems.FLUID_MOVER);

                output.accept(CastingFluids.MOLTEN_BRONZE.getBucket());
                output.accept(CastingFluids.MOLTEN_OBSIDIAN.getBucket());
                output.accept(CastingFluids.MOLTEN_STEEL.getBucket());
                output.accept(CastingFluids.MOLTEN_NETHERITE.getBucket());
                output.accept(CastingFluids.MOLTEN_ELECTRUM.getBucket());
                output.accept(CastingFluids.MOLTEN_INVAR.getBucket());
                output.accept(CastingFluids.MOLTEN_IRON.getBucket());
                output.accept(CastingFluids.MOLTEN_GOLD.getBucket());
                output.accept(CastingFluids.MOLTEN_COPPER.getBucket());
                output.accept(CastingFluids.MOLTEN_TIN.getBucket());
                output.accept(CastingFluids.MOLTEN_LEAD.getBucket());
                output.accept(CastingFluids.MOLTEN_SILVER.getBucket());
                output.accept(CastingFluids.MOLTEN_NICKEL.getBucket());
                output.accept(CastingFluids.MOLTEN_OSMIUM.getBucket());
                output.accept(CastingFluids.MOLTEN_QUARTZ.getBucket());
                output.accept(CastingFluids.MOLTEN_LAPIS.getBucket());
                output.accept(CastingFluids.MOLTEN_REDSTONE.getBucket());
                output.accept(CastingFluids.MOLTEN_DIAMOND.getBucket());
                output.accept(CastingFluids.MOLTEN_EMERALD.getBucket());
                output.accept(CastingFluids.MOLTEN_URANIUM.getBucket());
                output.accept(CastingFluids.MOLTEN_GLASS.getBucket());
                output.accept(CastingFluids.MOLTEN_DEBRIS.getBucket());
                output.accept(CastingFluids.MOLTEN_STONE.getBucket());
                output.accept(CastingFluids.MOLTEN_COAL.getBucket());
                output.accept(CastingFluids.MOLTEN_ALUMINUM.getBucket());
                output.accept(CastingFluids.MOLTEN_ZINC.getBucket());
                output.accept(CastingFluids.MOLTEN_PLATINUM.getBucket());
                output.accept(CastingFluids.MOLTEN_IRIDIUM.getBucket());
                output.accept(CastingFluids.MOLTEN_GLOWSTONE.getBucket());
                output.accept(CastingFluids.MOLTEN_ENDER.getBucket());
                output.accept(CastingFluids.MOLTEN_CONSTANTAN.getBucket());
                output.accept(CastingFluids.MOLTEN_BRASS.getBucket());
                output.accept(CastingFluids.MOLTEN_SIGNALUM.getBucket());
                output.accept(CastingFluids.MOLTEN_LUMIUM.getBucket());
                output.accept(CastingFluids.MOLTEN_ENDERIUM.getBucket());

                output.accept(CastingFluids.MOLTEN_CONDUCTIVE_ALLOY.getBucket());
                output.accept(CastingFluids.MOLTEN_SILICON.getBucket());
                output.accept(CastingFluids.MOLTEN_SOUL.getBucket());
                output.accept(CastingFluids.MOLTEN_END_STONE.getBucket());
                output.accept(CastingFluids.MOLTEN_SOULARIUM.getBucket());
                output.accept(CastingFluids.MOLTEN_DARK_STEEL.getBucket());
                output.accept(CastingFluids.MOLTEN_COPPER_ALLOY.getBucket());
                output.accept(CastingFluids.MOLTEN_PULSATING_ALLOY.getBucket());
                output.accept(CastingFluids.MOLTEN_VIBRANT_ALLOY.getBucket());
                output.accept(CastingFluids.MOLTEN_ENERGETIC_ALLOY.getBucket());
                output.accept(CastingFluids.MOLTEN_END_STEEL.getBucket());
                output.accept(CastingFluids.MOLTEN_REDSTONE_ALLOY.getBucket());

                output.accept(ModBlocks.SOLIDIFIER.asItem());
                output.accept(ModBlocks.CONTROLLER.asItem());
                output.accept(ModBlocks.MIXER.asItem());
                output.accept(ModBlocks.MIXER_WHISK.asItem());
                output.accept(ModBlocks.TANK.asItem());
                output.accept(ModBlocks.TOOL_MODIFIER.asItem());

            }).build());

    public static void register(IEventBus eventBus) {
        CREATIVE_MODE_TABS.register(eventBus);
    }


}