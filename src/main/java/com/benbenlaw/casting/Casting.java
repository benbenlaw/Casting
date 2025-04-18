package com.benbenlaw.casting;

import com.benbenlaw.casting.block.ModBlocks;
import com.benbenlaw.casting.block.entity.ModBlockEntities;
import com.benbenlaw.casting.config.BeheadingConfig;
import com.benbenlaw.casting.config.ToolModifierConfig;
import com.benbenlaw.casting.fluid.CastingFluids;
import com.benbenlaw.casting.item.CastingDataComponents;
import com.benbenlaw.casting.item.ToolModifierItems;
import com.benbenlaw.casting.networking.CastingModMessages;
import com.benbenlaw.casting.screen.*;
import com.benbenlaw.casting.item.ModCreativeModTab;
import com.benbenlaw.casting.item.ModItems;
import com.benbenlaw.casting.recipe.ModRecipes;
import com.mojang.logging.LogUtils;
import net.neoforged.api.distmarker.Dist;
import net.neoforged.bus.api.IEventBus;
import net.neoforged.bus.api.SubscribeEvent;
import net.neoforged.fml.ModContainer;
import net.neoforged.fml.common.EventBusSubscriber;
import net.neoforged.fml.common.Mod;
import net.neoforged.fml.config.ModConfig;
import net.neoforged.fml.event.lifecycle.FMLClientSetupEvent;
import net.neoforged.neoforge.capabilities.RegisterCapabilitiesEvent;
import net.neoforged.neoforge.client.event.RegisterMenuScreensEvent;
import net.neoforged.neoforge.client.extensions.common.RegisterClientExtensionsEvent;
import net.neoforged.neoforge.network.event.RegisterPayloadHandlersEvent;
import org.slf4j.Logger;

@Mod(Casting.MOD_ID)
public class Casting {
    public static final String MOD_ID = "casting";
    private static final Logger LOGGER = LogUtils.getLogger();

    public Casting(IEventBus modEventBus, final ModContainer modContainer) {

        ModItems.register(modEventBus);
        ToolModifierItems.register(modEventBus);
        CastingDataComponents.COMPONENTS.register(modEventBus);

        ModBlocks.register(modEventBus);
        ModCreativeModTab.register(modEventBus);
        ModBlockEntities.register(modEventBus);

        CastingFluids.FLUIDS.register(modEventBus);

        modEventBus.addListener(this::registerCapabilities);

        //    ModParticles.register(modEventBus);
        ModMenuTypes.register(modEventBus);
        ModRecipes.register(modEventBus);

        modContainer.registerConfig(ModConfig.Type.STARTUP, ToolModifierConfig.SPEC, "bbl/casting/tool_modifiers.toml");
        modContainer.registerConfig(ModConfig.Type.COMMON, BeheadingConfig.SPEC, "bbl/casting/beheading.toml");

        modEventBus.addListener(this::commonSetup);
        modEventBus.addListener(this::beheadingSetup);


      //  ModLoadingContext.get().getActiveContainer().registerConfig(ModConfig.Type.COMMON, ConfigFile.SPEC, "smelting.toml");

    }

    public void registerCapabilities(RegisterCapabilitiesEvent event) {
        ModBlockEntities.registerCapabilities(event);
    }

    //enqueueWork is used to delay the registration of the networking until after the common setup
    @SubscribeEvent
    public void beheadingSetup(final FMLClientSetupEvent event) {
        event.enqueueWork(BeheadingConfig::applyToHeadMap);
    }

    public void commonSetup(RegisterPayloadHandlersEvent event) {
        CastingModMessages.registerNetworking(event);

    }

    @EventBusSubscriber(modid = Casting.MOD_ID, bus = EventBusSubscriber.Bus.MOD, value = Dist.CLIENT)
    public static class ClientModEvents {


        @SubscribeEvent
        public static void registerScreens(RegisterMenuScreensEvent event) {
            event.register(ModMenuTypes.SMELTER_MENU.get(), SmelterScreen::new);
            event.register(ModMenuTypes.SOLIDIFIER_MENU.get(), SolidifierScreen::new);
            event.register(ModMenuTypes.MIXER_MENU.get(), MixerScreen::new);
            event.register(ModMenuTypes.TOOL_MODIFIER_MENU.get(), ToolModifierScreen::new);

        }

        @SubscribeEvent
        public static void onClientExtensions(RegisterClientExtensionsEvent event) {
            event.registerFluidType(CastingFluids.MOLTEN_BRONZE.getFluidType().getClientExtensions(),
                    CastingFluids.MOLTEN_BRONZE.getFluidType());
            event.registerFluidType(CastingFluids.MOLTEN_OBSIDIAN.getFluidType().getClientExtensions(),
                    CastingFluids.MOLTEN_OBSIDIAN.getFluidType());
            event.registerFluidType(CastingFluids.MOLTEN_STEEL.getFluidType().getClientExtensions(),
                    CastingFluids.MOLTEN_STEEL.getFluidType());
            event.registerFluidType(CastingFluids.MOLTEN_NETHERITE.getFluidType().getClientExtensions(),
                    CastingFluids.MOLTEN_NETHERITE.getFluidType());
            event.registerFluidType(CastingFluids.MOLTEN_ELECTRUM.getFluidType().getClientExtensions(),
                    CastingFluids.MOLTEN_ELECTRUM.getFluidType());
            event.registerFluidType(CastingFluids.MOLTEN_INVAR.getFluidType().getClientExtensions(),
                    CastingFluids.MOLTEN_INVAR.getFluidType());
            event.registerFluidType(CastingFluids.MOLTEN_IRON.getFluidType().getClientExtensions(),
                    CastingFluids.MOLTEN_IRON.getFluidType());
            event.registerFluidType(CastingFluids.MOLTEN_GOLD.getFluidType().getClientExtensions(),
                    CastingFluids.MOLTEN_GOLD.getFluidType());
            event.registerFluidType(CastingFluids.MOLTEN_COPPER.getFluidType().getClientExtensions(),
                    CastingFluids.MOLTEN_COPPER.getFluidType());
            event.registerFluidType(CastingFluids.MOLTEN_TIN.getFluidType().getClientExtensions(),
                    CastingFluids.MOLTEN_TIN.getFluidType());
            event.registerFluidType(CastingFluids.MOLTEN_LEAD.getFluidType().getClientExtensions(),
                    CastingFluids.MOLTEN_LEAD.getFluidType());
            event.registerFluidType(CastingFluids.MOLTEN_SILVER.getFluidType().getClientExtensions(),
                    CastingFluids.MOLTEN_SILVER.getFluidType());
            event.registerFluidType(CastingFluids.MOLTEN_NICKEL.getFluidType().getClientExtensions(),
                    CastingFluids.MOLTEN_NICKEL.getFluidType());
            event.registerFluidType(CastingFluids.MOLTEN_OSMIUM.getFluidType().getClientExtensions(),
                    CastingFluids.MOLTEN_OSMIUM.getFluidType());
            event.registerFluidType(CastingFluids.MOLTEN_QUARTZ.getFluidType().getClientExtensions(),
                    CastingFluids.MOLTEN_QUARTZ.getFluidType());
            event.registerFluidType(CastingFluids.MOLTEN_LAPIS.getFluidType().getClientExtensions(),
                    CastingFluids.MOLTEN_LAPIS.getFluidType());
            event.registerFluidType(CastingFluids.MOLTEN_REDSTONE.getFluidType().getClientExtensions(),
                    CastingFluids.MOLTEN_REDSTONE.getFluidType());
            event.registerFluidType(CastingFluids.MOLTEN_DIAMOND.getFluidType().getClientExtensions(),
                    CastingFluids.MOLTEN_DIAMOND.getFluidType());
            event.registerFluidType(CastingFluids.MOLTEN_EMERALD.getFluidType().getClientExtensions(),
                    CastingFluids.MOLTEN_EMERALD.getFluidType());
            event.registerFluidType(CastingFluids.MOLTEN_URANIUM.getFluidType().getClientExtensions(),
                    CastingFluids.MOLTEN_URANIUM.getFluidType());
            event.registerFluidType(CastingFluids.MOLTEN_GLASS.getFluidType().getClientExtensions(),
                    CastingFluids.MOLTEN_GLASS.getFluidType());
            event.registerFluidType(CastingFluids.MOLTEN_DEBRIS.getFluidType().getClientExtensions(),
                    CastingFluids.MOLTEN_DEBRIS.getFluidType());
            event.registerFluidType(CastingFluids.MOLTEN_STONE.getFluidType().getClientExtensions(),
                    CastingFluids.MOLTEN_STONE.getFluidType());
            event.registerFluidType(CastingFluids.MOLTEN_COAL.getFluidType().getClientExtensions(),
                    CastingFluids.MOLTEN_COAL.getFluidType());
            event.registerFluidType(CastingFluids.MOLTEN_ALUMINUM.getFluidType().getClientExtensions(),
                    CastingFluids.MOLTEN_ALUMINUM.getFluidType());
            event.registerFluidType(CastingFluids.MOLTEN_ZINC.getFluidType().getClientExtensions(),
                    CastingFluids.MOLTEN_ZINC.getFluidType());
            event.registerFluidType(CastingFluids.MOLTEN_PLATINUM.getFluidType().getClientExtensions(),
                    CastingFluids.MOLTEN_PLATINUM.getFluidType());
            event.registerFluidType(CastingFluids.MOLTEN_IRIDIUM.getFluidType().getClientExtensions(),
                    CastingFluids.MOLTEN_IRIDIUM.getFluidType());
            event.registerFluidType(CastingFluids.MOLTEN_GLOWSTONE.getFluidType().getClientExtensions(),
                    CastingFluids.MOLTEN_GLOWSTONE.getFluidType());
            event.registerFluidType(CastingFluids.MOLTEN_ENDER.getFluidType().getClientExtensions(),
                    CastingFluids.MOLTEN_ENDER.getFluidType());
            event.registerFluidType(CastingFluids.MOLTEN_CONSTANTAN.getFluidType().getClientExtensions(),
                    CastingFluids.MOLTEN_CONSTANTAN.getFluidType());
            event.registerFluidType(CastingFluids.MOLTEN_BRASS.getFluidType().getClientExtensions(),
                    CastingFluids.MOLTEN_BRASS.getFluidType());
            event.registerFluidType(CastingFluids.MOLTEN_SIGNALUM.getFluidType().getClientExtensions(),
                    CastingFluids.MOLTEN_SIGNALUM.getFluidType());
            event.registerFluidType(CastingFluids.MOLTEN_LUMIUM.getFluidType().getClientExtensions(),
                    CastingFluids.MOLTEN_LUMIUM.getFluidType());
            event.registerFluidType(CastingFluids.MOLTEN_ENDERIUM.getFluidType().getClientExtensions(),
                    CastingFluids.MOLTEN_ENDERIUM.getFluidType());

            event.registerFluidType(CastingFluids.MOLTEN_CONDUCTIVE_ALLOY.getFluidType().getClientExtensions(),
                    CastingFluids.MOLTEN_CONDUCTIVE_ALLOY.getFluidType());
            event.registerFluidType(CastingFluids.MOLTEN_SILICON.getFluidType().getClientExtensions(),
                    CastingFluids.MOLTEN_SILICON.getFluidType());
            event.registerFluidType(CastingFluids.MOLTEN_SOUL.getFluidType().getClientExtensions(),
                    CastingFluids.MOLTEN_SOUL.getFluidType());
            event.registerFluidType(CastingFluids.MOLTEN_END_STONE.getFluidType().getClientExtensions(),
                    CastingFluids.MOLTEN_END_STONE.getFluidType());
            event.registerFluidType(CastingFluids.MOLTEN_SOULARIUM.getFluidType().getClientExtensions(),
                    CastingFluids.MOLTEN_SOULARIUM.getFluidType());
            event.registerFluidType(CastingFluids.MOLTEN_DARK_STEEL.getFluidType().getClientExtensions(),
                    CastingFluids.MOLTEN_DARK_STEEL.getFluidType());
            event.registerFluidType(CastingFluids.MOLTEN_COPPER_ALLOY.getFluidType().getClientExtensions(),
                    CastingFluids.MOLTEN_COPPER_ALLOY.getFluidType());
            event.registerFluidType(CastingFluids.MOLTEN_PULSATING_ALLOY.getFluidType().getClientExtensions(),
                    CastingFluids.MOLTEN_PULSATING_ALLOY.getFluidType());
            event.registerFluidType(CastingFluids.MOLTEN_VIBRANT_ALLOY.getFluidType().getClientExtensions(),
                    CastingFluids.MOLTEN_VIBRANT_ALLOY.getFluidType());
            event.registerFluidType(CastingFluids.MOLTEN_ENERGETIC_ALLOY.getFluidType().getClientExtensions(),
                    CastingFluids.MOLTEN_ENERGETIC_ALLOY.getFluidType());
            event.registerFluidType(CastingFluids.MOLTEN_END_STEEL.getFluidType().getClientExtensions(),
                    CastingFluids.MOLTEN_END_STEEL.getFluidType());
            event.registerFluidType(CastingFluids.MOLTEN_REDSTONE_ALLOY.getFluidType().getClientExtensions(),
                    CastingFluids.MOLTEN_REDSTONE_ALLOY.getFluidType());



            
        }

        @SubscribeEvent
        public static void onClientSetup(FMLClientSetupEvent event) {

            event.enqueueWork(() -> {

                // Only Needed if i need a translucent fluid otherwise should be ok? //


           //     ItemBlockRenderTypes.setRenderLayer(ModFluids.MOLTEN_URANIUM_SOURCE.get(), RenderType.translucent());
           //     ItemBlockRenderTypes.setRenderLayer(ModFluids.MOLTEN_URANIUM_FLOWING.get(), RenderType.translucent());


            });
        }
    }
}
