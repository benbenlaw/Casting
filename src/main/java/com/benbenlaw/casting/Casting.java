package com.benbenlaw.casting;

import com.benbenlaw.casting.block.CastingBlocks;
import com.benbenlaw.casting.block.entity.CastingBlockEntities;
import com.benbenlaw.casting.block.entity.client.MultiblockCoolantTankBlockEntityRenderer;
import com.benbenlaw.casting.block.entity.client.MultiblockFuelTankBlockEntityRenderer;
import com.benbenlaw.casting.block.entity.client.TankBlockEntityRenderer;
import com.benbenlaw.casting.config.BeheadingConfig;
import com.benbenlaw.casting.config.CastingConfig;
import com.benbenlaw.casting.config.EquipmentModifierConfig;
import com.benbenlaw.casting.fluid.CastingFluids;
import com.benbenlaw.casting.item.CastingCreativeModeTab;
import com.benbenlaw.casting.item.CastingDataComponents;
import com.benbenlaw.casting.item.CastingItems;
import com.benbenlaw.casting.item.EquipmentModifierItems;
import com.benbenlaw.casting.network.CastingMessages;
import com.benbenlaw.casting.recipe.CastingRecipes;
import com.benbenlaw.casting.screen.*;
import com.benbenlaw.casting.screen.multiblock.*;
import com.benbenlaw.casting.util.CastingColorHandler;
import net.neoforged.api.distmarker.Dist;
import net.neoforged.bus.api.IEventBus;
import net.neoforged.bus.api.SubscribeEvent;
import net.neoforged.fml.ModContainer;
import net.neoforged.fml.common.EventBusSubscriber;
import net.neoforged.fml.common.Mod;
import net.neoforged.fml.config.ModConfig;
import net.neoforged.fml.event.lifecycle.FMLClientSetupEvent;
import net.neoforged.fml.event.lifecycle.FMLCommonSetupEvent;
import net.neoforged.fml.loading.FMLEnvironment;
import net.neoforged.neoforge.capabilities.RegisterCapabilitiesEvent;
import net.neoforged.neoforge.client.event.EntityRenderersEvent;
import net.neoforged.neoforge.client.event.RegisterMenuScreensEvent;
import net.neoforged.neoforge.client.extensions.common.RegisterClientExtensionsEvent;
import net.neoforged.neoforge.network.event.RegisterPayloadHandlersEvent;

@Mod(Casting.MOD_ID)
public class Casting {
    public static final String MOD_ID = "casting";

    public Casting(IEventBus modEventBus, final ModContainer modContainer) {

        CastingItems.ITEMS.register(modEventBus);
        CastingCreativeModeTab.CREATIVE_MODE_TABS.register(modEventBus);
        EquipmentModifierItems.ITEMS.register(modEventBus);
        CastingDataComponents.COMPONENTS.register(modEventBus);

        CastingBlocks.BLOCKS.register(modEventBus);
        CastingBlockEntities.BLOCK_ENTITIES.register(modEventBus);

        CastingFluids.FLUIDS.register(modEventBus);

        modEventBus.addListener(this::registerCapabilities);

        ////    ModParticles.register(modEventBus);
        CastingMenuTypes.MENUS.register(modEventBus);
        CastingRecipes.TYPES.register(modEventBus);
        CastingRecipes.SERIALIZER.register(modEventBus);

        modContainer.registerConfig(ModConfig.Type.STARTUP, EquipmentModifierConfig.SPEC, "bbl/casting/tool_modifiers.toml");
        modContainer.registerConfig(ModConfig.Type.COMMON, BeheadingConfig.SPEC, "bbl/casting/beheading.toml");
        modContainer.registerConfig(ModConfig.Type.COMMON, CastingConfig.SPEC, "bbl/casting/common.toml");

        if (FMLEnvironment.dist == Dist.CLIENT) {
            modEventBus.register(new CastingColorHandler());

        }

        modEventBus.addListener(this::commonSetup);
        modEventBus.addListener(this::beheadingSetup);


        //  ModLoadingContext.get().getActiveContainer().registerConfig(ModConfig.Type.COMMON, ConfigFile.SPEC, "smelting.toml");

    }

    public void registerCapabilities(RegisterCapabilitiesEvent event) {
        CastingBlockEntities.registerCapabilities(event);
    }

    //enqueueWork is used to delay the registration of the networking until after the common setup
    @SubscribeEvent
    public void beheadingSetup(final FMLCommonSetupEvent event) {
        event.enqueueWork(BeheadingConfig::applyToHeadMap);
    }

    public void commonSetup(RegisterPayloadHandlersEvent event) {
        CastingMessages.registerNetworking(event);

    }

    @EventBusSubscriber(modid = Casting.MOD_ID, bus = EventBusSubscriber.Bus.MOD, value = Dist.CLIENT)
    public static class ClientModEvents {

        @SubscribeEvent
        public static void registerRenderers(final EntityRenderersEvent.RegisterRenderers event) {

            //event.registerBlockEntityRenderer(CastingBlockEntities.CONTROLLER_BLOCK_ENTITY.get(), ControllerBlockEntityRenderer::new);
            event.registerBlockEntityRenderer(CastingBlockEntities.MULTIBLOCK_FUEL_TANK_BLOCK_ENTITY.get(), MultiblockFuelTankBlockEntityRenderer::new);
            event.registerBlockEntityRenderer(CastingBlockEntities.MULTIBLOCK_COOLANT_TANK_BLOCK_ENTITY.get(), MultiblockCoolantTankBlockEntityRenderer::new);
            event.registerBlockEntityRenderer(CastingBlockEntities.TANK_BLOCK_ENTITY.get(), TankBlockEntityRenderer::new);
        }

        @SubscribeEvent
        public static void registerScreens(RegisterMenuScreensEvent event) {
            event.register(CastingMenuTypes.MULTIBLOCK_CONTROLLER_MENU.get(), MultiblockControllerScreen::new);
            event.register(CastingMenuTypes.MULTIBLOCK_FUEL_TANK_MENU.get(), MultiblockFuelTankScreen::new);
            event.register(CastingMenuTypes.MULTIBLOCK_COOLANT_TANK_MENU.get(), MultiblockCoolantTankScreen::new);
            event.register(CastingMenuTypes.MULTIBLOCK_SOLIDIFIER_MENU.get(), MultiblockSolidifierScreen::new);
            event.register(CastingMenuTypes.MULTIBLOCK_VALVE_MENU.get(), MultiblockValveScreen::new);
            event.register(CastingMenuTypes.MULTIBLOCK_MIXER_MENU.get(), MultiblockMixerScreen::new);

            //OG Casting
            event.register(CastingMenuTypes.SMELTER_MENU.get(), SmelterScreen::new);
            event.register(CastingMenuTypes.SOLIDIFIER_MENU.get(), SolidifierScreen::new);
            event.register(CastingMenuTypes.MIXER_MENU.get(), MixerScreen::new);
            event.register(CastingMenuTypes.EQUIPMENT_MODIFIER_MENU.get(), EquipmentModifierScreen::new);

            //event.register(ModMenuTypes.MIXER_MENU.get(), MixerScreen::new);
            //event.register(ModMenuTypes.EQUIPMENT_MODIFIER_MENU.get(), EquipmentModifierScreen::new);

        }

        @SubscribeEvent
        public static void onClientExtensions(RegisterClientExtensionsEvent event) {
            CastingFluids.FLUIDS_MAP.values().forEach(fluid -> {
                var fluidType = fluid.getFluidType();
                var extensions = fluidType.getClientExtensions();
                event.registerFluidType(extensions, fluidType);
            });
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
