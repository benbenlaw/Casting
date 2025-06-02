package com.benbenlaw.casting.block.entity;

import com.benbenlaw.casting.Casting;
import com.benbenlaw.casting.block.CastingBlocks;
import com.benbenlaw.casting.block.entity.multiblock.*;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.world.level.block.entity.BlockEntity;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.neoforged.neoforge.capabilities.Capabilities;
import net.neoforged.neoforge.capabilities.RegisterCapabilitiesEvent;
import net.neoforged.neoforge.registries.DeferredHolder;
import net.neoforged.neoforge.registries.DeferredRegister;

import javax.annotation.Nonnull;
import java.util.function.Supplier;

public class CastingBlockEntities {

    public static final DeferredRegister<BlockEntityType<?>> BLOCK_ENTITIES =
            DeferredRegister.create(BuiltInRegistries.BLOCK_ENTITY_TYPE, Casting.MOD_ID);


    public static final DeferredHolder<BlockEntityType<?>, BlockEntityType<MultiblockControllerBlockEntity>> MULTIBLOCK_CONTROLLER_BLOCK_ENTITY =
            register("multiblock_controller_block_entity", () ->
                    BlockEntityType.Builder.of(MultiblockControllerBlockEntity::new, CastingBlocks.MULTIBLOCK_CONTROLLER.get()));
    public static final DeferredHolder<BlockEntityType<?>, BlockEntityType<MultiblockFuelTankBlockEntity>> MULTIBLOCK_FUEL_TANK_BLOCK_ENTITY =
            register("multiblock_fuel_tank_block_entity", () ->
                    BlockEntityType.Builder.of(MultiblockFuelTankBlockEntity::new, CastingBlocks.MULTIBLOCK_FUEL_TANK.get()));
    public static final DeferredHolder<BlockEntityType<?>, BlockEntityType<MultiblockCoolantTankBlockEntity>> MULTIBLOCK_COOLANT_TANK_BLOCK_ENTITY =
            register("multiblock_coolant_tank_block_entity", () ->
                    BlockEntityType.Builder.of(MultiblockCoolantTankBlockEntity::new, CastingBlocks.MULTIBLOCK_COOLANT_TANK.get()));
    public static final DeferredHolder<BlockEntityType<?>, BlockEntityType<MultiblockSolidifierBlockEntity>> MULTIBLOCK_SOLIDIFIER_BLOCK_ENTITY =
            register("multiblock_solidifier_block_entity", () ->
                    BlockEntityType.Builder.of(MultiblockSolidifierBlockEntity::new, CastingBlocks.MULTIBLOCK_SOLIDIFIER.get()));
    public static final DeferredHolder<BlockEntityType<?>, BlockEntityType<MultiblockValveBlockEntity>> MULTIBLOCK_VALVE_BLOCK_ENTITY =
            register("multiblock_valve_block_entity", () ->
                    BlockEntityType.Builder.of(MultiblockValveBlockEntity::new, CastingBlocks.MULTIBLOCK_VALVE.get()));
    public static final DeferredHolder<BlockEntityType<?>, BlockEntityType<MultiblockMixerBlockEntity>> MULTIBLOCK_MIXER_BLOCK_ENTITY =
            register("multiblock_mixer_block_entity", () ->
                    BlockEntityType.Builder.of(MultiblockMixerBlockEntity::new, CastingBlocks.MULTIBLOCK_MIXER.get()));

    //OG Casting
    public static final DeferredHolder<BlockEntityType<?>, BlockEntityType<ControllerBlockEntity>> CONTROLLER_BLOCK_ENTITY =
            register("controller_block_entity", () ->
                    BlockEntityType.Builder.of(ControllerBlockEntity::new, CastingBlocks.CONTROLLER.get()));
    public static final DeferredHolder<BlockEntityType<?>, BlockEntityType<SolidifierBlockEntity>> SOLIDIFIER_BLOCK_ENTITY =
            register("solidifier_block_entity", () ->
                    BlockEntityType.Builder.of(SolidifierBlockEntity::new, CastingBlocks.SOLIDIFIER.get()));
    public static final DeferredHolder<BlockEntityType<?>, BlockEntityType<TankBlockEntity>> TANK_BLOCK_ENTITY =
            register("tank_block_entity", () ->
                    BlockEntityType.Builder.of(TankBlockEntity::new, CastingBlocks.TANK.get()));
    public static final DeferredHolder<BlockEntityType<?>, BlockEntityType<MixerBlockEntity>> MIXER_BLOCK_ENTITY =
            register("mixer_block_entity", () ->
                    BlockEntityType.Builder.of(MixerBlockEntity::new, CastingBlocks.MIXER.get()));

    public static final DeferredHolder<BlockEntityType<?>, BlockEntityType<EquipmentModifierBlockEntity>> EQUIPMENT_MODIFIER_BLOCK_ENTITY =
            register("tool_modifier_block_entity", () ->
                    BlockEntityType.Builder.of(EquipmentModifierBlockEntity::new, CastingBlocks.EQUIPMENT_MODIFIER.get()));


    //Capability Registration (Item Handler)
    public static void registerCapabilities(RegisterCapabilitiesEvent event) {

        //Controller
        event.registerBlockEntity(Capabilities.ItemHandler.BLOCK,
                CastingBlockEntities.MULTIBLOCK_CONTROLLER_BLOCK_ENTITY.get(), MultiblockControllerBlockEntity::getItemHandlerCapability);
        event.registerBlockEntity(Capabilities.FluidHandler.BLOCK,
                CastingBlockEntities.MULTIBLOCK_CONTROLLER_BLOCK_ENTITY.get(), MultiblockControllerBlockEntity::getFluidHandlerCapability);

        //Fuel Tank
        event.registerBlockEntity(Capabilities.ItemHandler.BLOCK,
                CastingBlockEntities.MULTIBLOCK_FUEL_TANK_BLOCK_ENTITY.get(), MultiblockFuelTankBlockEntity::getItemHandlerCapability);
        event.registerBlockEntity(Capabilities.FluidHandler.BLOCK,
                CastingBlockEntities.MULTIBLOCK_FUEL_TANK_BLOCK_ENTITY.get(), MultiblockFuelTankBlockEntity::getFluidHandlerCapability);

        //Coolant Tank
        event.registerBlockEntity(Capabilities.ItemHandler.BLOCK,
                CastingBlockEntities.MULTIBLOCK_COOLANT_TANK_BLOCK_ENTITY.get(), MultiblockCoolantTankBlockEntity::getItemHandlerCapability);
        event.registerBlockEntity(Capabilities.FluidHandler.BLOCK,
                CastingBlockEntities.MULTIBLOCK_COOLANT_TANK_BLOCK_ENTITY.get(), MultiblockCoolantTankBlockEntity::getFluidHandlerCapability);

        //Solidifier
        event.registerBlockEntity(Capabilities.ItemHandler.BLOCK,
                CastingBlockEntities.MULTIBLOCK_SOLIDIFIER_BLOCK_ENTITY.get(), MultiblockSolidifierBlockEntity::getItemHandlerCapability);
        //Solidifier Fluid handler disabled but can actually extract and insert fluid into the shared tank
        //event.registerBlockEntity(Capabilities.FluidHandler.BLOCK,
        //        CastingBlockEntities.MULTIBLOCK_SOLIDIFIER_BLOCK_ENTITY.get(), MultiblockSolidifierBlockEntity::getFluidHandlerCapability);

        //Valve
        event.registerBlockEntity(Capabilities.FluidHandler.BLOCK,
                CastingBlockEntities.MULTIBLOCK_VALVE_BLOCK_ENTITY.get(), MultiblockValveBlockEntity::getFluidHandlerCapability);
        event.registerBlockEntity(Capabilities.ItemHandler.BLOCK,
                CastingBlockEntities.MULTIBLOCK_VALVE_BLOCK_ENTITY.get(), MultiblockValveBlockEntity::getItemHandlerCapability);


        //OG Casting

        event.registerBlockEntity(Capabilities.ItemHandler.BLOCK,
                CastingBlockEntities.CONTROLLER_BLOCK_ENTITY.get(), ControllerBlockEntity::getItemHandlerCapability);

        event.registerBlockEntity(Capabilities.FluidHandler.BLOCK,
                CastingBlockEntities.CONTROLLER_BLOCK_ENTITY.get(), ControllerBlockEntity::getFluidHandlerCapability);

        event.registerBlockEntity(Capabilities.ItemHandler.BLOCK,
                CastingBlockEntities.SOLIDIFIER_BLOCK_ENTITY.get(), SolidifierBlockEntity::getItemHandlerCapability);

        event.registerBlockEntity(Capabilities.FluidHandler.BLOCK,
                CastingBlockEntities.SOLIDIFIER_BLOCK_ENTITY.get(), SolidifierBlockEntity::getFluidHandlerCapability);

        event.registerBlockEntity(Capabilities.FluidHandler.BLOCK,
                CastingBlockEntities.MIXER_BLOCK_ENTITY.get(), MixerBlockEntity::getFluidHandlerCapability);

        event.registerBlockEntity(Capabilities.FluidHandler.BLOCK,
                CastingBlockEntities.TANK_BLOCK_ENTITY.get(), TankBlockEntity::getFluidHandlerCapability);

        event.registerBlockEntity(Capabilities.ItemHandler.BLOCK,
                CastingBlockEntities.EQUIPMENT_MODIFIER_BLOCK_ENTITY.get(), EquipmentModifierBlockEntity::getItemHandlerCapability);

        event.registerBlockEntity(Capabilities.FluidHandler.BLOCK,
                CastingBlockEntities.EQUIPMENT_MODIFIER_BLOCK_ENTITY.get(), EquipmentModifierBlockEntity::getFluidHandlerCapability);


    }

    public static <T extends BlockEntity> DeferredHolder<BlockEntityType<?>, BlockEntityType<T>> register(@Nonnull String name, @Nonnull Supplier<BlockEntityType.Builder<T>> initializer) {
        return BLOCK_ENTITIES.register(name, () -> initializer.get().build(null));
    }


}
