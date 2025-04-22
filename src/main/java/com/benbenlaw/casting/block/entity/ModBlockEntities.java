package com.benbenlaw.casting.block.entity;


import com.benbenlaw.casting.Casting;
import com.benbenlaw.casting.block.ModBlocks;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.world.level.block.entity.BlockEntity;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.neoforged.bus.api.IEventBus;
import net.neoforged.neoforge.capabilities.Capabilities;
import net.neoforged.neoforge.capabilities.RegisterCapabilitiesEvent;
import net.neoforged.neoforge.registries.DeferredHolder;
import net.neoforged.neoforge.registries.DeferredRegister;

import javax.annotation.Nonnull;
import java.util.function.Supplier;

public class ModBlockEntities {
    public static final DeferredRegister<BlockEntityType<?>> BLOCK_ENTITIES =
            DeferredRegister.create(BuiltInRegistries.BLOCK_ENTITY_TYPE, Casting.MOD_ID);


    public static final DeferredHolder<BlockEntityType<?>, BlockEntityType<ControllerBlockEntity>> CONTROLLER_BLOCK_ENTITY =
            register("controller_block_entity", () ->
                    BlockEntityType.Builder.of(ControllerBlockEntity::new, ModBlocks.CONTROLLER.get()));
    public static final DeferredHolder<BlockEntityType<?>, BlockEntityType<SolidifierBlockEntity>> SOLIDIFIER_BLOCK_ENTITY =
            register("solidifier_block_entity", () ->
                    BlockEntityType.Builder.of(SolidifierBlockEntity::new, ModBlocks.SOLIDIFIER.get()));
    public static final DeferredHolder<BlockEntityType<?>, BlockEntityType<TankBlockEntity>> TANK_BLOCK_ENTITY =
            register("tank_block_entity", () ->
                    BlockEntityType.Builder.of(TankBlockEntity::new, ModBlocks.TANK.get()));
    public static final DeferredHolder<BlockEntityType<?>, BlockEntityType<MixerBlockEntity>> MIXER_BLOCK_ENTITY =
            register("mixer_block_entity", () ->
                    BlockEntityType.Builder.of(MixerBlockEntity::new, ModBlocks.MIXER.get()));

    public static final DeferredHolder<BlockEntityType<?>, BlockEntityType<EquipmentModifierBlockEntity>> EQUIPMENT_MODIFIER_BLOCK_ENTITY =
            register("tool_modifier_block_entity", () ->
                    BlockEntityType.Builder.of(EquipmentModifierBlockEntity::new, ModBlocks.EQUIPMENT_MODIFIER.get()));





    //Capability Registration (Item Handler)
    public static void registerCapabilities(RegisterCapabilitiesEvent event) {
        event.registerBlockEntity(Capabilities.ItemHandler.BLOCK,
                ModBlockEntities.CONTROLLER_BLOCK_ENTITY.get(), ControllerBlockEntity::getItemHandlerCapability);

        event.registerBlockEntity(Capabilities.FluidHandler.BLOCK,
                ModBlockEntities.CONTROLLER_BLOCK_ENTITY.get(), ControllerBlockEntity::getFluidHandlerCapability);

        event.registerBlockEntity(Capabilities.ItemHandler.BLOCK,
                ModBlockEntities.SOLIDIFIER_BLOCK_ENTITY.get(), SolidifierBlockEntity::getItemHandlerCapability);

        event.registerBlockEntity(Capabilities.FluidHandler.BLOCK,
                ModBlockEntities.SOLIDIFIER_BLOCK_ENTITY.get(), SolidifierBlockEntity::getFluidHandlerCapability);

        event.registerBlockEntity(Capabilities.FluidHandler.BLOCK,
                ModBlockEntities.MIXER_BLOCK_ENTITY.get(), MixerBlockEntity::getFluidHandlerCapability);

        event.registerBlockEntity(Capabilities.FluidHandler.BLOCK,
                ModBlockEntities.TANK_BLOCK_ENTITY.get(), TankBlockEntity::getFluidHandlerCapability);
    }






    public static <T extends BlockEntity> DeferredHolder<BlockEntityType<?>, BlockEntityType<T>> register(@Nonnull String name, @Nonnull Supplier<BlockEntityType.Builder<T>> initializer) {
        return BLOCK_ENTITIES.register(name, () -> initializer.get().build(null));
    }

    public static void register(IEventBus eventBus) {
        BLOCK_ENTITIES.register(eventBus);
    }


}
