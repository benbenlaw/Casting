package com.benbenlaw.casting.fluid;

import com.benbenlaw.casting.Casting;
import com.benbenlaw.core.Core;
import com.benbenlaw.core.fluid.FluidDeferredRegister;
import com.benbenlaw.core.fluid.FluidRegistryObject;
import net.minecraft.world.item.BucketItem;
import net.minecraft.world.level.block.LiquidBlock;
import net.neoforged.neoforge.fluids.BaseFlowingFluid;

import java.util.HashMap;
import java.util.Map;

public class CastingFluids {

    public static final FluidDeferredRegister FLUIDS = new FluidDeferredRegister(Casting.MOD_ID);

    public static final Map<String, FluidRegistryObject<FluidDeferredRegister.CoreFluidTypes,
            BaseFlowingFluid.Source, BaseFlowingFluid.Flowing, LiquidBlock, BucketItem>> FLUIDS_MAP = new HashMap<>();

    static {
        for (FluidData data : FluidData.FLUID_DEFINITIONS) {

            if (data.name().contains("molten")) {
                var fluid = FLUIDS.register(data.name(), (renderProperties) ->
                        renderProperties.texture(
                                        Core.identifier(data.stillTexture()),
                                        Core.identifier(data.flowTexture())
                                ).tint(data.tint())
                                .temperature(data.fluidProduceType().temp()).moveLikeLava()
                );
                FLUIDS_MAP.put(data.name(), fluid);
            } 

            else {
                var fluid = FLUIDS.register(data.name(), (renderProperties) ->
                        renderProperties.texture(
                                        Core.identifier(data.stillTexture()),
                                        Core.identifier(data.flowTexture())
                                ).tint(data.tint())
                                .temperature(data.fluidProduceType().temp()).moveLikeWater()
                );
                FLUIDS_MAP.put(data.name(), fluid);

            }




        }
    }

}
