package com.benbenlaw.casting.mixin;

import jagm.classicpipes.blockentity.FluidPipeEntity;
import jagm.classicpipes.services.NeoForgeService;
import jagm.classicpipes.util.FluidInPipe;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.level.block.entity.BlockEntity;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.material.Fluid;
import net.minecraft.world.level.material.Fluids;
import net.neoforged.fml.ModList;
import net.neoforged.neoforge.capabilities.Capabilities;
import net.neoforged.neoforge.transfer.ResourceHandler;
import net.neoforged.neoforge.transfer.fluid.FluidResource;
import net.neoforged.neoforge.transfer.transaction.Transaction;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Overwrite;

import java.util.function.Predicate;

@Mixin(NeoForgeService.class)
public class TestFixNeoForgeServiceMixin {

    /**
     * @author  benbenlaw
     * @reason  temp fix fluid extraction bug with Classic Pipes
     * @implNote  this is no longer loaded by default as now fixed in Classic Pipes
     */
    @Overwrite
    public boolean handleFluidExtraction(FluidPipeEntity pipe, BlockState pipeState, ServerLevel level,
                                         BlockPos containerPos, Direction face, int amount, Predicate<Fluid> predicate) {

        if (!ModList.get().isLoaded("classicpipes")) return false;

        BlockEntity blockEntity = level.getBlockEntity(containerPos);
        if (blockEntity instanceof FluidPipeEntity || pipe.totalAmount() >= 1000) {
            return false;
        }

        BlockState state = level.getBlockState(containerPos);
        ResourceHandler<FluidResource> fluidHandler =
                level.getCapability(Capabilities.Fluid.BLOCK, containerPos, state, blockEntity, face);

        if (fluidHandler == null) {
            return false;
        }

        int amountToDrain = Math.min(amount, pipe.remainingCapacity());

        if (!pipe.isEmpty()) {
            Fluid fluid = pipe.getFluid() != null ? pipe.getFluid() : Fluids.WATER;
            if (!predicate.test(fluid)) {
                return false;
            }

            try (Transaction transaction = Transaction.open(null)) {
                int amountExtracted = fluidHandler.extract(FluidResource.of(fluid), amountToDrain, transaction);
                if (amountExtracted > 0) {
                    pipe.insertFluidPacket(level, new FluidInPipe(amountExtracted, pipe.getTargetSpeed(),
                            (short) 0, face.getOpposite(), face.getOpposite(), (short) 0));
                    transaction.commit();
                    return true;
                }
            }
            return false;
        }

        for (int tank = 0; tank < fluidHandler.size(); tank++) {
            FluidResource tankResource = fluidHandler.getResource(tank);
            if (tankResource.isEmpty() || !predicate.test(tankResource.getFluid())) {
                continue;
            }

            try (Transaction transaction = Transaction.open(null)) {
                int amountExtracted = fluidHandler.extract(tankResource, amountToDrain, transaction);
                if (amountExtracted > 0) {
                    pipe.setFluid(tankResource.getFluid());
                    pipe.insertFluidPacket(level, new FluidInPipe(amountExtracted, pipe.getTargetSpeed(),
                            (short) 0, face.getOpposite(), face.getOpposite(), (short) 0));
                    transaction.commit();
                    return true;
                }
            }
        }

        return false;
    }
}