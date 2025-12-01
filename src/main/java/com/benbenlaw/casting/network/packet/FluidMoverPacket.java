package com.benbenlaw.casting.network.packet;

import com.benbenlaw.casting.block.entity.ControllerBlockEntity;
import com.benbenlaw.casting.block.entity.EquipmentModifierBlockEntity;
import com.benbenlaw.casting.block.entity.MixerBlockEntity;
import com.benbenlaw.casting.block.entity.SolidifierBlockEntity;
import com.benbenlaw.casting.item.CastingDataComponents;
import com.benbenlaw.casting.item.util.FluidListComponent;
import com.benbenlaw.casting.network.payload.FluidMoverPayload;
import net.minecraft.core.BlockPos;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.entity.BlockEntity;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.material.Fluid;
import net.neoforged.neoforge.fluids.FluidStack;
import net.neoforged.neoforge.fluids.capability.IFluidHandler;
import net.neoforged.neoforge.fluids.capability.templates.FluidTank;
import net.neoforged.neoforge.network.handling.IPayloadContext;

import java.util.List;

@Deprecated(since = "2.0.0")
public record FluidMoverPacket() {

    public static final FluidMoverPacket INSTANCE = new FluidMoverPacket();

    public static FluidMoverPacket get() {
        return INSTANCE;
    }


    public void handle(final FluidMoverPayload payload, IPayloadContext context) {

        Player player = context.player();
        Level level = player.level();
        BlockPos blockPos = payload.blockPos();
        BlockState blockState = level.getBlockState(blockPos);
        BlockEntity blockEntity = level.getBlockEntity(blockPos);
        int tankNumber = payload.tankID();
        ItemStack carriedItem = player.containerMenu.getCarried();
        int maxItemFluidCap = 8000;

        if (blockEntity instanceof MixerBlockEntity mixerBlockEntity) {

            // Select the correct tank based on the tankNumber (from the packet)
            FluidTank selectedTank = null;
            boolean isOutputTank = false;
            switch (tankNumber) {
                case 1 -> selectedTank = mixerBlockEntity.TANK_1;
                case 2 -> selectedTank = mixerBlockEntity.TANK_2;
                case 3 -> selectedTank = mixerBlockEntity.TANK_3;
                case 4 -> selectedTank = mixerBlockEntity.TANK_4;
                case 5 -> selectedTank = mixerBlockEntity.TANK_5;
                case 6 -> selectedTank = mixerBlockEntity.TANK_6;
                case 7 -> {
                    selectedTank = mixerBlockEntity.OUTPUT_TANK;
                    isOutputTank = true; // Mark this as the output tank
                }
                default -> {
                    player.sendSystemMessage(Component.literal("Invalid tank number!"));
                    return;
                }
            }

            transferFluidBetweenTankAndItem(selectedTank, carriedItem, player, isOutputTank);
            mixerBlockEntity.sync();

        }

        if (blockEntity instanceof ControllerBlockEntity controllerBlockEntity) {
            FluidTank selectedTank = null;
            boolean isOutputTank = true;
            switch (tankNumber) {

                case 1 -> selectedTank = controllerBlockEntity.TANK_1;
                case 2 -> selectedTank = controllerBlockEntity.TANK_2;
                case 3 -> selectedTank = controllerBlockEntity.TANK_3;
                case 4 -> selectedTank = controllerBlockEntity.TANK_4;

                default -> {
                    player.sendSystemMessage(Component.literal("Invalid tank number!"));
                    return;
                }

            }
            transferFluidBetweenTankAndItem(selectedTank, carriedItem, player, isOutputTank);
            controllerBlockEntity.sync();

        }

        if (blockEntity instanceof SolidifierBlockEntity solidifierBlockEntity) {
            FluidTank selectedTank = null;
            boolean isOutputTank = false;
            if (tankNumber == 1) {
                selectedTank = solidifierBlockEntity.TANK;
            } else {
                player.sendSystemMessage(Component.literal("Invalid tank number!"));
                return;
            }
            solidifierBlockEntity.sync();
            transferFluidBetweenTankAndItem(selectedTank, carriedItem, player, isOutputTank);
        }

        if (blockEntity instanceof EquipmentModifierBlockEntity equipmentModifierBlockEntity) {
            FluidTank selectedTank = null;
            boolean isOutputTank = false;
            if (tankNumber == 1) {
                selectedTank = equipmentModifierBlockEntity.TANK;
            } else {
                player.sendSystemMessage(Component.literal("Invalid tank number!"));
                return;
            }
            transferFluidBetweenTankAndItem(selectedTank, carriedItem, player, isOutputTank);
            equipmentModifierBlockEntity.sync();

        }
    }
    private void transferFluidBetweenTankAndItem(FluidTank tank, ItemStack carriedItem, Player player, boolean isOutputTank) {
        int itemMaxCapacity = 8000;

        if (carriedItem.get(CastingDataComponents.FLUIDS) != null) {
            FluidListComponent component = carriedItem.get(CastingDataComponents.FLUIDS);
            List<FluidStack> fluids = component.fluids();

            Fluid carriedFluid = fluids.get(0).getFluid();
            int carriedAmount = fluids.get(0).getAmount();

            if (!isOutputTank) {
                if (tank.isEmpty() || tank.getFluid().getFluid().equals(carriedFluid)) {
                    int availableTankSpace = tank.getCapacity() - tank.getFluidAmount();
                    int amountToTransfer = Math.min(carriedAmount, availableTankSpace);

                    if (amountToTransfer > 0) {
                        tank.fill(new FluidStack(carriedFluid, amountToTransfer), IFluidHandler.FluidAction.EXECUTE);
                        int remainingAmount = carriedAmount - amountToTransfer;
                        carriedItem.set(CastingDataComponents.FLUIDS, new FluidListComponent(List.of(new FluidStack(carriedFluid, remainingAmount))));

                        if (carriedItem.get(CastingDataComponents.FLUIDS).fluids().get(0).isEmpty()) {
                            carriedItem.remove(CastingDataComponents.FLUIDS);
                        }
                        return;
                    }
                } else {
                    player.sendSystemMessage(Component.literal("Fluid types don't match!"));
                }
            } else {
                player.sendSystemMessage(Component.literal("Cannot add fluids to the output tank!"));
            }
        }

        if (!tank.isEmpty()) {
            FluidStack currentFluidInTank = tank.getFluid();

            if (carriedItem.get(CastingDataComponents.FLUIDS) == null) {
                int transferAmount = Math.min(currentFluidInTank.getAmount(), itemMaxCapacity);
                carriedItem.set(CastingDataComponents.FLUIDS, new FluidListComponent(List.of(new FluidStack(currentFluidInTank.getFluid(), transferAmount))));
                tank.setFluid(new FluidStack(currentFluidInTank.getFluid(), currentFluidInTank.getAmount() - transferAmount));
            } else {
                FluidListComponent component = carriedItem.get(CastingDataComponents.FLUIDS);
                List<FluidStack> fluids = component.fluids();
                Fluid carriedFluid = fluids.get(0).getFluid();
                int carriedAmount = fluids.get(0).getAmount();

                if (carriedFluid.equals(currentFluidInTank.getFluid())) {
                    int remainingCapacity = itemMaxCapacity - carriedAmount;
                    int currentTankAmount = currentFluidInTank.getAmount();
                    int amountToTransfer = Math.min(currentTankAmount, remainingCapacity);

                    if (amountToTransfer > 0) {
                        int newCarriedAmount = carriedAmount + amountToTransfer;
                        carriedItem.set(CastingDataComponents.FLUIDS, new FluidListComponent(List.of(new FluidStack(carriedFluid, newCarriedAmount))));
                        tank.setFluid(new FluidStack(currentFluidInTank.getFluid(), currentTankAmount - amountToTransfer));
                    }
                } else {
                    player.sendSystemMessage(Component.literal("Fluid types don't match!"));
                }
            }
        }
    }



}
