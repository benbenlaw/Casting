package com.benbenlaw.casting.network.packet;

import com.benbenlaw.casting.block.entity.ControllerBlockEntity;
import com.benbenlaw.casting.block.entity.EquipmentModifierBlockEntity;
import com.benbenlaw.casting.block.entity.MixerBlockEntity;
import com.benbenlaw.casting.block.entity.SolidifierBlockEntity;
import com.benbenlaw.casting.item.CastingDataComponents;
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

            // If a valid tank is selected, perform the fluid transfer
            transferFluidBetweenTankAndItem(selectedTank, carriedItem, player, isOutputTank);
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
        }



    }
    private void transferFluidBetweenTankAndItem(FluidTank tank, ItemStack carriedItem, Player player, boolean isOutputTank) {
        int itemMaxCapacity = 8000;  // Max fluid capacity for the item

        // Check if the carried item has fluid (attempt to empty the item into the tank first)
        if (carriedItem.get(CastingDataComponents.FLUID_TYPE) != null) {
            String fluidAsString = carriedItem.get(CastingDataComponents.FLUID_TYPE);
            assert fluidAsString != null;

            Fluid carriedFluid = BuiltInRegistries.FLUID.get(ResourceLocation.tryParse(fluidAsString));
            int carriedAmount = carriedItem.get(CastingDataComponents.FLUID_AMOUNT);

            // Ensure we're not trying to add fluid to the output tank
            if (!isOutputTank) {
                // If the tank is empty or contains the same fluid as the carried item
                if (tank.isEmpty() || tank.getFluid().getFluid().equals(carriedFluid)) {
                    // Calculate the available space in the tank
                    int availableTankSpace = tank.getCapacity() - tank.getFluidAmount();
                    int amountToTransfer = Math.min(carriedAmount, availableTankSpace); // Amount we can transfer to the tank

                    if (amountToTransfer > 0) {
                        // Transfer the fluid from the item to the tank
                        tank.fill(new FluidStack(carriedFluid, amountToTransfer), IFluidHandler.FluidAction.EXECUTE);

                        // Reduce the fluid amount in the carried item
                        carriedItem.set(CastingDataComponents.FLUID_AMOUNT, carriedAmount - amountToTransfer);

                        // If the item is empty after the transfer, clear the fluid data
                        if (carriedItem.get(CastingDataComponents.FLUID_AMOUNT) <= 0) {
                            carriedItem.remove(CastingDataComponents.FLUID_TYPE);
                            carriedItem.remove(CastingDataComponents.FLUID_AMOUNT);
                        }
                        return; // End the method if we successfully transferred fluid to the tank
                    }
                } else {
                    player.sendSystemMessage(Component.literal("Fluid types don't match!"));
                }
            } else {
                player.sendSystemMessage(Component.literal("Cannot add fluids to the output tank!"));
            }
        }

        // If the carried item is empty (no fluid) or we finished emptying it, try to fill it from the tank
        if (!tank.isEmpty()) {
            FluidStack currentFluidInTank = tank.getFluid();

            if (carriedItem.get(CastingDataComponents.FLUID_TYPE) == null) {
                // Transfer fluid from the tank to the item if the item has no fluid
                int transferAmount = Math.min(currentFluidInTank.getAmount(), itemMaxCapacity); // Limit by item max capacity
                carriedItem.set(CastingDataComponents.FLUID_TYPE, currentFluidInTank.getFluid().getFluidType().toString());
                carriedItem.set(CastingDataComponents.FLUID_AMOUNT, transferAmount);

                // Reduce or empty the tank after transfer
                tank.setFluid(new FluidStack(currentFluidInTank.getFluid(), currentFluidInTank.getAmount() - transferAmount));

            } else if (carriedItem.get(CastingDataComponents.FLUID_TYPE) != null) {
                String fluidAsString = carriedItem.get(CastingDataComponents.FLUID_TYPE);
                assert fluidAsString != null;

                Fluid carriedFluid = BuiltInRegistries.FLUID.get(ResourceLocation.tryParse(fluidAsString));
                int carriedAmount = carriedItem.get(CastingDataComponents.FLUID_AMOUNT);

                // Check if the carried fluid matches the fluid in the tank
                if (carriedFluid.equals(currentFluidInTank.getFluid())) {
                    int remainingCapacity = itemMaxCapacity - carriedAmount; // Remaining capacity in the item
                    int currentTankAmount = currentFluidInTank.getAmount();

                    // Determine how much fluid can be transferred
                    int amountToTransfer = Math.min(currentTankAmount, remainingCapacity);

                    if (amountToTransfer > 0) {
                        carriedItem.set(CastingDataComponents.FLUID_AMOUNT, carriedAmount + amountToTransfer);

                        // Reduce the fluid in the tank
                        tank.setFluid(new FluidStack(currentFluidInTank.getFluid(), currentTankAmount - amountToTransfer));
                    }

                } else {
                    player.sendSystemMessage(Component.literal("Fluid types don't match!"));
                }
            }
        }
    }





}
