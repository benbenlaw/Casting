package com.benbenlaw.casting.network.packet;

import com.benbenlaw.casting.block.entity.ControllerBlockEntity;
import com.benbenlaw.casting.block.entity.EquipmentModifierBlockEntity;
import com.benbenlaw.casting.block.entity.MixerBlockEntity;
import com.benbenlaw.casting.block.entity.SolidifierBlockEntity;
import com.benbenlaw.casting.network.payload.ClearTankPayload;
import net.minecraft.core.BlockPos;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.entity.BlockEntity;
import net.minecraft.world.level.block.state.BlockState;
import net.neoforged.neoforge.network.handling.IPayloadContext;

@Deprecated(since = "2.0.0")
public record ClearTankPacket() {

    public static final ClearTankPacket INSTANCE = new ClearTankPacket();

    public static ClearTankPacket get() {
        return INSTANCE;
    }

    public void handle(final ClearTankPayload payload, IPayloadContext context) {

        Player player = context.player();
        Level level = player.level();
        boolean isShiftDown = payload.isShiftDown();
        BlockPos blockPos = payload.blockPos();
        BlockState blockState = level.getBlockState(blockPos);
        BlockEntity blockEntity = level.getBlockEntity(blockPos);
        int tankNumber = payload.tankID();

        if (blockEntity instanceof SolidifierBlockEntity solidifierBlockEntity) {
            if (isShiftDown) {
                solidifierBlockEntity.TANK.getFluid().setAmount(0);
                solidifierBlockEntity.sync();
            }
        }

        if (blockEntity instanceof EquipmentModifierBlockEntity equipmentModifierBlockEntity) {
            if (isShiftDown) {
                equipmentModifierBlockEntity.TANK.getFluid().setAmount(0);
                equipmentModifierBlockEntity.sync();
            }
        }

        if (blockEntity instanceof ControllerBlockEntity controllerBlockEntity) {
            if (isShiftDown) {
                if (tankNumber == 1) {
                    controllerBlockEntity.TANK_1.getFluid().setAmount(0);
                    controllerBlockEntity.sync();
                }
                if (tankNumber == 2) {
                    controllerBlockEntity.TANK_2.getFluid().setAmount(0);
                    controllerBlockEntity.sync();
                }
                if (tankNumber == 3) {
                    controllerBlockEntity.TANK_3.getFluid().setAmount(0);
                    controllerBlockEntity.sync();
                }
                if (tankNumber == 4) {
                    controllerBlockEntity.TANK_4.getFluid().setAmount(0);
                    controllerBlockEntity.sync();
                }
            }
        }

        if (blockEntity instanceof MixerBlockEntity mixerBlockEntity) {
            if (isShiftDown) {
                if (tankNumber == 1) {
                    mixerBlockEntity.TANK_1.getFluid().setAmount(0);
                    mixerBlockEntity.sync();
                }
                if (tankNumber == 2) {
                    mixerBlockEntity.TANK_2.getFluid().setAmount(0);
                    mixerBlockEntity.sync();
                }
                if (tankNumber == 3) {
                    mixerBlockEntity.TANK_3.getFluid().setAmount(0);
                    mixerBlockEntity.sync();
                }
                if (tankNumber == 4) {
                    mixerBlockEntity.TANK_4.getFluid().setAmount(0);
                    mixerBlockEntity.sync();
                }
                if (tankNumber == 5) {
                    mixerBlockEntity.TANK_5.getFluid().setAmount(0);
                    mixerBlockEntity.sync();
                }
                if (tankNumber == 6) {
                    mixerBlockEntity.TANK_6.getFluid().setAmount(0);
                    mixerBlockEntity.sync();
                }
                if (tankNumber == 7) {
                    mixerBlockEntity.OUTPUT_TANK.getFluid().setAmount(0);
                    mixerBlockEntity.sync();
                }
            }
        }


        //ADD CODE HERE
    }
}
