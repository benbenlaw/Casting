package com.benbenlaw.casting.data;

import com.benbenlaw.casting.Casting;
import com.benbenlaw.casting.fluid.CastingFluids;
import com.benbenlaw.casting.item.EquipmentModifierItems;
import com.benbenlaw.casting.item.ModItems;
import com.benbenlaw.opolisutilities.OpolisUtilities;
import com.benbenlaw.opolisutilities.fluid.FluidDeferredRegister;
import net.minecraft.core.Holder;
import net.minecraft.data.PackOutput;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.item.BucketItem;
import net.minecraft.world.item.Item;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.material.Fluid;
import net.neoforged.neoforge.client.model.generators.ItemModelBuilder;
import net.neoforged.neoforge.client.model.generators.ItemModelProvider;
import net.neoforged.neoforge.client.model.generators.loaders.DynamicFluidContainerModelBuilder;
import net.neoforged.neoforge.common.data.ExistingFileHelper;
import net.neoforged.neoforge.internal.versions.neoforge.NeoForgeVersion;
import net.neoforged.neoforge.registries.DeferredBlock;
import net.neoforged.neoforge.registries.DeferredItem;

public class CastingItemModelProvider extends ItemModelProvider {

    public CastingItemModelProvider(PackOutput output, ExistingFileHelper existingFileHelper) {
        super(output, Casting.MOD_ID, existingFileHelper);
    }

    @Override
    protected void registerModels() {

        //Misc Items
        simpleItem(ModItems.BLOCK_MOLD);
        simpleItem(ModItems.GEAR_MOLD);
        simpleItem(ModItems.INGOT_MOLD);
        simpleItem(ModItems.NUGGET_MOLD);
        simpleItem(ModItems.PLATE_MOLD);
        simpleItem(ModItems.ROD_MOLD);
        simpleItem(ModItems.GEM_MOLD);
        simpleItem(ModItems.DUST_MOLD);
        simpleItem(ModItems.BALL_MOLD);
        simpleItem(ModItems.REPAIRING_MOLD);
        simpleItem(ModItems.BLACK_BRICK);

        simpleItem(EquipmentModifierItems.AUTO_SMELT);
        simpleItem(EquipmentModifierItems.BEHEADING);
        simpleItem(EquipmentModifierItems.EFFICIENCY);
        simpleItem(EquipmentModifierItems.EXCAVATION);
        simpleItem(EquipmentModifierItems.FORTUNE);
        simpleItem(EquipmentModifierItems.IGNITE);
        simpleItem(EquipmentModifierItems.KNOCKBACK);
        simpleItem(EquipmentModifierItems.LIFESTEAL);
        simpleItem(EquipmentModifierItems.LOOTING);
        simpleItem(EquipmentModifierItems.MAGNET);
        simpleItem(EquipmentModifierItems.PROTECTION);
        simpleItem(EquipmentModifierItems.REPAIRING);
        simpleItem(EquipmentModifierItems.SHARPNESS);
        simpleItem(EquipmentModifierItems.SILK_TOUCH);
        simpleItem(EquipmentModifierItems.STEP_ASSIST);
        simpleItem(EquipmentModifierItems.TELEPORTING);
        simpleItem(EquipmentModifierItems.TORCH_PLACING);
        simpleItem(EquipmentModifierItems.UNBREAKING);


        simpleItem(ModItems.FLUID_MOVER);

        simpleBucketItem("molten_coal_bucket", CastingFluids.MOLTEN_COAL.getFluid());
        simpleBucketItem("molten_copper_bucket", CastingFluids.MOLTEN_COPPER.getFluid());
        simpleBucketItem("molten_diamond_bucket", CastingFluids.MOLTEN_DIAMOND.getFluid());
        simpleBucketItem("molten_emerald_bucket", CastingFluids.MOLTEN_EMERALD.getFluid());
        simpleBucketItem("molten_gold_bucket", CastingFluids.MOLTEN_GOLD.getFluid());
        simpleBucketItem("molten_iron_bucket", CastingFluids.MOLTEN_IRON.getFluid());
        simpleBucketItem("molten_lapis_bucket", CastingFluids.MOLTEN_LAPIS.getFluid());
        simpleBucketItem("molten_lead_bucket", CastingFluids.MOLTEN_LEAD.getFluid());
        simpleBucketItem("molten_osmium_bucket", CastingFluids.MOLTEN_OSMIUM.getFluid());
        simpleBucketItem("molten_redstone_bucket", CastingFluids.MOLTEN_REDSTONE.getFluid());
        simpleBucketItem("molten_tin_bucket", CastingFluids.MOLTEN_TIN.getFluid());
        simpleBucketItem("molten_uranium_bucket", CastingFluids.MOLTEN_URANIUM.getFluid());
        simpleBucketItem("molten_glass_bucket", CastingFluids.MOLTEN_GLASS.getFluid());
        simpleBucketItem("molten_debris_bucket", CastingFluids.MOLTEN_DEBRIS.getFluid());
        simpleBucketItem("molten_quartz_bucket", CastingFluids.MOLTEN_QUARTZ.getFluid());
        simpleBucketItem("molten_stone_bucket", CastingFluids.MOLTEN_STONE.getFluid());
        simpleBucketItem("molten_silver_bucket", CastingFluids.MOLTEN_SILVER.getFluid());
        simpleBucketItem("molten_aluminum_bucket", CastingFluids.MOLTEN_ALUMINUM.getFluid());
        simpleBucketItem("molten_nickel_bucket", CastingFluids.MOLTEN_NICKEL.getFluid());
        simpleBucketItem("molten_zinc_bucket", CastingFluids.MOLTEN_ZINC.getFluid());
        simpleBucketItem("molten_iridium_bucket", CastingFluids.MOLTEN_IRIDIUM.getFluid());
        simpleBucketItem("molten_platinum_bucket", CastingFluids.MOLTEN_PLATINUM.getFluid());
        simpleBucketItem("molten_invar_bucket", CastingFluids.MOLTEN_INVAR.getFluid());
        simpleBucketItem("molten_glowstone_bucket", CastingFluids.MOLTEN_GLOWSTONE.getFluid());
        simpleBucketItem("molten_ender_bucket", CastingFluids.MOLTEN_ENDER.getFluid());
        simpleBucketItem("molten_end_stone_bucket", CastingFluids.MOLTEN_END_STONE.getFluid());
        simpleBucketItem("molten_silicon_bucket", CastingFluids.MOLTEN_SILICON.getFluid());
        simpleBucketItem("molten_soul_bucket", CastingFluids.MOLTEN_SOUL.getFluid());

        simpleBucketItem("molten_bronze_bucket", CastingFluids.MOLTEN_BRONZE.getFluid());
        simpleBucketItem("molten_obsidian_bucket", CastingFluids.MOLTEN_OBSIDIAN.getFluid());
        simpleBucketItem("molten_steel_bucket", CastingFluids.MOLTEN_STEEL.getFluid());
        simpleBucketItem("molten_netherite_bucket", CastingFluids.MOLTEN_NETHERITE.getFluid());
        simpleBucketItem("molten_electrum_bucket", CastingFluids.MOLTEN_ELECTRUM.getFluid());
        simpleBucketItem("molten_signalum_bucket", CastingFluids.MOLTEN_SIGNALUM.getFluid());
        simpleBucketItem("molten_lumium_bucket", CastingFluids.MOLTEN_LUMIUM.getFluid());
        simpleBucketItem("molten_enderium_bucket", CastingFluids.MOLTEN_ENDERIUM.getFluid());
        simpleBucketItem("molten_constantan_bucket", CastingFluids.MOLTEN_CONSTANTAN.getFluid());
        simpleBucketItem("molten_brass_bucket", CastingFluids.MOLTEN_BRASS.getFluid());
        simpleBucketItem("molten_energetic_alloy_bucket", CastingFluids.MOLTEN_ENERGETIC_ALLOY.getFluid());
        simpleBucketItem("molten_vibrant_alloy_bucket", CastingFluids.MOLTEN_VIBRANT_ALLOY.getFluid());
        simpleBucketItem("molten_pulsating_alloy_bucket", CastingFluids.MOLTEN_PULSATING_ALLOY.getFluid());
        simpleBucketItem("molten_copper_alloy_bucket", CastingFluids.MOLTEN_COPPER_ALLOY.getFluid());
        simpleBucketItem("molten_dark_steel_bucket", CastingFluids.MOLTEN_DARK_STEEL.getFluid());
        simpleBucketItem("molten_soularium_bucket", CastingFluids.MOLTEN_SOULARIUM.getFluid());
        simpleBucketItem("molten_end_steel_bucket", CastingFluids.MOLTEN_END_STEEL.getFluid());
        simpleBucketItem("molten_conductive_alloy_bucket", CastingFluids.MOLTEN_CONDUCTIVE_ALLOY.getFluid());
        simpleBucketItem("molten_redstone_alloy_bucket", CastingFluids.MOLTEN_REDSTONE_ALLOY.getFluid());






    }

    private void simpleBucketItem(String name, Fluid fluid) {
        withExistingParent(name, ResourceLocation.fromNamespaceAndPath(NeoForgeVersion.MOD_ID, "item/bucket"))
                .customLoader(DynamicFluidContainerModelBuilder::begin)
                .fluid(fluid);
    }


    private void simpleItem(DeferredItem<Item> item) {
        withExistingParent(item.getId().getPath(),
                ResourceLocation.withDefaultNamespace("item/generated")).texture("layer0",
                ResourceLocation.fromNamespaceAndPath(Casting.MOD_ID, "item/" + item.getId().getPath()));
    }

    private ItemModelBuilder simpleBlockItem(DeferredBlock<Block> item) {
        return withExistingParent(item.getId().getPath(),
                ResourceLocation.withDefaultNamespace("item/generated")).texture("layer0",
                ResourceLocation.fromNamespaceAndPath(Casting.MOD_ID,"item/" + item.getId().getPath()));
    }


    @Override
    public String getName() {
        return Casting.MOD_ID + " Item Models";
    }
}
