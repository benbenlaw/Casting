package com.benbenlaw.casting.data;

import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.tags.ItemTags;
import net.minecraft.tags.TagKey;
import net.minecraft.world.item.Item;

import java.util.Objects;

public class ModdedTags {

    // TODO - MOVE TO OPOLISUTILITIES


    //Type Tags

    public static TagKey<Item> gearTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "gears")))));

    public static TagKey<Item> plateTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "plates")))));

    public static TagKey<Item> rodTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "rods")))));
    
    public static TagKey<Item> stoneRodTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "rods/stone")))));

    public static TagKey<Item> diamondNuggetTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "nuggets/diamond")))));

    public static TagKey<Item> emeraldNuggetTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "nuggets/emerald")))));

    public static TagKey<Item> copperNuggetTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "nuggets/copper")))));


    //Dust Tags


    public static TagKey<Item> copperDustTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "dusts/copper")))));
    public static TagKey<Item> ironDustTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "dusts/iron")))));
    public static TagKey<Item> goldDustTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "dusts/gold")))));
    public static TagKey<Item> diamondDustTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "dusts/diamond")))));
    public static TagKey<Item> emeraldDustTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "dusts/emerald")))));
    public static TagKey<Item> coalDustTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "dusts/coal")))));
    public static TagKey<Item> lapisDustTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "dusts/lapis")))));
    public static TagKey<Item> quartzDustTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "dusts/quartz")))));



    //Gear Tags

    public static TagKey<Item> ironGearTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "gears/iron")))));

    public static TagKey<Item> goldGearTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "gears/gold")))));

    public static TagKey<Item> copperGearTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "gears/copper")))));

    public static TagKey<Item> diamondGearTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "gears/diamond")))));

    public static TagKey<Item> emeraldGearTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "gears/emerald")))));

    public static TagKey<Item> stoneGearTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "gears/stone")))));
    public static TagKey<Item> lapisGearTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "gears/lapis")))));

    //Silicon

    public static TagKey<Item> silicon = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "silicon")))));

    public static TagKey<Item> siliconDust = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "dusts/silicon")))));

    //Rod Tags

    public static TagKey<Item> ironRodTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "rods/iron")))));

    public static TagKey<Item> goldRodTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "rods/gold")))));

    public static TagKey<Item> copperRodTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "rods/copper")))));

    //Plate Tags

    public static TagKey<Item> ironPlateTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "plates/iron")))));

    public static TagKey<Item> goldPlateTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "plates/gold")))));

    public static TagKey<Item> copperPlateTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "plates/copper")))));

    public static TagKey<Item> stonePlateTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "plates/stone")))));


    //Netherite Tags

    public static TagKey<Item> netheriteNuggetTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "nuggets/netherite")))));
    public static TagKey<Item> netheriteGearTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "gears/netherite")))));
    public static TagKey<Item> netheriteRodTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "rods/netherite")))));
    public static TagKey<Item> netheritePlateTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "plates/netherite")))));
    public static TagKey<Item> netheriteDustTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "dusts/netherite")))));






    //Tin Tags
    public static TagKey<Item> tinNuggetTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "nuggets/tin")))));
    public static TagKey<Item> tinIngotTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "ingots/tin")))));
    public static TagKey<Item> tinBlockTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "storage_blocks/tin")))));
    public static TagKey<Item> tinOreTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "ores/tin")))));
    public static TagKey<Item> tinRawOreTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "raw_materials/tin")))));
    public static TagKey<Item> tinGearTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "gears/tin")))));
    public static TagKey<Item> tinRodTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "rods/tin")))));
    public static TagKey<Item> tinPlateTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "plates/tin")))));
    public static TagKey<Item> tinRawOreBlockTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "storage_blocks/raw_tin")))));
    public static TagKey<Item> tinDustTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "dusts/tin")))));



    //Osmium Tags

    public static TagKey<Item> osmiumNuggetTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "nuggets/osmium")))));
    public static TagKey<Item> osmiumIngotTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "ingots/osmium")))));
    public static TagKey<Item> osmiumBlockTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "storage_blocks/osmium")))));
    public static TagKey<Item> osmiumOreTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "ores/osmium")))));
    public static TagKey<Item> osmiumRawOreTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "raw_materials/osmium")))));
    public static TagKey<Item> osmiumGearTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "gears/osmium")))));
    public static TagKey<Item> osmiumRodTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "rods/osmium")))));
    public static TagKey<Item> osmiumPlateTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "plates/osmium")))));
    public static TagKey<Item> osmiumRawOreBlockTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "storage_blocks/raw_osmium")))));
    public static TagKey<Item> osmiumDustTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "dusts/osmium")))));


    //Uranium Tags

    public static TagKey<Item> uraniumNuggetTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "nuggets/uranium")))));
    public static TagKey<Item> uraniumIngotTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "ingots/uranium")))));
    public static TagKey<Item> uraniumBlockTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "storage_blocks/uranium")))));
    public static TagKey<Item> uraniumOreTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "ores/uranium")))));
    public static TagKey<Item> uraniumRawOreTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "raw_materials/uranium")))));
    public static TagKey<Item> uraniumGearTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "gears/uranium")))));
    public static TagKey<Item> uraniumRodTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "rods/uranium")))));
    public static TagKey<Item> uraniumPlateTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "plates/uranium")))));
    public static TagKey<Item> uraniumRawOreBlockTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "storage_blocks/raw_uranium")))));
    public static TagKey<Item> uraniumDustTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "dusts/uranium")))));


    //Lead Tags

    public static TagKey<Item> leadNuggetTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "nuggets/lead")))));
    public static TagKey<Item> leadIngotTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "ingots/lead")))));
    public static TagKey<Item> leadBlockTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "storage_blocks/lead")))));
    public static TagKey<Item> leadOreTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "ores/lead")))));
    public static TagKey<Item> leadRawOreTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "raw_materials/lead")))));
    public static TagKey<Item> leadGearTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "gears/lead")))));
    public static TagKey<Item> leadRodTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "rods/lead")))));
    public static TagKey<Item> leadPlateTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "plates/lead")))));
    public static TagKey<Item> leadRawOreBlockTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "storage_blocks/raw_lead")))));
    public static TagKey<Item> leadDustTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "dusts/lead")))));


    //Silver Tags

    public static TagKey<Item> silverNuggetTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "nuggets/silver")))));
    public static TagKey<Item> silverIngotTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "ingots/silver")))));
    public static TagKey<Item> silverBlockTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "storage_blocks/silver")))));
    public static TagKey<Item> silverOreTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "ores/silver")))));
    public static TagKey<Item> silverRawOreTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "raw_materials/silver")))));
    public static TagKey<Item> silverGearTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "gears/silver")))));
    public static TagKey<Item> silverRodTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "rods/silver")))));
    public static TagKey<Item> silverPlateTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "plates/silver")))));
    public static TagKey<Item> silverRawOreBlockTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "storage_blocks/raw_silver")))));
    public static TagKey<Item> silverDustTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "dusts/silver")))));


    //Bronze Tags

    public static TagKey<Item> bronzeNuggetTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "nuggets/bronze")))));
    public static TagKey<Item> bronzeIngotTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "ingots/bronze")))));
    public static TagKey<Item> bronzeBlockTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "storage_blocks/bronze")))));
    public static TagKey<Item> bronzeGearTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "gears/bronze")))));
    public static TagKey<Item> bronzeRodTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "rods/bronze")))));
    public static TagKey<Item> bronzePlateTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "plates/bronze")))));
    public static TagKey<Item> bronzeDustTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "dusts/bronze")))));


    //Steel Tags

    public static TagKey<Item> steelNuggetTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "nuggets/steel")))));
    public static TagKey<Item> steelIngotTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "ingots/steel")))));
    public static TagKey<Item> steelBlockTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "storage_blocks/steel")))));
    public static TagKey<Item> steelGearTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "gears/steel")))));
    public static TagKey<Item> steelRodTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "rods/steel")))));
    public static TagKey<Item> steelPlateTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "plates/steel")))));
    public static TagKey<Item> steelDustTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "dusts/steel")))));


    //Electrum Tags

    public static TagKey<Item> electrumNuggetTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "nuggets/electrum")))));
    public static TagKey<Item> electrumIngotTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "ingots/electrum")))));
    public static TagKey<Item> electrumBlockTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "storage_blocks/electrum")))));
    public static TagKey<Item> electrumGearTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "gears/electrum")))));
    public static TagKey<Item> electrumRodTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "rods/electrum")))));
    public static TagKey<Item> electrumPlateTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "plates/electrum")))));
    public static TagKey<Item> electrumDustTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "dusts/electrum")))));

    //Nickel Tags

    public static TagKey<Item> nickelOreTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "ores/nickel")))));
    public static TagKey<Item> nickelRawOreTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "raw_materials/nickel")))));
    public static TagKey<Item> nickelNuggetTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "nuggets/nickel")))));
    public static TagKey<Item> nickelIngotTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "ingots/nickel")))));
    public static TagKey<Item> nickelBlockTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "storage_blocks/nickel")))));
    public static TagKey<Item> nickelGearTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "gears/nickel")))));
    public static TagKey<Item> nickelRodTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "rods/nickel")))));
    public static TagKey<Item> nickelPlateTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "plates/nickel")))));
    public static TagKey<Item> nickelRawOreBlockTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "storage_blocks/raw_nickel")))));
    public static TagKey<Item> nickelDustTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "dusts/nickel")))));



    //Invar Tags

    public static TagKey<Item> invarNuggetTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "nuggets/invar")))));
    public static TagKey<Item> invarIngotTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "ingots/invar")))));
    public static TagKey<Item> invarBlockTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "storage_blocks/invar")))));
    public static TagKey<Item> invarGearTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "gears/invar")))));
    public static TagKey<Item> invarRodTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "rods/invar")))));
    public static TagKey<Item> invarPlateTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "plates/invar")))));
    public static TagKey<Item> invarDustTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "dusts/invar")))));


    //Aluminum Tags

    public static TagKey<Item> aluminumNuggetTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "nuggets/aluminum")))));
    public static TagKey<Item> aluminumIngotTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "ingots/aluminum")))));
    public static TagKey<Item> aluminumBlockTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "storage_blocks/aluminum")))));
    public static TagKey<Item> aluminumOreTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "ores/aluminum")))));
    public static TagKey<Item> aluminumRawOreTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "raw_materials/aluminum")))));
    public static TagKey<Item> aluminumGearTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "gears/aluminum")))));
    public static TagKey<Item> aluminumRodTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "rods/aluminum")))));
    public static TagKey<Item> aluminumPlateTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "plates/aluminum")))));
    public static TagKey<Item> aluminumRawOreBlockTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "storage_blocks/raw_aluminum")))));
    public static TagKey<Item> aluminumDustTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "dusts/aluminum")))));

    //Zinc Tags

    public static TagKey<Item> zincNuggetTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "nuggets/zinc")))));
    public static TagKey<Item> zincIngotTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "ingots/zinc")))));
    public static TagKey<Item> zincBlockTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "storage_blocks/zinc")))));
    public static TagKey<Item> zincOreTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "ores/zinc")))));
    public static TagKey<Item> zincRawOreTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "raw_materials/zinc")))));
    public static TagKey<Item> zincGearTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "gears/zinc")))));
    public static TagKey<Item> zincRodTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "rods/zinc")))));
    public static TagKey<Item> zincPlateTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "plates/zinc")))));
    public static TagKey<Item> zincRawOreBlockTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "storage_blocks/raw_zinc")))));
    public static TagKey<Item> zincDustTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "dusts/zinc")))));

    //Iridium Tags

    public static TagKey<Item> iridiumNuggetTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "nuggets/iridium")))));
    public static TagKey<Item> iridiumIngotTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "ingots/iridium")))));
    public static TagKey<Item> iridiumBlockTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "storage_blocks/iridium")))));
    public static TagKey<Item> iridiumOreTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "ores/iridium")))));
    public static TagKey<Item> iridiumRawOreTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "raw_materials/iridium")))));
    public static TagKey<Item> iridiumGearTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "gears/iridium")))));
    public static TagKey<Item> iridiumRodTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "rods/iridium")))));
    public static TagKey<Item> iridiumPlateTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "plates/iridium")))));
    public static TagKey<Item> iridiumRawOreBlockTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "storage_blocks/raw_iridium")))));
    public static TagKey<Item> iridiumDustTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "dusts/iridium")))));

    //Platinum Tags

    public static TagKey<Item> platinumNuggetTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "nuggets/platinum")))));
    public static TagKey<Item> platinumIngotTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "ingots/platinum")))));
    public static TagKey<Item> platinumBlockTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "storage_blocks/platinum")))));
    public static TagKey<Item> platinumOreTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "ores/platinum")))));
    public static TagKey<Item> platinumRawOreTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "raw_materials/platinum")))));
    public static TagKey<Item> platinumGearTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "gears/platinum")))));
    public static TagKey<Item> platinumRodTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "rods/platinum")))));
    public static TagKey<Item> platinumPlateTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "plates/platinum")))));
    public static TagKey<Item> platinumRawOreBlockTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "storage_blocks/raw_platinum")))));
    public static TagKey<Item> platinumDustTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "dusts/platinum")))));

    //Signalum Tags

    public static TagKey<Item> signalumNuggetTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "nuggets/signalum")))));
    public static TagKey<Item> signalumIngotTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "ingots/signalum")))));
    public static TagKey<Item> signalumBlockTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "storage_blocks/signalum")))));
    public static TagKey<Item> signalumGearTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "gears/signalum")))));
    public static TagKey<Item> signalumRodTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "rods/signalum")))));
    public static TagKey<Item> signalumPlateTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "plates/signalum")))));
    public static TagKey<Item> signalumDustTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "dusts/signalum")))));


    //Lumium Tags

    public static TagKey<Item> lumiumNuggetTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "nuggets/lumium")))));
    public static TagKey<Item> lumiumIngotTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "ingots/lumium")))));
    public static TagKey<Item> lumiumBlockTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "storage_blocks/lumium")))));
    public static TagKey<Item> lumiumGearTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "gears/lumium")))));
    public static TagKey<Item> lumiumRodTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "rods/lumium")))));
    public static TagKey<Item> lumiumPlateTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "plates/lumium")))));
    public static TagKey<Item> lumiumDustTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "dusts/lumium")))));

    //Enderium Tags

    public static TagKey<Item> enderiumNuggetTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "nuggets/enderium")))));
    public static TagKey<Item> enderiumIngotTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "ingots/enderium")))));
    public static TagKey<Item> enderiumBlockTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "storage_blocks/enderium")))));
    public static TagKey<Item> enderiumGearTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "gears/enderium")))));
    public static TagKey<Item> enderiumRodTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "rods/enderium")))));
    public static TagKey<Item> enderiumPlateTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "plates/enderium")))));
    public static TagKey<Item> enderiumDustTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "dusts/enderium")))));

    //Constantan Tags

    public static TagKey<Item> constantanNuggetTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "nuggets/constantan")))));
    public static TagKey<Item> constantanIngotTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "ingots/constantan")))));
    public static TagKey<Item> constantanBlockTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "storage_blocks/constantan")))));
    public static TagKey<Item> constantanGearTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "gears/constantan")))));
    public static TagKey<Item> constantanRodTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "rods/constantan")))));
    public static TagKey<Item> constantanPlateTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "plates/constantan")))));
    public static TagKey<Item> constantanDustTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "dusts/constantan")))));

    //Brass Tags

    public static TagKey<Item> brassNuggetTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "nuggets/brass")))));
    public static TagKey<Item> brassIngotTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "ingots/brass")))));
    public static TagKey<Item> brassBlockTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "storage_blocks/brass")))));
    public static TagKey<Item> brassGearTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "gears/brass")))));
    public static TagKey<Item> brassRodTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "rods/brass")))));
    public static TagKey<Item> brassPlateTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "plates/brass")))));
    public static TagKey<Item> brassDustTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "dusts/brass")))));

    //Conductive Alloy

    public static TagKey<Item> conductiveAlloyNuggetTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "nuggets/conductive_alloy")))));
    public static TagKey<Item> conductiveAlloyIngotTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "ingots/conductive_alloy")))));
    public static TagKey<Item> conductiveAlloyBlockTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "storage_blocks/conductive_alloy")))));
    public static TagKey<Item> conductiveAlloyGearTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "gears/conductive_alloy")))));
    public static TagKey<Item> conductiveAlloyRodTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "rods/conductive_alloy")))));
    public static TagKey<Item> conductiveAlloyPlateTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "plates/conductive_alloy")))));
    public static TagKey<Item> conductiveAlloyDustTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "dusts/conductive_alloy")))));

    //Energetic Alloy

    public static TagKey<Item> energeticAlloyNuggetTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "nuggets/energetic_alloy")))));
    public static TagKey<Item> energeticAlloyIngotTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "ingots/energetic_alloy")))));
    public static TagKey<Item> energeticAlloyBlockTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "storage_blocks/energetic_alloy")))));
    public static TagKey<Item> energeticAlloyGearTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "gears/energized")))));
    public static TagKey<Item> energeticAlloyRodTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "rods/energetic_alloy")))));
    public static TagKey<Item> energeticAlloyPlateTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "plates/energetic_alloy")))));
    public static TagKey<Item> energeticAlloyDustTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "dusts/energetic_alloy")))));


    //Vibrant Alloy

    public static TagKey<Item> vibrantAlloyNuggetTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "nuggets/vibrant_alloy")))));
    public static TagKey<Item> vibrantAlloyIngotTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "ingots/vibrant_alloy")))));
    public static TagKey<Item> vibrantAlloyBlockTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "storage_blocks/vibrant_alloy")))));
    public static TagKey<Item> vibrantAlloyGearTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "gears/vibrant")))));
    public static TagKey<Item> vibrantAlloyRodTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "rods/vibrant_alloy")))));
    public static TagKey<Item> vibrantAlloyPlateTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "plates/vibrant_alloy")))));
    public static TagKey<Item> vibrantAlloyDustTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "dusts/vibrant_alloy")))));

    //Pulsating Alloy

    public static TagKey<Item> pulsatingAlloyNuggetTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "nuggets/pulsating_alloy")))));
    public static TagKey<Item> pulsatingAlloyIngotTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "ingots/pulsating_alloy")))));
    public static TagKey<Item> pulsatingAlloyBlockTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "storage_blocks/pulsating_alloy")))));
    public static TagKey<Item> pulsatingAlloyGearTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "gears/pulsating_alloy")))));
    public static TagKey<Item> pulsatingAlloyRodTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "rods/pulsating_alloy")))));
    public static TagKey<Item> pulsatingAlloyPlateTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "plates/pulsating_alloy")))));
    public static TagKey<Item> pulsatingAlloyDustTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "dusts/pulsating_alloy")))));

    //Soularium Tags



    public static TagKey<Item> soulariumNuggetTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "nuggets/soularium")))));
    public static TagKey<Item> soulariumIngotTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "ingots/soularium")))));
    public static TagKey<Item> soulariumBlockTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "storage_blocks/soularium")))));
    public static TagKey<Item> soulariumGearTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "gears/soularium")))));
    public static TagKey<Item> soulariumRodTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "rods/soularium")))));
    public static TagKey<Item> soulariumPlateTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "plates/soularium")))));
    public static TagKey<Item> soulariumDustTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "dusts/soularium")))));

    //End Steel

    public static TagKey<Item> endSteelNuggetTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "nuggets/end_steel")))));
    public static TagKey<Item> endSteelIngotTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "ingots/end_steel")))));
    public static TagKey<Item> endSteelBlockTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "storage_blocks/end_steel")))));
    public static TagKey<Item> endSteelGearTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "gears/end_steel")))));
    public static TagKey<Item> endSteelRodTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "rods/end_steel")))));
    public static TagKey<Item> endSteelPlateTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "plates/end_steel")))));
    public static TagKey<Item> endSteelDustTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "dusts/end_steel")))));

    //Dark Steel

    public static TagKey<Item> darkSteelNuggetTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "nuggets/dark_steel")))));
    public static TagKey<Item> darkSteelIngotTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "ingots/dark_steel")))));
    public static TagKey<Item> darkSteelBlockTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "storage_blocks/dark_steel")))));
    public static TagKey<Item> darkSteelGearTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "gears/dark_steel")))));
    public static TagKey<Item> darkSteelRodTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "rods/dark_steel")))));
    public static TagKey<Item> darkSteelPlateTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "plates/dark_steel")))));
    public static TagKey<Item> darkSteelDustTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "dusts/dark_steel")))));

    //Redstone Alloy

    public static TagKey<Item> redstoneAlloyNuggetTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "nuggets/redstone_alloy")))));
    public static TagKey<Item> redstoneAlloyIngotTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "ingots/redstone_alloy")))));
    public static TagKey<Item> redstoneAlloyBlockTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "storage_blocks/redstone_alloy")))));
    public static TagKey<Item> redstoneAlloyGearTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "gears/redstone_alloy")))));
    public static TagKey<Item> redstoneAlloyRodTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "rods/redstone_alloy")))));
    public static TagKey<Item> redstoneAlloyPlateTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "plates/redstone_alloy")))));
    public static TagKey<Item> redstoneAlloyDustTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "dusts/redstone_alloy")))));

    //Copper Alloy

    public static TagKey<Item> copperAlloyNuggetTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "nuggets/copper_alloy")))));
    public static TagKey<Item> copperAlloyIngotTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "ingots/copper_alloy")))));
    public static TagKey<Item> copperAlloyBlockTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "storage_blocks/copper_alloy")))));
    public static TagKey<Item> copperAlloyGearTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "gears/copper_alloy")))));
    public static TagKey<Item> copperAlloyRodTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "rods/copper_alloy")))));
    public static TagKey<Item> copperAlloyPlateTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "plates/copper_alloy")))));
    public static TagKey<Item> copperAlloyDustTag = ItemTags.create(Objects.requireNonNull(ResourceLocation.tryParse(
            String.valueOf(ResourceLocation.fromNamespaceAndPath("c", "dusts/copper_alloy")))));










}
