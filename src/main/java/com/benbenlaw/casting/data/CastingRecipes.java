package com.benbenlaw.casting.data;

import com.benbenlaw.casting.Casting;
import com.benbenlaw.casting.block.ModBlocks;
import com.benbenlaw.casting.data.recipes.*;
import com.benbenlaw.casting.fluid.CastingFluids;
import com.benbenlaw.casting.item.ModItems;
import com.benbenlaw.casting.util.CastingTags;
import net.minecraft.core.HolderLookup;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.data.PackOutput;
import net.minecraft.data.recipes.*;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.tags.ItemTags;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.Items;
import net.minecraft.world.item.crafting.Ingredient;
import net.minecraft.world.level.block.Blocks;
import net.minecraft.world.level.material.Fluids;
import net.neoforged.neoforge.common.Tags;
import net.neoforged.neoforge.common.conditions.ModLoadedCondition;
import net.neoforged.neoforge.common.conditions.NotCondition;
import net.neoforged.neoforge.common.conditions.TagEmptyCondition;
import net.neoforged.neoforge.common.crafting.SizedIngredient;
import net.neoforged.neoforge.fluids.FluidStack;

import java.util.concurrent.CompletableFuture;

import static com.benbenlaw.casting.data.ModdedTags.*;
import static com.benbenlaw.casting.util.ValidToolTypesForToolModifiers.*;

public class CastingRecipes extends RecipeProvider {

    public CastingRecipes(PackOutput output, CompletableFuture<HolderLookup.Provider> completableFuture) {
        super(output, completableFuture);
    }


    @Override
    protected void buildRecipes(RecipeOutput consumer) {

        //Tool Modifier
        toolModifierRecipes(consumer, null, new FluidStack(CastingFluids.MOLTEN_LAPIS.getFluid(), 5400), FORTUNE);
        toolModifierRecipes(consumer, null, new FluidStack(CastingFluids.MOLTEN_REDSTONE.getFluid(), 5400), EFFICIENCY);
        toolModifierRecipes(consumer, null, new FluidStack(CastingFluids.MOLTEN_EMERALD.getFluid(), 2700), SILK_TOUCH);
        toolModifierRecipes(consumer, null, new FluidStack(CastingFluids.MOLTEN_OBSIDIAN.getFluid(), 32000), UNBREAKING);
        toolModifierRecipes(consumer, new SizedIngredient(Ingredient.of(com.benbenlaw.opolisutilities.block.ModBlocks.ITEM_REPAIRER.get()), 1), null, REPAIRING);
        toolModifierRecipes(consumer, null, new FluidStack(CastingFluids.MOLTEN_GLOWSTONE.getFluid(), 16000), REPAIRING);
        toolModifierRecipes(consumer, new SizedIngredient(Ingredient.of(Tags.Items.RODS_WOODEN), 64), new FluidStack(CastingFluids.MOLTEN_COAL.getFluid(), 5120), TORCH_PLACING);
        toolModifierRecipes(consumer, null, new FluidStack(Fluids.LAVA, 16000), AUTO_SMELT);
        toolModifierRecipes(consumer, new SizedIngredient(Ingredient.of(Tags.Items.GEMS_EMERALD), 8), new FluidStack(CastingFluids.MOLTEN_LAPIS.getFluid(), 16000), LOOTING);
        toolModifierRecipes(consumer, null, new FluidStack(CastingFluids.MOLTEN_QUARTZ.getFluid(), 2700), SHARPNESS);
        toolModifierRecipes(consumer, null, new FluidStack(CastingFluids.MOLTEN_SOUL.getFluid(), 2560), BEHEADING);
        toolModifierRecipes(consumer, new SizedIngredient(Ingredient.of(Items.GOLDEN_APPLE), 1), new FluidStack(CastingFluids.MOLTEN_GOLD.getFluid(), 6480), LIFESTEAL);
        toolModifierRecipes(consumer, new SizedIngredient(Ingredient.of(Items.PISTON), 8), null,KNOCKBACK);
        toolModifierRecipes(consumer, new SizedIngredient(Ingredient.of(Items.FLINT_AND_STEEL), 1), new FluidStack(Fluids.LAVA, 16000),IGNITE);
        toolModifierRecipes(consumer, null, new FluidStack(CastingFluids.MOLTEN_DIAMOND.getFluid(), 900),EXCAVATION);
        toolModifierRecipes(consumer, null, new FluidStack(CastingFluids.MOLTEN_ENDER.getFluid(), 2560), TELEPORTING);
        toolModifierRecipes(consumer, new SizedIngredient(Ingredient.of(Tags.Items.INGOTS_IRON), 12), new FluidStack(CastingFluids.MOLTEN_GOLD.getFluid(), 1080), MAGNET);
        toolModifierRecipes(consumer, null, new FluidStack(CastingFluids.MOLTEN_STEEL.getFluid(), 720), PROTECTION);
        toolModifierRecipes(consumer, new SizedIngredient(Ingredient.of(Items.PISTON), 6), null, STEP_ASSIST);
        toolModifierRecipes(consumer, new SizedIngredient(Ingredient.of(Items.GOLDEN_CARROT), 12), null, NIGHT_VISION);
        toolModifierRecipes(consumer, new SizedIngredient(Ingredient.of(Items.PUFFERFISH), 6), null, WATER_BREATHING);

        //Tool Modifier
        ShapedRecipeBuilder.shaped(RecipeCategory.MISC, ModBlocks.EQUIPMENT_MODIFIER.get(), 1)
                .pattern("BBB")
                .pattern("STS")
                .pattern("BBB")
                .define('B', ModBlocks.BLACK_BRICKS.get())
                .define('S', ModBlocks.SOLIDIFIER.get())
                .define('T', Tags.Items.PLAYER_WORKSTATIONS_CRAFTING_TABLES)
                .unlockedBy("has_black_brick", has(ModItems.BLACK_BRICK.get()))
                .save(consumer);

        //Fluid Mover
        ShapedRecipeBuilder.shaped(RecipeCategory.MISC, ModItems.FLUID_MOVER.get(), 1)
                .pattern(" BB")
                .pattern(" BB")
                .pattern("B  ")
                .define('B', ModItems.BLACK_BRICK.get())
                .unlockedBy("has_black_brick", has(ModItems.BLACK_BRICK.get()))
                .save(consumer);

        //Black Brick

        SimpleCookingRecipeBuilder.smelting(Ingredient.of(Items.BRICK), RecipeCategory.MISC, new ItemStack(ModItems.BLACK_BRICK.get()), 0.5f, 200)
                .unlockedBy("has_brick", has(Items.BRICK))
                .save(consumer);

        ShapedRecipeBuilder.shaped(RecipeCategory.MISC, ModBlocks.BLACK_BRICKS.get(), 1)
                .pattern("BB")
                .pattern("BB")
                .define('B', ModItems.BLACK_BRICK.get())
                .unlockedBy("has_black_brick", has(ModItems.BLACK_BRICK.get()))
                .save(consumer);

        // Tank
        ShapedRecipeBuilder.shaped(RecipeCategory.MISC, ModBlocks.TANK.get(), 1)
                .pattern("BBB")
                .pattern("BGB")
                .pattern("BBB")
                .define('B', ModItems.BLACK_BRICK.get())
                .define('G', Tags.Items.GLASS_BLOCKS)
                .unlockedBy("has_black_brick", has(ModItems.BLACK_BRICK.get()))
                .save(consumer);

        // Controller
        ShapedRecipeBuilder.shaped(RecipeCategory.MISC, ModBlocks.CONTROLLER.get(), 1)
                .pattern("BBB")
                .pattern("B B")
                .pattern("BBB")
                .define('B', ModBlocks.BLACK_BRICKS.get())
                .unlockedBy("has_black_brick", has(ModItems.BLACK_BRICK.get()))
                .save(consumer);

        // Solidifier
        ShapedRecipeBuilder.shaped(RecipeCategory.MISC, ModBlocks.SOLIDIFIER.get(), 1)
                .pattern("BBB")
                .pattern("K K")
                .pattern("BBB")
                .define('B', ModBlocks.BLACK_BRICKS.get())
                .define('K', ModItems.BLACK_BRICK.get())
                .unlockedBy("has_black_brick", has(ModItems.BLACK_BRICK.get()))
                .save(consumer);

        // Mixer
        ShapedRecipeBuilder.shaped(RecipeCategory.MISC, ModBlocks.MIXER.get(), 1)
                .pattern("BBB")
                .pattern("T T")
                .pattern("BBB")
                .define('B', ModBlocks.BLACK_BRICKS.get())
                .define('T', ModBlocks.TANK)
                .unlockedBy("has_black_brick", has(ModBlocks.BLACK_BRICKS.get()))
                .save(consumer);

        // Mixer Whisk
        ShapedRecipeBuilder.shaped(RecipeCategory.MISC, ModBlocks.MIXER_WHISK.get(), 1)
                .pattern("BIB")
                .pattern("BSB")
                .pattern("BIB")
                .define('B', ModItems.BLACK_BRICK.get())
                .define('S', Tags.Items.STORAGE_BLOCKS_IRON)
                .define('I', Tags.Items.INGOTS_IRON)
                .unlockedBy("has_black_brick", has(ModItems.BLACK_BRICK.get()))
                .save(consumer);

        // Gear Mold
        ShapedRecipeBuilder.shaped(RecipeCategory.MISC, ModItems.GEAR_MOLD.get(), 1)
                .pattern(" B ")
                .pattern("BGB")
                .pattern(" B ")
                .define('B', ModItems.BLACK_BRICK.get())
                .define('G', gearTag)
                .unlockedBy("has_black_brick", has(ModItems.BLACK_BRICK.get()))
                .save(consumer.withConditions(new NotCondition(new TagEmptyCondition(gearTag))));

        // Ingot Mold
        ShapedRecipeBuilder.shaped(RecipeCategory.MISC, ModItems.INGOT_MOLD.get(), 1)
                .pattern(" B ")
                .pattern("BIB")
                .pattern(" B ")
                .define('B', ModItems.BLACK_BRICK.get())
                .define('I', Tags.Items.BRICKS)
                .unlockedBy("has_black_brick", has(ModItems.BLACK_BRICK.get()))
                .save(consumer);

        // Nugget Mold
        ShapedRecipeBuilder.shaped(RecipeCategory.MISC, ModItems.NUGGET_MOLD.get(), 1)
                .pattern(" B ")
                .pattern("BIB")
                .pattern(" B ")
                .define('B', ModItems.BLACK_BRICK.get())
                .define('I', Tags.Items.NUGGETS)
                .unlockedBy("has_black_brick", has(ModItems.BLACK_BRICK.get()))
                .save(consumer);

        // Plate Mold
        ShapedRecipeBuilder.shaped(RecipeCategory.MISC, ModItems.PLATE_MOLD.get(), 1)
                .pattern(" B ")
                .pattern("BIB")
                .pattern(" B ")
                .define('B', ModItems.BLACK_BRICK.get())
                .define('I', plateTag)
                .unlockedBy("has_black_brick", has(ModItems.BLACK_BRICK.get()))
                .save(consumer.withConditions(new NotCondition(new TagEmptyCondition(plateTag))));

        // Rod Mold
        ShapedRecipeBuilder.shaped(RecipeCategory.MISC, ModItems.ROD_MOLD.get(), 1)
                .pattern(" B ")
                .pattern("BIB")
                .pattern(" B ")
                .define('B', ModItems.BLACK_BRICK.get())
                .define('I', rodTag)
                .unlockedBy("has_black_brick", has(ModItems.BLACK_BRICK.get()))
                .save(consumer.withConditions(new NotCondition(new TagEmptyCondition(rodTag))));

        // Block Mold
        ShapedRecipeBuilder.shaped(RecipeCategory.MISC, ModItems.BLOCK_MOLD.get(), 1)
                .pattern(" B ")
                .pattern("BIB")
                .pattern(" B ")
                .define('B', ModItems.BLACK_BRICK.get())
                .define('I', Tags.Items.STORAGE_BLOCKS)
                .unlockedBy("has_black_brick", has(ModItems.BLACK_BRICK.get()))
                .save(consumer);

        // Gem Mold
        ShapedRecipeBuilder.shaped(RecipeCategory.MISC, ModItems.GEM_MOLD.get(), 1)
                .pattern(" B ")
                .pattern("BIB")
                .pattern(" B ")
                .define('B', ModItems.BLACK_BRICK.get())
                .define('I', Tags.Items.GEMS)
                .unlockedBy("has_black_brick", has(ModItems.BLACK_BRICK.get()))
                .save(consumer);

        // Dust Mold
        ShapedRecipeBuilder.shaped(RecipeCategory.MISC, ModItems.DUST_MOLD.get(), 1)
                .pattern(" B ")
                .pattern("BIB")
                .pattern(" B ")
                .define('B', ModItems.BLACK_BRICK.get())
                .define('I', Tags.Items.DUSTS)
                .unlockedBy("has_black_brick", has(ModItems.BLACK_BRICK.get()))
                .save(consumer);

        // Ball Mold
        ShapedRecipeBuilder.shaped(RecipeCategory.MISC, ModItems.BALL_MOLD.get(), 1)
                .pattern(" B ")
                .pattern("BIB")
                .pattern(" B ")
                .define('B', ModItems.BLACK_BRICK.get())
                .define('I', Items.CLAY_BALL)
                .unlockedBy("has_black_brick", has(ModItems.BLACK_BRICK.get()))
                .save(consumer);

        // Repairing Mold
        ShapedRecipeBuilder.shaped(RecipeCategory.MISC, ModItems.REPAIRING_MOLD.get(), 1)
                .pattern(" B ")
                .pattern("BIB")
                .pattern(" B ")
                .define('B', ModItems.BLACK_BRICK.get())
                .define('I', Items.ANVIL)
                .unlockedBy("has_black_brick", has(ModItems.BLACK_BRICK.get()))
                .save(consumer);

        // ********** Fuels ********** //

        // Lava
        FuelRecipeBuilder.FuelRecipesBuilder(new FluidStack(Fluids.LAVA, 10), 1000)
                .unlockedBy("has_item", has(Items.BUCKET))
                .save(consumer);

        // Coal
        FuelRecipeBuilder.FuelRecipesBuilder(new FluidStack(CastingFluids.MOLTEN_COAL.getFluid(), 8), 1200)
                .unlockedBy("has_item", has(Items.BUCKET))
                .save(consumer);

        // Obsidian
        FuelRecipeBuilder.FuelRecipesBuilder(new FluidStack(CastingFluids.MOLTEN_OBSIDIAN.getFluid(), 20), 1400)
                .unlockedBy("has_item", has(Items.BUCKET))
                .save(consumer);

        // Glowstone
        FuelRecipeBuilder.FuelRecipesBuilder(new FluidStack(CastingFluids.MOLTEN_GLOWSTONE.getFluid(), 25), 1600)
                .unlockedBy("has_item", has(Items.BUCKET))
                .save(consumer);

        // Netherite
        FuelRecipeBuilder.FuelRecipesBuilder(new FluidStack(CastingFluids.MOLTEN_NETHERITE.getFluid(), 10), 1800)
                .unlockedBy("has_item", has(Items.BUCKET))
                .save(consumer);

        // Uranium
        FuelRecipeBuilder.FuelRecipesBuilder(new FluidStack(CastingFluids.MOLTEN_URANIUM.getFluid(), 25), 2000)
                .unlockedBy("has_item", has(Items.BUCKET))
                .save(consumer);

        // Ender
        FuelRecipeBuilder.FuelRecipesBuilder(new FluidStack(CastingFluids.MOLTEN_ENDER.getFluid(), 5), 700)
                .unlockedBy("has_item", has(Items.BUCKET))
                .save(consumer);

        // Water
        FuelRecipeBuilder.FuelRecipesBuilder(new FluidStack(Fluids.WATER, 125), 50)
                .unlockedBy("has_item", has(Items.BUCKET))
                .save(consumer);


        // ********** Processing ********** //

        // Iron Processing

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(Tags.Items.NUGGETS_IRON), 1),
                        new FluidStack(CastingFluids.MOLTEN_IRON.getFluid(), 10), 1000)
                .unlockedBy("has_item", has(Tags.Items.INGOTS_IRON))
                .save(consumer, "casting:melting/iron/from_nugget");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(Tags.Items.INGOTS_IRON), 1),
                        new FluidStack(CastingFluids.MOLTEN_IRON.getFluid(), 90), 1000)
                .unlockedBy("has_item", has(Tags.Items.INGOTS_IRON))
                .save(consumer, "casting:melting/iron/from_ingot");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(ironDustTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_IRON.getFluid(), 90), 1000)
                .unlockedBy("has_item", has(ironDustTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(ironDustTag))), "casting:melting/iron/from_dust");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(Tags.Items.STORAGE_BLOCKS_IRON), 1),
                        new FluidStack(CastingFluids.MOLTEN_IRON.getFluid(), 810), 1000)
                .unlockedBy("has_item", has(Tags.Items.ORES_IRON))
                .save(consumer, "casting:melting/iron/from_block");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(Tags.Items.ORES_IRON), 1),
                        new FluidStack(CastingFluids.MOLTEN_IRON.getFluid(), 100), 1000)
                .unlockedBy("has_item", has(Tags.Items.ORES_IRON))
                .save(consumer, "casting:melting/iron/from_ore");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(Tags.Items.STORAGE_BLOCKS_RAW_IRON), 1),
                        new FluidStack(CastingFluids.MOLTEN_IRON.getFluid(), 900), 1000)
                .unlockedBy("has_item", has(Tags.Items.ORES_IRON))
                .save(consumer, "casting:melting/iron/from_raw_ore_block");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(Tags.Items.RAW_MATERIALS_IRON), 1),
                        new FluidStack(CastingFluids.MOLTEN_IRON.getFluid(), 100), 1000)
                .unlockedBy("has_item", has(Tags.Items.ORES_IRON))
                .save(consumer, "casting:melting/iron/from_raw_ore");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.BLOCK_MOLD, 1), new SizedIngredient(Ingredient.of(Items.IRON_BLOCK), 1),
                        new FluidStack(CastingFluids.MOLTEN_IRON.getFluid(), 810))
                .unlockedBy("has_item", has(Items.IRON_INGOT)).save(consumer, "casting:solidifier/iron/block");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.INGOT_MOLD, 1), new SizedIngredient(Ingredient.of(Items.IRON_INGOT), 1),
                        new FluidStack(CastingFluids.MOLTEN_IRON.getFluid(), 90))
                .unlockedBy("has_item", has(Items.IRON_INGOT)).save(consumer, "casting:solidifier/iron/ingot");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.NUGGET_MOLD, 1), new SizedIngredient(Ingredient.of(Items.IRON_NUGGET), 1),
                        new FluidStack(CastingFluids.MOLTEN_IRON.getFluid(), 10))
                .unlockedBy("has_item", has(Items.IRON_INGOT)).save(consumer, "casting:solidifier/iron/nugget");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.GEAR_MOLD, 1), new SizedIngredient(Ingredient.of(ironGearTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_IRON.getFluid(), 360))
                .unlockedBy("has_item", has(Items.IRON_INGOT)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(ironGearTag))), "casting:solidifier/iron/gear");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.ROD_MOLD, 1), new SizedIngredient(Ingredient.of(ironRodTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_IRON.getFluid(), 90))
                .unlockedBy("has_item", has(Items.IRON_INGOT)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(ironRodTag))), "casting:solidifier/iron/rod");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.PLATE_MOLD, 1), new SizedIngredient(Ingredient.of(ironPlateTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_IRON.getFluid(), 90))
                .unlockedBy("has_item", has(Items.IRON_INGOT)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(ironPlateTag))), "casting:solidifier/iron/plate");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.DUST_MOLD, 1), new SizedIngredient(Ingredient.of(ironDustTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_IRON.getFluid(), 90))
                .unlockedBy("has_item", has(ironDustTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(ironDustTag))), "casting:solidifier/iron/dust");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(Items.IRON_HELMET), 1),
                        new FluidStack(CastingFluids.MOLTEN_IRON.getFluid(), 450), 1000)
                .unlockedBy("has_item", has(Items.IRON_HELMET))
                .save(consumer, "casting:melting/iron/from_helmet");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(Items.IRON_CHESTPLATE), 1),
                        new FluidStack(CastingFluids.MOLTEN_IRON.getFluid(), 720), 1000)
                .unlockedBy("has_item", has(Items.IRON_CHESTPLATE))
                .save(consumer, "casting:melting/iron/from_chestplate");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(Items.IRON_LEGGINGS), 1),
                        new FluidStack(CastingFluids.MOLTEN_IRON.getFluid(), 630), 1000)
                .unlockedBy("has_item", has(Items.IRON_LEGGINGS))
                .save(consumer, "casting:melting/iron/from_leggings");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(Items.IRON_BOOTS), 1),
                        new FluidStack(CastingFluids.MOLTEN_IRON.getFluid(), 360), 1000)
                .unlockedBy("has_item", has(Items.IRON_BOOTS))
                .save(consumer, "casting:melting/iron/from_boots");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(Items.IRON_PICKAXE), 1),
                        new FluidStack(CastingFluids.MOLTEN_IRON.getFluid(), 270), 1000)
                .unlockedBy("has_item", has(Items.IRON_BOOTS))
                .save(consumer, "casting:melting/iron/from_pickaxe");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(Items.IRON_SWORD), 1),
                        new FluidStack(CastingFluids.MOLTEN_IRON.getFluid(), 180), 1000)
                .unlockedBy("has_item", has(Items.IRON_SWORD))
                .save(consumer, "casting:melting/iron/from_sword");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(Items.IRON_SHOVEL), 1),
                        new FluidStack(CastingFluids.MOLTEN_IRON.getFluid(), 90), 1000)
                .unlockedBy("has_item", has(Items.IRON_SHOVEL))
                .save(consumer, "casting:melting/iron/from_shovel");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(Items.IRON_AXE), 1),
                        new FluidStack(CastingFluids.MOLTEN_IRON.getFluid(), 270), 1000)
                .unlockedBy("has_item", has(Items.IRON_AXE))
                .save(consumer, "casting:melting/iron/from_axe");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(Items.IRON_HOE), 1),
                        new FluidStack(CastingFluids.MOLTEN_IRON.getFluid(), 180), 1000)
                .unlockedBy("has_item", has(Items.IRON_HOE))
                .save(consumer, "casting:melting/iron/from_hoe");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(Items.IRON_HORSE_ARMOR), 1),
                        new FluidStack(CastingFluids.MOLTEN_IRON.getFluid(), 450), 1000)
                .unlockedBy("has_item", has(Items.IRON_HORSE_ARMOR))
                .save(consumer, "casting:melting/iron/from_horse_armor");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(Items.IRON_DOOR), 1),
                        new FluidStack(CastingFluids.MOLTEN_IRON.getFluid(), 180), 1000)
                .unlockedBy("has_item", has(Items.IRON_DOOR))
                .save(consumer, "casting:melting/iron/from_door");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(Items.IRON_TRAPDOOR), 1),
                        new FluidStack(CastingFluids.MOLTEN_IRON.getFluid(), 360), 1000)
                .unlockedBy("has_item", has(Items.IRON_TRAPDOOR))
                .save(consumer, "casting:melting/iron/from_trapdoor");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(Items.IRON_BARS), 1),
                        new FluidStack(CastingFluids.MOLTEN_IRON.getFluid(), 30), 1000)
                .unlockedBy("has_item", has(Items.IRON_BARS))
                .save(consumer, "casting:melting/iron/from_bars");


        //Gold Processing

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(Tags.Items.NUGGETS_GOLD), 1),
                        new FluidStack(CastingFluids.MOLTEN_GOLD.getFluid(), 10), 1000)
                .unlockedBy("has_item", has(Tags.Items.INGOTS_GOLD))
                .save(consumer, "casting:melting/gold/from_nugget");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(Tags.Items.INGOTS_GOLD), 1),
                        new FluidStack(CastingFluids.MOLTEN_GOLD.getFluid(), 90), 1000)
                .unlockedBy("has_item", has(Tags.Items.INGOTS_GOLD))
                .save(consumer, "casting:melting/gold/from_ingot");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(goldDustTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_GOLD.getFluid(), 90), 1000)
                .unlockedBy("has_item", has(goldDustTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(goldDustTag))), "casting:melting/gold/from_dust");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(Tags.Items.STORAGE_BLOCKS_GOLD), 1),
                        new FluidStack(CastingFluids.MOLTEN_GOLD.getFluid(), 810), 1000)
                .unlockedBy("has_item", has(Tags.Items.ORES_GOLD))
                .save(consumer, "casting:melting/gold/from_block");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(Tags.Items.ORES_GOLD), 1),
                        new FluidStack(CastingFluids.MOLTEN_GOLD.getFluid(), 100), 1000)
                .unlockedBy("has_item", has(Tags.Items.ORES_GOLD))
                .save(consumer, "casting:melting/gold/from_ore");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(Tags.Items.STORAGE_BLOCKS_RAW_GOLD), 1),
                        new FluidStack(CastingFluids.MOLTEN_GOLD.getFluid(), 900), 1000)
                .unlockedBy("has_item", has(Tags.Items.ORES_GOLD))
                .save(consumer, "casting:melting/gold/from_raw_ore_block");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(Tags.Items.RAW_MATERIALS_GOLD), 1),
                        new FluidStack(CastingFluids.MOLTEN_GOLD.getFluid(), 100), 1000)
                .unlockedBy("has_item", has(Tags.Items.ORES_GOLD)).save(consumer, "casting:melting/gold/from_raw_ore");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.BLOCK_MOLD, 1), new SizedIngredient(Ingredient.of(Items.GOLD_BLOCK), 1),
                        new FluidStack(CastingFluids.MOLTEN_GOLD.getFluid(), 810))
                .unlockedBy("has_item", has(Items.GOLD_INGOT)).save(consumer, "casting:solidifier/gold/block");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.INGOT_MOLD, 1), new SizedIngredient(Ingredient.of(Items.GOLD_INGOT), 1),
                        new FluidStack(CastingFluids.MOLTEN_GOLD.getFluid(), 90))
                .unlockedBy("has_item", has(Items.GOLD_INGOT)).save(consumer, "casting:solidifier/gold/ingot");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.NUGGET_MOLD, 1), new SizedIngredient(Ingredient.of(Items.GOLD_NUGGET), 1),
                        new FluidStack(CastingFluids.MOLTEN_GOLD.getFluid(), 10))
                .unlockedBy("has_item", has(Items.GOLD_INGOT)).save(consumer, "casting:solidifier/gold/nugget");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.GEAR_MOLD, 1), new SizedIngredient(Ingredient.of(goldGearTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_GOLD.getFluid(), 360))
                .unlockedBy("has_item", has(Items.GOLD_INGOT)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(goldGearTag))), "casting:solidifier/gold/gear");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.ROD_MOLD, 1), new SizedIngredient(Ingredient.of(goldRodTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_GOLD.getFluid(), 90))
                .unlockedBy("has_item", has(Items.GOLD_INGOT)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(goldRodTag))), "casting:solidifier/gold/rod");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.PLATE_MOLD, 1), new SizedIngredient(Ingredient.of(goldPlateTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_GOLD.getFluid(), 90))
                .unlockedBy("has_item", has(Items.GOLD_INGOT)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(goldPlateTag))), "casting:solidifier/gold/plate");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.DUST_MOLD, 1), new SizedIngredient(Ingredient.of(goldDustTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_GOLD.getFluid(), 90))
                .unlockedBy("has_item", has(goldDustTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(goldDustTag))), "casting:solidifier/gold/dust");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(Items.GOLDEN_HELMET), 1),
                        new FluidStack(CastingFluids.MOLTEN_GOLD.getFluid(), 450), 1000)
                .unlockedBy("has_item", has(Items.GOLDEN_HELMET))
                .save(consumer, "casting:melting/gold/from_helmet");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(Items.GOLDEN_CHESTPLATE), 1),
                        new FluidStack(CastingFluids.MOLTEN_GOLD.getFluid(), 720), 1000)
                .unlockedBy("has_item", has(Items.GOLDEN_CHESTPLATE))
                .save(consumer, "casting:melting/gold/from_chestplate");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(Items.GOLDEN_LEGGINGS), 1),
                        new FluidStack(CastingFluids.MOLTEN_GOLD.getFluid(), 630), 1000)
                .unlockedBy("has_item", has(Items.GOLDEN_LEGGINGS))
                .save(consumer, "casting:melting/gold/from_leggings");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(Items.GOLDEN_BOOTS), 1),
                        new FluidStack(CastingFluids.MOLTEN_GOLD.getFluid(), 360), 1000)
                .unlockedBy("has_item", has(Items.GOLDEN_BOOTS))
                .save(consumer, "casting:melting/gold/from_boots");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(Items.GOLDEN_PICKAXE), 1),
                        new FluidStack(CastingFluids.MOLTEN_GOLD.getFluid(), 270), 1000)
                .unlockedBy("has_item", has(Items.GOLDEN_PICKAXE))
                .save(consumer, "casting:melting/gold/from_pickaxe");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(Items.GOLDEN_SWORD), 1),
                        new FluidStack(CastingFluids.MOLTEN_GOLD.getFluid(), 180), 1000)
                .unlockedBy("has_item", has(Items.GOLDEN_SWORD))
                .save(consumer, "casting:melting/gold/from_sword");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(Items.GOLDEN_SHOVEL), 1),
                        new FluidStack(CastingFluids.MOLTEN_GOLD.getFluid(), 90), 1000)
                .unlockedBy("has_item", has(Items.GOLDEN_SHOVEL))
                .save(consumer, "casting:melting/gold/from_shovel");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(Items.GOLDEN_AXE), 1),
                        new FluidStack(CastingFluids.MOLTEN_GOLD.getFluid(), 270), 1000)
                .unlockedBy("has_item", has(Items.GOLDEN_AXE))
                .save(consumer, "casting:melting/gold/from_axe");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(Items.GOLDEN_HOE), 1),
                        new FluidStack(CastingFluids.MOLTEN_GOLD.getFluid(), 180), 1000)
                .unlockedBy("has_item", has(Items.GOLDEN_HOE))
                .save(consumer, "casting:melting/gold/from_hoe");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(Items.GOLDEN_HORSE_ARMOR), 1),
                        new FluidStack(CastingFluids.MOLTEN_GOLD.getFluid(), 450), 1000)
                .unlockedBy("has_item", has(Items.GOLDEN_HORSE_ARMOR))
                .save(consumer, "casting:melting/gold/from_horse_armor");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(Items.GOLDEN_CARROT), 1),
                        new FluidStack(CastingFluids.MOLTEN_GOLD.getFluid(), 80), 1000)
                .unlockedBy("has_item", has(Items.GOLDEN_CARROT))
                .save(consumer, "casting:melting/gold/from_carrot");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(Items.GOLDEN_APPLE), 1),
                        new FluidStack(CastingFluids.MOLTEN_GOLD.getFluid(), 720), 1000)
                .unlockedBy("has_item", has(Items.GOLDEN_APPLE))
                .save(consumer, "casting:melting/gold/from_apple");


        //Copper Processing

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(copperNuggetTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_COPPER.getFluid(), 10), 1000)
                .unlockedBy("has_item", has(Tags.Items.INGOTS_COPPER))
                .save(consumer, "casting:melting/copper/from_nugget");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(Tags.Items.INGOTS_COPPER), 1),
                        new FluidStack(CastingFluids.MOLTEN_COPPER.getFluid(), 90), 1000)
                .unlockedBy("has_item", has(Tags.Items.INGOTS_COPPER))
                .save(consumer, "casting:melting/copper/from_ingot");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(copperDustTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_COPPER.getFluid(), 90), 1000)
                .unlockedBy("has_item", has(copperDustTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(copperDustTag))), "casting:melting/copper/from_dust");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(Tags.Items.STORAGE_BLOCKS_COPPER), 1),
                        new FluidStack(CastingFluids.MOLTEN_COPPER.getFluid(), 810), 1000)
                .unlockedBy("has_item", has(Tags.Items.ORES_COPPER))
                .save(consumer, "casting:melting/copper/from_block");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(Tags.Items.ORES_COPPER), 1),
                        new FluidStack(CastingFluids.MOLTEN_COPPER.getFluid(), 100), 1000)
                .unlockedBy("has_item", has(Tags.Items.ORES_COPPER))
                .save(consumer, "casting:melting/copper/from_ore");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(Tags.Items.STORAGE_BLOCKS_RAW_COPPER), 1),
                        new FluidStack(CastingFluids.MOLTEN_COPPER.getFluid(), 900), 1000)
                .unlockedBy("has_item", has(Tags.Items.ORES_COPPER))
                .save(consumer, "casting:melting/copper/from_raw_ore_block");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(Tags.Items.RAW_MATERIALS_COPPER), 1),
                        new FluidStack(CastingFluids.MOLTEN_COPPER.getFluid(), 100), 1000)
                .unlockedBy("has_item", has(Tags.Items.ORES_COPPER)).save(consumer, "casting:melting/copper/from_raw_ore");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.BLOCK_MOLD, 1), new SizedIngredient(Ingredient.of(Items.COPPER_BLOCK), 1),
                        new FluidStack(CastingFluids.MOLTEN_COPPER.getFluid(), 810))
                .unlockedBy("has_item", has(Items.COPPER_INGOT)).save(consumer, "casting:solidifier/copper/block");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.INGOT_MOLD, 1), new SizedIngredient(Ingredient.of(Items.COPPER_INGOT), 1),
                        new FluidStack(CastingFluids.MOLTEN_COPPER.getFluid(), 90))
                .unlockedBy("has_item", has(Items.COPPER_INGOT)).save(consumer, "casting:solidifier/copper/ingot");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.NUGGET_MOLD, 1), new SizedIngredient(Ingredient.of(com.benbenlaw.opolisutilities.item.ModItems.COPPER_NUGGET), 1),
                        new FluidStack(CastingFluids.MOLTEN_COPPER.getFluid(), 10))
                .unlockedBy("has_item", has(Items.COPPER_INGOT)).save(consumer, "casting:solidifier/copper/nugget");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.GEAR_MOLD, 1), new SizedIngredient(Ingredient.of(copperGearTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_COPPER.getFluid(), 360))
                .unlockedBy("has_item", has(Items.COPPER_INGOT)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(copperGearTag))), "casting:solidifier/copper/gear");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.ROD_MOLD, 1), new SizedIngredient(Ingredient.of(copperRodTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_COPPER.getFluid(), 90))
                .unlockedBy("has_item", has(Items.COPPER_INGOT)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(copperRodTag))), "casting:solidifier/copper/rod");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.PLATE_MOLD, 1), new SizedIngredient(Ingredient.of(copperPlateTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_COPPER.getFluid(), 90))
                .unlockedBy("has_item", has(Items.COPPER_INGOT)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(copperPlateTag))), "casting:solidifier/copper/plate");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.DUST_MOLD, 1), new SizedIngredient(Ingredient.of(copperDustTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_COPPER.getFluid(), 90))
                .unlockedBy("has_item", has(copperDustTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(copperDustTag))), "casting:solidifier/copper/dust");

        //Tin Processing

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(tinNuggetTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_TIN.getFluid(), 10), 1000)
                .unlockedBy("has_item", has(tinIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(tinNuggetTag))), "casting:melting/tin/from_nugget");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(tinIngotTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_TIN.getFluid(), 90), 1000)
                .unlockedBy("has_item", has(tinIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(tinIngotTag))), "casting:melting/tin/from_ingot");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(tinDustTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_TIN.getFluid(), 90), 1000)
                .unlockedBy("has_item", has(tinDustTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(tinDustTag))), "casting:melting/tin/from_dust");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(tinBlockTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_TIN.getFluid(), 810), 1000)
                .unlockedBy("has_item", has(tinBlockTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(tinBlockTag))), "casting:melting/tin/from_block");


        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(tinOreTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_TIN.getFluid(), 100), 1000)
                .unlockedBy("has_item", has(tinOreTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(tinOreTag))), "casting:melting/tin/from_ore");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(tinRawOreBlockTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_TIN.getFluid(), 900), 1000)
                .unlockedBy("has_item", has(tinRawOreTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(tinRawOreBlockTag))), "casting:melting/tin/from_raw_ore_block");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(tinRawOreTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_TIN.getFluid(), 100), 1000)
                .unlockedBy("has_item", has(tinRawOreTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(tinRawOreTag))), "casting:melting/tin/from_raw_ore");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.BLOCK_MOLD, 1), new SizedIngredient(Ingredient.of(tinBlockTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_TIN.getFluid(), 810))
                .unlockedBy("has_item", has(tinIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(tinBlockTag))), "casting:solidifier/tin/block");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.INGOT_MOLD, 1), new SizedIngredient(Ingredient.of(tinIngotTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_TIN.getFluid(), 90))
                .unlockedBy("has_item", has(tinIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(tinIngotTag))), "casting:solidifier/tin/ingot");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.NUGGET_MOLD, 1), new SizedIngredient(Ingredient.of(tinNuggetTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_TIN.getFluid(), 10))
                .unlockedBy("has_item", has(tinIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(tinNuggetTag))), "casting:solidifier/tin/nugget");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.GEAR_MOLD, 1), new SizedIngredient(Ingredient.of(tinGearTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_TIN.getFluid(), 360))
                .unlockedBy("has_item", has(tinIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(tinGearTag))), "casting:solidifier/tin/gear");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.ROD_MOLD, 1), new SizedIngredient(Ingredient.of(tinRodTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_TIN.getFluid(), 90))
                .unlockedBy("has_item", has(tinIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(tinIngotTag))), "casting:solidifier/tin/rod");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.PLATE_MOLD, 1), new SizedIngredient(Ingredient.of(tinPlateTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_TIN.getFluid(), 90))
                .unlockedBy("has_item", has(tinIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(tinPlateTag))), "casting:solidifier/tin/plate");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.DUST_MOLD, 1), new SizedIngredient(Ingredient.of(tinDustTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_TIN.getFluid(), 90))
                .unlockedBy("has_item", has(tinDustTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(tinDustTag))), "casting:solidifier/tin/dust");

        //Osmium Processing

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(osmiumNuggetTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_OSMIUM.getFluid(), 10), 1000)
                .unlockedBy("has_item", has(osmiumIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(osmiumNuggetTag))), "casting:melting/osmium/from_nugget");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(osmiumIngotTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_OSMIUM.getFluid(), 90), 1000)
                .unlockedBy("has_item", has(osmiumIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(osmiumIngotTag))), "casting:melting/osmium/from_ingot");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(osmiumDustTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_OSMIUM.getFluid(), 90), 1000)
                .unlockedBy("has_item", has(osmiumDustTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(osmiumDustTag))), "casting:melting/osmium/from_dust");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(osmiumOreTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_OSMIUM.getFluid(), 100), 1000)
                .unlockedBy("has_item", has(osmiumOreTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(osmiumOreTag))), "casting:melting/osmium/from_ore");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(osmiumRawOreBlockTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_OSMIUM.getFluid(), 900), 1000)
                .unlockedBy("has_item", has(osmiumRawOreTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(osmiumRawOreBlockTag))), "casting:melting/osmium/from_raw_ore_block");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(osmiumRawOreTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_OSMIUM.getFluid(), 100), 1000)
                .unlockedBy("has_item", has(osmiumRawOreTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(osmiumRawOreTag))), "casting:melting/osmium/from_raw_ore");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.BLOCK_MOLD, 1), new SizedIngredient(Ingredient.of(osmiumBlockTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_OSMIUM.getFluid(), 810))
                .unlockedBy("has_item", has(osmiumIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(osmiumBlockTag))), "casting:solidifier/osmium/block");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.INGOT_MOLD, 1), new SizedIngredient(Ingredient.of(osmiumIngotTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_OSMIUM.getFluid(), 90))
                .unlockedBy("has_item", has(osmiumIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(osmiumIngotTag))), "casting:solidifier/osmium/ingot");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.NUGGET_MOLD, 1), new SizedIngredient(Ingredient.of(osmiumNuggetTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_OSMIUM.getFluid(), 10))
                .unlockedBy("has_item", has(osmiumIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(osmiumNuggetTag))), "casting:solidifier/osmium/nugget");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.GEAR_MOLD, 1), new SizedIngredient(Ingredient.of(osmiumGearTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_OSMIUM.getFluid(), 360))
                .unlockedBy("has_item", has(osmiumIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(osmiumGearTag))), "casting:solidifier/osmium/gear");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.ROD_MOLD, 1), new SizedIngredient(Ingredient.of(osmiumRodTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_OSMIUM.getFluid(), 90))
                .unlockedBy("has_item", has(osmiumIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(osmiumIngotTag))), "casting:solidifier/osmium/rod");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.PLATE_MOLD, 1), new SizedIngredient(Ingredient.of(osmiumPlateTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_OSMIUM.getFluid(), 90))
                .unlockedBy("has_item", has(osmiumIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(osmiumPlateTag))), "casting:solidifier/osmium/plate");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.DUST_MOLD, 1), new SizedIngredient(Ingredient.of(osmiumDustTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_OSMIUM.getFluid(), 90))
                .unlockedBy("has_item", has(osmiumDustTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(osmiumDustTag))), "casting:solidifier/osmium/dust");

        //Uranium Processing

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(uraniumNuggetTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_URANIUM.getFluid(), 10), 1000)
                .unlockedBy("has_item", has(uraniumIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(uraniumNuggetTag))), "casting:melting/uranium/from_nugget");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(uraniumIngotTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_URANIUM.getFluid(), 90), 1000)
                .unlockedBy("has_item", has(uraniumIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(uraniumIngotTag))), "casting:melting/uranium/from_ingot");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(uraniumDustTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_URANIUM.getFluid(), 90), 1000)
                .unlockedBy("has_item", has(uraniumDustTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(uraniumDustTag))), "casting:melting/uranium/from_dust");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(uraniumOreTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_URANIUM.getFluid(), 100), 1000)
                .unlockedBy("has_item", has(uraniumOreTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(uraniumOreTag))), "casting:melting/uranium/from_ore");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(uraniumRawOreBlockTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_URANIUM.getFluid(), 900), 1000)
                .unlockedBy("has_item", has(uraniumRawOreTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(uraniumRawOreBlockTag))), "casting:melting/uranium/from_raw_ore_block");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(uraniumRawOreTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_URANIUM.getFluid(), 100), 1000)
                .unlockedBy("has_item", has(uraniumRawOreTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(uraniumRawOreTag))), "casting:melting/uranium/from_raw_ore");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.BLOCK_MOLD, 1), new SizedIngredient(Ingredient.of(uraniumBlockTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_URANIUM.getFluid(), 810))
                .unlockedBy("has_item", has(uraniumIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(uraniumBlockTag))), "casting:solidifier/uranium/block");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.INGOT_MOLD, 1), new SizedIngredient(Ingredient.of(uraniumIngotTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_URANIUM.getFluid(), 90))
                .unlockedBy("has_item", has(uraniumIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(uraniumIngotTag))), "casting:solidifier/uranium/ingot");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.NUGGET_MOLD, 1), new SizedIngredient(Ingredient.of(uraniumNuggetTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_URANIUM.getFluid(), 10))
                .unlockedBy("has_item", has(uraniumIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(uraniumNuggetTag))), "casting:solidifier/uranium/nugget");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.GEAR_MOLD, 1), new SizedIngredient(Ingredient.of(uraniumGearTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_URANIUM.getFluid(), 360))
                .unlockedBy("has_item", has(uraniumIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(uraniumGearTag))), "casting:solidifier/uranium/gear");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.ROD_MOLD, 1), new SizedIngredient(Ingredient.of(uraniumRodTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_URANIUM.getFluid(), 90))
                .unlockedBy("has_item", has(uraniumIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(uraniumIngotTag))), "casting:solidifier/uranium/rod");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.PLATE_MOLD, 1), new SizedIngredient(Ingredient.of(uraniumPlateTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_URANIUM.getFluid(), 90))
                .unlockedBy("has_item", has(uraniumIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(uraniumPlateTag))), "casting:solidifier/uranium/plate");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.DUST_MOLD, 1), new SizedIngredient(Ingredient.of(uraniumDustTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_URANIUM.getFluid(), 90))
                .unlockedBy("has_item", has(uraniumDustTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(uraniumDustTag))), "casting:solidifier/uranium/dust");

        //Lead Processing

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(leadNuggetTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_LEAD.getFluid(), 10), 1000)
                .unlockedBy("has_item", has(leadIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(leadNuggetTag))), "casting:melting/lead/from_nugget");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(leadIngotTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_LEAD.getFluid(), 90), 1000)
                .unlockedBy("has_item", has(leadIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(leadIngotTag))), "casting:melting/lead/from_ingot");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(leadDustTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_LEAD.getFluid(), 90), 1000)
                .unlockedBy("has_item", has(leadDustTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(leadDustTag))), "casting:melting/lead/from_dust");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(leadOreTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_LEAD.getFluid(), 100), 1000)
                .unlockedBy("has_item", has(leadOreTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(leadOreTag))), "casting:melting/lead/from_ore");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(leadRawOreBlockTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_LEAD.getFluid(), 900), 1000)
                .unlockedBy("has_item", has(leadRawOreTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(leadRawOreBlockTag))), "casting:melting/lead/from_raw_ore_block");


        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(leadRawOreTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_LEAD.getFluid(), 100), 1000)
                .unlockedBy("has_item", has(leadRawOreTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(leadRawOreTag))), "casting:melting/lead/from_raw_ore");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.BLOCK_MOLD, 1), new SizedIngredient(Ingredient.of(leadBlockTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_LEAD.getFluid(), 810))
                .unlockedBy("has_item", has(leadIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(leadBlockTag))), "casting:solidifier/lead/block");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.INGOT_MOLD, 1), new SizedIngredient(Ingredient.of(leadIngotTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_LEAD.getFluid(), 90))
                .unlockedBy("has_item", has(leadIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(leadIngotTag))), "casting:solidifier/lead/ingot");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.NUGGET_MOLD, 1), new SizedIngredient(Ingredient.of(leadNuggetTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_LEAD.getFluid(), 10))
                .unlockedBy("has_item", has(leadIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(leadNuggetTag))), "casting:solidifier/lead/nugget");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.GEAR_MOLD, 1), new SizedIngredient(Ingredient.of(leadGearTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_LEAD.getFluid(), 360))
                .unlockedBy("has_item", has(leadIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(leadGearTag))), "casting:solidifier/lead/gear");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.ROD_MOLD, 1), new SizedIngredient(Ingredient.of(leadRodTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_LEAD.getFluid(), 90))
                .unlockedBy("has_item", has(leadIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(leadIngotTag))), "casting:solidifier/lead/rod");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.PLATE_MOLD, 1), new SizedIngredient(Ingredient.of(leadPlateTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_LEAD.getFluid(), 90))
                .unlockedBy("has_item", has(leadIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(leadPlateTag))), "casting:solidifier/lead/plate");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.DUST_MOLD, 1), new SizedIngredient(Ingredient.of(leadDustTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_LEAD.getFluid(), 90))
                .unlockedBy("has_item", has(leadDustTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(leadDustTag))), "casting:solidifier/lead/dust");

        //Silver Processing

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(silverNuggetTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_SILVER.getFluid(), 10), 1000)
                .unlockedBy("has_item", has(silverIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(silverNuggetTag))), "casting:melting/silver/from_nugget");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(silverIngotTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_SILVER.getFluid(), 90), 1000)
                .unlockedBy("has_item", has(silverIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(silverIngotTag))), "casting:melting/silver/from_ingot");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(silverDustTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_SILVER.getFluid(), 90), 1000)
                .unlockedBy("has_item", has(silverDustTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(silverDustTag))), "casting:melting/silver/from_dust");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(silverOreTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_SILVER.getFluid(), 100), 1000)
                .unlockedBy("has_item", has(silverOreTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(silverOreTag))), "casting:melting/silver/from_ore");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(silverRawOreBlockTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_SILVER.getFluid(), 900), 1000)
                .unlockedBy("has_item", has(silverRawOreTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(silverRawOreBlockTag))), "casting:melting/silver/from_raw_ore_block");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(silverRawOreTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_SILVER.getFluid(), 100), 1000)
                .unlockedBy("has_item", has(silverRawOreTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(silverRawOreTag))), "casting:melting/silver/from_raw_ore");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.BLOCK_MOLD, 1), new SizedIngredient(Ingredient.of(silverBlockTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_SILVER.getFluid(), 810))
                .unlockedBy("has_item", has(silverIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(silverBlockTag))), "casting:solidifier/silver/block");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.INGOT_MOLD, 1), new SizedIngredient(Ingredient.of(silverIngotTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_SILVER.getFluid(), 90))
                .unlockedBy("has_item", has(silverIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(silverIngotTag))), "casting:solidifier/silver/ingot");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.NUGGET_MOLD, 1), new SizedIngredient(Ingredient.of(silverNuggetTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_SILVER.getFluid(), 10))
                .unlockedBy("has_item", has(silverIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(silverNuggetTag))), "casting:solidifier/silver/nugget");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.GEAR_MOLD, 1), new SizedIngredient(Ingredient.of(silverGearTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_SILVER.getFluid(), 360))
                .unlockedBy("has_item", has(silverIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(silverGearTag))), "casting:solidifier/silver/gear");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.ROD_MOLD, 1), new SizedIngredient(Ingredient.of(silverRodTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_SILVER.getFluid(), 90))
                .unlockedBy("has_item", has(silverIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(silverIngotTag))), "casting:solidifier/silver/rod");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.PLATE_MOLD, 1), new SizedIngredient(Ingredient.of(silverPlateTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_SILVER.getFluid(), 90))
                .unlockedBy("has_item", has(silverIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(silverPlateTag))), "casting:solidifier/silver/plate");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.DUST_MOLD, 1), new SizedIngredient(Ingredient.of(silverDustTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_SILVER.getFluid(), 90))
                .unlockedBy("has_item", has(silverDustTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(silverDustTag))), "casting:solidifier/silver/dust");

        //Coal Processing

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(com.benbenlaw.opolisutilities.item.ModItems.MINI_COAL), 1),
                        new FluidStack(CastingFluids.MOLTEN_COAL.getFluid(), 10), 1000)
                .unlockedBy("has_item", has(Items.COAL))
                .save(consumer, "casting:melting/coal/from_nugget");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(Items.COAL), 1),
                        new FluidStack(CastingFluids.MOLTEN_COAL.getFluid(), 80), 1000)
                .unlockedBy("has_item", has(Items.COAL))
                .save(consumer, "casting:melting/coal/from_item");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(coalDustTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_COAL.getFluid(), 80), 1000)
                .unlockedBy("has_item", has(coalDustTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(coalDustTag))), "casting:melting/coal/from_dust");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(Tags.Items.ORES_COAL), 1),
                        new FluidStack(CastingFluids.MOLTEN_COAL.getFluid(), 100), 1000)
                .unlockedBy("has_item", has(Tags.Items.ORES_COAL))
                .save(consumer, "casting:melting/coal/from_ore");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.BLOCK_MOLD, 1), new SizedIngredient(Ingredient.of(Items.COAL_BLOCK), 1),
                        new FluidStack(CastingFluids.MOLTEN_COAL.getFluid(), 720))
                .unlockedBy("has_item", has(Items.COAL)).save(consumer, "casting:solidifier/coal/block");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.GEM_MOLD, 1), new SizedIngredient(Ingredient.of(Items.COAL), 1),
                        new FluidStack(CastingFluids.MOLTEN_COAL.getFluid(), 80))
                .unlockedBy("has_item", has(Items.COAL)).save(consumer, "casting:solidifier/coal/coal");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.NUGGET_MOLD, 1), new SizedIngredient(Ingredient.of(com.benbenlaw.opolisutilities.item.ModItems.MINI_COAL), 1),
                        new FluidStack(CastingFluids.MOLTEN_COAL.getFluid(), 10))
                .unlockedBy("has_item", has(Items.COAL)).save(consumer, "casting:solidifier/coal/nugget");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.DUST_MOLD, 1), new SizedIngredient(Ingredient.of(coalDustTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_COAL.getFluid(), 80))
                .unlockedBy("has_item", has(coalDustTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(coalDustTag))), "casting:solidifier/coal/dust");


        // Emerald Processing

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(emeraldNuggetTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_EMERALD.getFluid(), 10), 1000)
                .unlockedBy("has_item", has(Tags.Items.GEMS_EMERALD))
                .save(consumer.withConditions(new NotCondition(new TagEmptyCondition(emeraldNuggetTag))), "casting:melting/emerald/from_nugget");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(emeraldDustTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_EMERALD.getFluid(), 90), 1000)
                .unlockedBy("has_item", has(emeraldDustTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(emeraldDustTag))), "casting:melting/emerald/from_dust");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(Tags.Items.GEMS_EMERALD), 1),
                        new FluidStack(CastingFluids.MOLTEN_EMERALD.getFluid(), 90), 1000)
                .unlockedBy("has_item", has(Tags.Items.GEMS_EMERALD))
                .save(consumer, "casting:melting/emerald/from_gem");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(Tags.Items.ORES_EMERALD), 1),
                        new FluidStack(CastingFluids.MOLTEN_EMERALD.getFluid(), 100), 1000)
                .unlockedBy("has_item", has(Tags.Items.ORES_EMERALD))
                .save(consumer, "casting:melting/emerald/from_ore");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.BLOCK_MOLD, 1), new SizedIngredient(Ingredient.of(Items.EMERALD_BLOCK), 1),
                        new FluidStack(CastingFluids.MOLTEN_EMERALD.getFluid(), 810))
                .unlockedBy("has_item", has(Items.EMERALD)).save(consumer, "casting:solidifier/emerald/block");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.GEM_MOLD, 1), new SizedIngredient(Ingredient.of(Items.EMERALD), 1),
                        new FluidStack(CastingFluids.MOLTEN_EMERALD.getFluid(), 90))
                .unlockedBy("has_item", has(Items.EMERALD)).save(consumer, "casting:solidifier/emerald/emerald");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.GEAR_MOLD, 1), new SizedIngredient(Ingredient.of(emeraldGearTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_EMERALD.getFluid(), 360))
                .unlockedBy("has_item", has(emeraldGearTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(emeraldGearTag))), "casting:solidifier/emerald/gear");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.ROD_MOLD, 1), new SizedIngredient(Ingredient.of(Items.EMERALD), 1),
                        new FluidStack(CastingFluids.MOLTEN_EMERALD.getFluid(), 90))
                .unlockedBy("has_item", has(Items.EMERALD)).save(consumer, "casting:solidifier/emerald/rod");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.PLATE_MOLD, 1), new SizedIngredient(Ingredient.of(emeraldPlateTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_EMERALD.getFluid(), 90))
                .unlockedBy("has_item", has(emeraldPlateTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(emeraldPlateTag))), "casting:solidifier/emerald/plate");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.DUST_MOLD, 1), new SizedIngredient(Ingredient.of(emeraldDustTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_EMERALD.getFluid(), 90))
                .unlockedBy("has_item", has(emeraldDustTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(emeraldDustTag))), "casting:solidifier/emerald/dust");

        //Diamond Processing

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(diamondNuggetTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_DIAMOND.getFluid(), 10), 1000)
                .unlockedBy("has_item", has(Tags.Items.GEMS_DIAMOND))
                .save(consumer.withConditions(new NotCondition(new TagEmptyCondition(diamondNuggetTag))), "casting:melting/diamond/from_nugget");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(Tags.Items.GEMS_DIAMOND), 1),
                        new FluidStack(CastingFluids.MOLTEN_DIAMOND.getFluid(), 90), 1000)
                .unlockedBy("has_item", has(Tags.Items.GEMS_DIAMOND))
                .save(consumer, "casting:melting/diamond/from_gem");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(diamondDustTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_DIAMOND.getFluid(), 90), 1000)
                .unlockedBy("has_item", has(diamondDustTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(diamondDustTag))), "casting:melting/diamond/from_dust");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(Tags.Items.ORES_DIAMOND), 1),
                        new FluidStack(CastingFluids.MOLTEN_DIAMOND.getFluid(), 100), 1000)
                .unlockedBy("has_item", has(Tags.Items.ORES_DIAMOND))
                .save(consumer, "casting:melting/diamond/from_ore");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.BLOCK_MOLD, 1), new SizedIngredient(Ingredient.of(Items.DIAMOND_BLOCK), 1),
                        new FluidStack(CastingFluids.MOLTEN_DIAMOND.getFluid(), 810))
                .unlockedBy("has_item", has(Items.DIAMOND)).save(consumer, "casting:solidifier/diamond/block");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.GEM_MOLD, 1), new SizedIngredient(Ingredient.of(Items.DIAMOND), 1),
                        new FluidStack(CastingFluids.MOLTEN_DIAMOND.getFluid(), 90))
                .unlockedBy("has_item", has(Items.DIAMOND)).save(consumer, "casting:solidifier/diamond/diamond");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.GEAR_MOLD, 1), new SizedIngredient(Ingredient.of(diamondGearTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_DIAMOND.getFluid(), 360))
                .unlockedBy("has_item", has(diamondGearTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(diamondGearTag))), "casting:solidifier/diamond/gear");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.ROD_MOLD, 1), new SizedIngredient(Ingredient.of(Items.DIAMOND), 1),
                        new FluidStack(CastingFluids.MOLTEN_DIAMOND.getFluid(), 90))
                .unlockedBy("has_item", has(Items.DIAMOND)).save(consumer, "casting:solidifier/diamond/rod");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.PLATE_MOLD, 1), new SizedIngredient(Ingredient.of(diamondPlateTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_DIAMOND.getFluid(), 90))
                .unlockedBy("has_item", has(diamondPlateTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(diamondPlateTag))), "casting:solidifier/diamond/plate");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.DUST_MOLD, 1), new SizedIngredient(Ingredient.of(diamondDustTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_DIAMOND.getFluid(), 90))
                .unlockedBy("has_item", has(diamondDustTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(diamondDustTag))), "casting:solidifier/diamond/dust");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(Items.DIAMOND_HELMET), 1),
                        new FluidStack(CastingFluids.MOLTEN_DIAMOND.getFluid(), 450), 1000)
                .unlockedBy("has_item", has(Items.DIAMOND_HELMET))
                .save(consumer, "casting:melting/diamond/from_helmet");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(Items.DIAMOND_CHESTPLATE), 1),
                        new FluidStack(CastingFluids.MOLTEN_DIAMOND.getFluid(), 720), 1000)
                .unlockedBy("has_item", has(Items.DIAMOND_CHESTPLATE))
                .save(consumer, "casting:melting/diamond/from_chestplate");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(Items.DIAMOND_LEGGINGS), 1),
                        new FluidStack(CastingFluids.MOLTEN_DIAMOND.getFluid(), 630), 1000)
                .unlockedBy("has_item", has(Items.DIAMOND_LEGGINGS))
                .save(consumer, "casting:melting/diamond/from_leggings");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(Items.DIAMOND_BOOTS), 1),
                        new FluidStack(CastingFluids.MOLTEN_DIAMOND.getFluid(), 360), 1000)
                .unlockedBy("has_item", has(Items.DIAMOND_BOOTS))
                .save(consumer, "casting:melting/diamond/from_boots");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(Items.DIAMOND_SWORD), 1),
                        new FluidStack(CastingFluids.MOLTEN_DIAMOND.getFluid(), 180), 1000)
                .unlockedBy("has_item", has(Items.DIAMOND_SWORD))
                .save(consumer, "casting:melting/diamond/from_sword");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(Items.DIAMOND_PICKAXE), 1),
                        new FluidStack(CastingFluids.MOLTEN_DIAMOND.getFluid(), 270), 1000)
                .unlockedBy("has_item", has(Items.DIAMOND_PICKAXE))
                .save(consumer, "casting:melting/diamond/from_pickaxe");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(Items.DIAMOND_AXE), 1),
                        new FluidStack(CastingFluids.MOLTEN_DIAMOND.getFluid(), 270), 1000)
                .unlockedBy("has_item", has(Items.DIAMOND_AXE))
                .save(consumer, "casting:melting/diamond/from_axe");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(Items.DIAMOND_SHOVEL), 1),
                        new FluidStack(CastingFluids.MOLTEN_DIAMOND.getFluid(), 90), 1000)
                .unlockedBy("has_item", has(Items.DIAMOND_SHOVEL))
                .save(consumer, "casting:melting/diamond/from_shovel");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(Items.DIAMOND_HOE), 1),
                        new FluidStack(CastingFluids.MOLTEN_DIAMOND.getFluid(), 180), 1000)
                .unlockedBy("has_item", has(Items.DIAMOND_HOE))
                .save(consumer, "casting:melting/diamond/from_hoe");


        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(Items.DIAMOND_HORSE_ARMOR), 1),
                        new FluidStack(CastingFluids.MOLTEN_DIAMOND.getFluid(), 450), 1000)
                .unlockedBy("has_item", has(Items.DIAMOND_HORSE_ARMOR))
                .save(consumer, "casting:melting/diamond/from_horse_armor");

        //Redstone Processing

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(Tags.Items.DUSTS_REDSTONE), 1),
                        new FluidStack(CastingFluids.MOLTEN_REDSTONE.getFluid(), 90), 1000)
                .unlockedBy("has_item", has(Tags.Items.DUSTS_REDSTONE))
                .save(consumer, "casting:melting/redstone/from_dust");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(Tags.Items.ORES_REDSTONE), 1),
                        new FluidStack(CastingFluids.MOLTEN_REDSTONE.getFluid(), 100), 1000)
                .unlockedBy("has_item", has(Tags.Items.ORES_REDSTONE))
                .save(consumer, "casting:melting/redstone/from_ore");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.BLOCK_MOLD, 1), new SizedIngredient(Ingredient.of(Items.REDSTONE_BLOCK), 1),
                        new FluidStack(CastingFluids.MOLTEN_REDSTONE.getFluid(), 810))
                .unlockedBy("has_item", has(Items.REDSTONE)).save(consumer, "casting:solidifier/redstone/block");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.DUST_MOLD, 1), new SizedIngredient(Ingredient.of(Items.REDSTONE), 1),
                        new FluidStack(CastingFluids.MOLTEN_REDSTONE.getFluid(), 90))
                .unlockedBy("has_item", has(Items.REDSTONE)).save(consumer, "casting:solidifier/redstone/redstone");


        //Lapis Processing

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(Tags.Items.GEMS_LAPIS), 1),
                        new FluidStack(CastingFluids.MOLTEN_LAPIS.getFluid(), 90), 1000)
                .unlockedBy("has_item", has(Tags.Items.GEMS_LAPIS))
                .save(consumer, "casting:melting/lapis/from_gem");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(lapisDustTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_LAPIS.getFluid(), 90), 1000)
                .unlockedBy("has_item", has(lapisDustTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(lapisDustTag))), "casting:melting/lapis/from_dust");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(Tags.Items.ORES_LAPIS), 1),
                        new FluidStack(CastingFluids.MOLTEN_LAPIS.getFluid(), 100), 1000)
                .unlockedBy("has_item", has(Tags.Items.ORES_LAPIS))
                .save(consumer, "casting:melting/lapis/from_ore");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.BLOCK_MOLD, 1), new SizedIngredient(Ingredient.of(Items.LAPIS_BLOCK), 1),
                        new FluidStack(CastingFluids.MOLTEN_LAPIS.getFluid(), 810))
                .unlockedBy("has_item", has(Items.LAPIS_LAZULI)).save(consumer, "casting:solidifier/lapis/block");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.GEM_MOLD, 1), new SizedIngredient(Ingredient.of(Items.LAPIS_LAZULI), 1),
                        new FluidStack(CastingFluids.MOLTEN_LAPIS.getFluid(), 90))
                .unlockedBy("has_item", has(Items.LAPIS_LAZULI)).save(consumer, "casting:solidifier/lapis/lapis");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.GEAR_MOLD, 1), new SizedIngredient(Ingredient.of(lapisGearTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_LAPIS.getFluid(), 360))
                .unlockedBy("has_item", has(lapisGearTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(lapisGearTag))), "casting:solidifier/lapis/gear");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.ROD_MOLD, 1), new SizedIngredient(Ingredient.of(lapisRodTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_LAPIS.getFluid(), 90))
                .unlockedBy("has_item", has(lapisRodTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(lapisRodTag))), "casting:solidifier/lapis/rod");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.PLATE_MOLD, 1), new SizedIngredient(Ingredient.of(lapisPlateTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_LAPIS.getFluid(), 90))
                .unlockedBy("has_item", has(lapisPlateTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(lapisPlateTag))), "casting:solidifier/lapis/plate");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.DUST_MOLD, 1), new SizedIngredient(Ingredient.of(lapisDustTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_LAPIS.getFluid(), 90))
                .unlockedBy("has_item", has(lapisDustTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(lapisDustTag))), "casting:solidifier/lapis/dust");

        //Obsidian Processing

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(Tags.Items.OBSIDIANS), 1),
                        new FluidStack(CastingFluids.MOLTEN_OBSIDIAN.getFluid(), 1000), 1200)
                .unlockedBy("has_item", has(Tags.Items.ORES_LAPIS))
                .save(consumer, "casting:melting/obsidian/from_block");

        MixingRecipeBuilder.MixingRecipesBuilder(
                        new FluidStack(Fluids.WATER, 1000),
                        new FluidStack(Fluids.LAVA, 1000),
                        null,
                        null,
                        null,
                        null,
                        new FluidStack(CastingFluids.MOLTEN_OBSIDIAN.getFluid(), 1000))
                .unlockedBy("has_item", has(Items.WATER_BUCKET))
                .save(consumer, "casting:mixing/obsidian");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.BLOCK_MOLD, 1), new SizedIngredient(Ingredient.of(Items.OBSIDIAN), 1),
                        new FluidStack(CastingFluids.MOLTEN_OBSIDIAN.getFluid(), 1000))
                .unlockedBy("has_item", has(Items.OBSIDIAN)).save(consumer, "casting:solidifier/obsidian/block");

        //Bronze Processing

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(bronzeBlockTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_BRONZE.getFluid(), 810), 1000)
                .unlockedBy("has_item", has(bronzeBlockTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(bronzeBlockTag))), "casting:melting/bronze/from_block");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(bronzeIngotTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_BRONZE.getFluid(), 90), 1000)
                .unlockedBy("has_item", has(bronzeIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(bronzeIngotTag))), "casting:melting/bronze/from_ingot");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(bronzeDustTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_BRONZE.getFluid(), 90), 1000)
                .unlockedBy("has_item", has(bronzeDustTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(bronzeDustTag))), "casting:melting/bronze/from_dust");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(bronzeNuggetTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_BRONZE.getFluid(), 10), 1000)
                .unlockedBy("has_item", has(bronzeNuggetTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(bronzeNuggetTag))), "casting:melting/bronze/from_nugget");

        MixingRecipeBuilder.MixingRecipesBuilder(
                        new FluidStack(CastingFluids.MOLTEN_COPPER.getFluid(), 270),
                        new FluidStack(CastingFluids.MOLTEN_TIN.getFluid(), 90),
                        null,
                        null,
                        null,
                        null,
                        new FluidStack(CastingFluids.MOLTEN_BRONZE.getFluid(), 360))
                .unlockedBy("has_item", has(bronzeIngotTag))
                .save(consumer.withConditions(new NotCondition(new TagEmptyCondition(bronzeIngotTag))), "casting:mixing/bronze");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.BLOCK_MOLD, 1), new SizedIngredient(Ingredient.of(bronzeBlockTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_BRONZE.getFluid(), 810))
                .unlockedBy("has_item", has(bronzeBlockTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(bronzeBlockTag))), "casting:solidifier/bronze/block");


        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.INGOT_MOLD, 1), new SizedIngredient(Ingredient.of(bronzeIngotTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_BRONZE.getFluid(), 90))
                .unlockedBy("has_item", has(bronzeIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(bronzeIngotTag))), "casting:solidifier/bronze/ingot");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.NUGGET_MOLD, 1), new SizedIngredient(Ingredient.of(bronzeNuggetTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_BRONZE.getFluid(), 10))
                .unlockedBy("has_item", has(bronzeIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(bronzeNuggetTag))), "casting:solidifier/bronze/nugget");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.GEAR_MOLD, 1), new SizedIngredient(Ingredient.of(bronzeGearTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_BRONZE.getFluid(), 360))
                .unlockedBy("has_item", has(bronzeIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(bronzeGearTag))), "casting:solidifier/bronze/gear");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.ROD_MOLD, 1), new SizedIngredient(Ingredient.of(bronzeRodTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_BRONZE.getFluid(), 90))
                .unlockedBy("has_item", has(bronzeIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(bronzeRodTag))), "casting:solidifier/bronze/rod");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.PLATE_MOLD, 1), new SizedIngredient(Ingredient.of(bronzePlateTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_BRONZE.getFluid(), 90))
                .unlockedBy("has_item", has(bronzeIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(bronzePlateTag))), "casting:solidifier/bronze/plate");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.DUST_MOLD, 1), new SizedIngredient(Ingredient.of(bronzeDustTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_BRONZE.getFluid(), 90))
                .unlockedBy("has_item", has(bronzeDustTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(bronzeDustTag))), "casting:solidifier/bronze/dust");

        //Steel Processing

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(steelBlockTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_STEEL.getFluid(), 810), 1000)
                .unlockedBy("has_item", has(steelBlockTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(steelBlockTag))), "casting:melting/steel/from_block");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(steelIngotTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_STEEL.getFluid(), 90), 1000)
                .unlockedBy("has_item", has(steelIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(steelIngotTag))), "casting:melting/steel/from_ingot");


        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(steelDustTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_STEEL.getFluid(), 90), 1000)
                .unlockedBy("has_item", has(steelDustTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(steelDustTag))), "casting:melting/steel/from_dust");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(steelNuggetTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_STEEL.getFluid(), 10), 1000)
                .unlockedBy("has_item", has(steelNuggetTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(steelNuggetTag))), "casting:melting/steel/from_nugget");

        MixingRecipeBuilder.MixingRecipesBuilder(
                        new FluidStack(CastingFluids.MOLTEN_COAL.getFluid(), 160),
                        new FluidStack(CastingFluids.MOLTEN_IRON.getFluid(), 90),
                        null,
                        null,
                        null,
                        null,
                        new FluidStack(CastingFluids.MOLTEN_STEEL.getFluid(), 90))
                .unlockedBy("has_item", has(steelIngotTag))
                .save(consumer.withConditions(new NotCondition(new TagEmptyCondition(steelIngotTag))), "casting:mixing/steel");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.BLOCK_MOLD, 1), new SizedIngredient(Ingredient.of(steelBlockTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_STEEL.getFluid(), 810))
                .unlockedBy("has_item", has(steelBlockTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(steelBlockTag))), "casting:solidifier/steel/block");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.INGOT_MOLD, 1), new SizedIngredient(Ingredient.of(steelIngotTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_STEEL.getFluid(), 90))
                .unlockedBy("has_item", has(steelIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(steelIngotTag))), "casting:solidifier/steel/ingot");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.NUGGET_MOLD, 1), new SizedIngredient(Ingredient.of(steelNuggetTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_STEEL.getFluid(), 10))
                .unlockedBy("has_item", has(steelIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(steelNuggetTag))), "casting:solidifier/steel/nugget");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.GEAR_MOLD, 1), new SizedIngredient(Ingredient.of(steelGearTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_STEEL.getFluid(), 360))
                .unlockedBy("has_item", has(steelIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(steelGearTag))), "casting:solidifier/steel/gear");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.ROD_MOLD, 1), new SizedIngredient(Ingredient.of(steelRodTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_STEEL.getFluid(), 90))
                .unlockedBy("has_item", has(steelIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(steelRodTag))), "casting:solidifier/steel/rod");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.PLATE_MOLD, 1), new SizedIngredient(Ingredient.of(steelPlateTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_STEEL.getFluid(), 90))
                .unlockedBy("has_item", has(steelIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(steelPlateTag))), "casting:solidifier/steel/plate");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.DUST_MOLD, 1), new SizedIngredient(Ingredient.of(steelDustTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_STEEL.getFluid(), 90))
                .unlockedBy("has_item", has(steelDustTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(steelDustTag))), "casting:solidifier/steel/dust");

        //Netherite Processing

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(Blocks.NETHERITE_BLOCK), 1),
                        new FluidStack(CastingFluids.MOLTEN_NETHERITE.getFluid(), 810), 1250)
                .unlockedBy("has_item", has(Blocks.NETHERITE_BLOCK)).save(consumer, "casting:melting/netherite/from_block");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(Tags.Items.INGOTS_NETHERITE), 1),
                        new FluidStack(CastingFluids.MOLTEN_NETHERITE.getFluid(), 90), 1250)
                .unlockedBy("has_item", has(Tags.Items.INGOTS_NETHERITE)).save(consumer, "casting:melting/netherite/from_ingot");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(netheriteDustTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_NETHERITE.getFluid(), 90), 1250)
                .unlockedBy("has_item", has(netheriteDustTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(netheriteDustTag))), "casting:melting/netherite/from_dust");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(netheriteNuggetTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_NETHERITE.getFluid(), 10), 1250)
                .unlockedBy("has_item", has(netheriteNuggetTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(netheriteNuggetTag))), "casting:melting/netherite/from_nugget");

        MixingRecipeBuilder.MixingRecipesBuilder(
                        new FluidStack(CastingFluids.MOLTEN_GOLD.getFluid(), 360),
                        new FluidStack(CastingFluids.MOLTEN_DEBRIS.getFluid(), 360),
                        null,
                        null,
                        null,
                        null,
                        new FluidStack(CastingFluids.MOLTEN_NETHERITE.getFluid(), 90))
                .unlockedBy("has_item", has(steelIngotTag))
                .save(consumer, "casting:mixing/netherite");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.BLOCK_MOLD, 1), new SizedIngredient(Ingredient.of(Blocks.NETHERITE_BLOCK), 1),
                        new FluidStack(CastingFluids.MOLTEN_NETHERITE.getFluid(), 810))
                .unlockedBy("has_item", has(Blocks.NETHERITE_BLOCK)).save(consumer, "casting:solidifier/netherite/block");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.INGOT_MOLD, 1), new SizedIngredient(Ingredient.of(Tags.Items.INGOTS_NETHERITE), 1),
                        new FluidStack(CastingFluids.MOLTEN_NETHERITE.getFluid(), 90))
                .unlockedBy("has_item", has(Tags.Items.INGOTS_NETHERITE)).save(consumer, "casting:solidifier/netherite/ingot");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.NUGGET_MOLD, 1), new SizedIngredient(Ingredient.of(netheriteNuggetTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_NETHERITE.getFluid(), 10))
                .unlockedBy("has_item", has(netheriteNuggetTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(netheriteNuggetTag))), "casting:solidifier/netherite/nugget");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.GEAR_MOLD, 1), new SizedIngredient(Ingredient.of(netheriteGearTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_NETHERITE.getFluid(), 360))
                .unlockedBy("has_item", has(netheriteGearTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(netheriteGearTag))), "casting:solidifier/netherite/gear");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.ROD_MOLD, 1), new SizedIngredient(Ingredient.of(netheriteRodTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_NETHERITE.getFluid(), 90))
                .unlockedBy("has_item", has(netheriteRodTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(netheriteRodTag))), "casting:solidifier/netherite/rod");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.PLATE_MOLD, 1), new SizedIngredient(Ingredient.of(netheritePlateTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_NETHERITE.getFluid(), 90))
                .unlockedBy("has_item", has(netheritePlateTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(netheritePlateTag))), "casting:solidifier/netherite/plate");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.DUST_MOLD, 1), new SizedIngredient(Ingredient.of(netheriteDustTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_NETHERITE.getFluid(), 90))
                .unlockedBy("has_item", has(netheriteDustTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(netheriteDustTag))), "casting:solidifier/netherite/dust");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(Items.NETHERITE_HELMET), 1),
                        new FluidStack(CastingFluids.MOLTEN_NETHERITE.getFluid(), 50), 1000)
                .unlockedBy("has_item", has(Items.NETHERITE_HELMET))
                .save(consumer, "casting:melting/netherite/from_netherite_helmet");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(Items.NETHERITE_CHESTPLATE), 1),
                        new FluidStack(CastingFluids.MOLTEN_NETHERITE.getFluid(), 80), 1000)
                .unlockedBy("has_item", has(Items.NETHERITE_CHESTPLATE))
                .save(consumer, "casting:melting/netherite/from_netherite_chestplate");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(Items.NETHERITE_LEGGINGS), 1),
                        new FluidStack(CastingFluids.MOLTEN_NETHERITE.getFluid(), 70), 1000)
                .unlockedBy("has_item", has(Items.NETHERITE_LEGGINGS))
                .save(consumer, "casting:melting/netherite/from_netherite_leggings");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(Items.NETHERITE_BOOTS), 1),
                        new FluidStack(CastingFluids.MOLTEN_NETHERITE.getFluid(), 40), 1000)
                .unlockedBy("has_item", has(Items.NETHERITE_BOOTS))
                .save(consumer, "casting:melting/netherite/from_netherite_boots");


        //Debris Processing

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(Blocks.ANCIENT_DEBRIS), 1),
                        new FluidStack(CastingFluids.MOLTEN_DEBRIS.getFluid(), 100), 1250)
                .unlockedBy("has_item", has(Blocks.ANCIENT_DEBRIS)).save(consumer, "casting:melting/debris/from_ore");


        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(Items.NETHERITE_SCRAP), 1),
                        new FluidStack(CastingFluids.MOLTEN_DEBRIS.getFluid(), 90), 1250)
                .unlockedBy("has_item", has(Blocks.ANCIENT_DEBRIS)).save(consumer, "casting:melting/debris/from_scrap");

        //Quartz Processing

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(Tags.Items.GEMS_QUARTZ), 1),
                        new FluidStack(CastingFluids.MOLTEN_QUARTZ.getFluid(), 90), 1000)
                .unlockedBy("has_item", has(Tags.Items.GEMS_QUARTZ))
                .save(consumer, "casting:melting/quartz/from_gem");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(quartzDustTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_QUARTZ.getFluid(), 90), 1000)
                .unlockedBy("has_item", has(quartzDustTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(quartzDustTag))), "casting:melting/quartz/from_dust");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(Tags.Items.ORES_QUARTZ), 1),
                        new FluidStack(CastingFluids.MOLTEN_QUARTZ.getFluid(), 100), 1000)
                .unlockedBy("has_item", has(Tags.Items.ORES_QUARTZ))
                .save(consumer, "casting:melting/quartz/from_ore");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(Items.QUARTZ_BLOCK), 1),
                        new FluidStack(CastingFluids.MOLTEN_QUARTZ.getFluid(), 360), 1000)
                .unlockedBy("has_item", has(Tags.Items.ORES_QUARTZ))
                .save(consumer, "casting:melting/quartz/from_block");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.BLOCK_MOLD, 1), new SizedIngredient(Ingredient.of(Items.QUARTZ_BLOCK), 1),
                        new FluidStack(CastingFluids.MOLTEN_QUARTZ.getFluid(), 360))
                .unlockedBy("has_item", has(Items.QUARTZ)).save(consumer, "casting:solidifier/quartz/block");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.GEM_MOLD, 1), new SizedIngredient(Ingredient.of(Items.QUARTZ), 1),
                        new FluidStack(CastingFluids.MOLTEN_QUARTZ.getFluid(), 90))
                .unlockedBy("has_item", has(Items.QUARTZ)).save(consumer, "casting:solidifier/quartz/quartz");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.DUST_MOLD, 1), new SizedIngredient(Ingredient.of(quartzDustTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_QUARTZ.getFluid(), 90))
                .unlockedBy("has_item", has(quartzDustTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(quartzDustTag))), "casting:solidifier/quartz/dust");

        //Glass Processing

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(Items.GLASS), 1),
                        new FluidStack(CastingFluids.MOLTEN_GLASS.getFluid(), 1000), 250)
                .unlockedBy("has_item", has(Items.GLASS))
                .save(consumer, "casting:melting/glass/from_item");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(Tags.Items.SANDS), 1),
                        new FluidStack(CastingFluids.MOLTEN_GLASS.getFluid(), 1000), 250)
                .unlockedBy("has_item", has(Tags.Items.SANDS))
                .save(consumer, "casting:melting/glass/from_sand");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.BLOCK_MOLD, 1), new SizedIngredient(Ingredient.of(Items.GLASS), 1),
                        new FluidStack(CastingFluids.MOLTEN_GLASS.getFluid(), 1000))
                .unlockedBy("has_item", has(Items.GLASS)).save(consumer, "casting:solidifier/glass/block");

        //Stone Processing

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(Tags.Items.STONES), 1),
                        new FluidStack(CastingFluids.MOLTEN_STONE.getFluid(), 1000), 900)
                .unlockedBy("has_item", has(Tags.Items.STONES))
                .save(consumer, "casting:melting/stone/from_stone");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(Tags.Items.COBBLESTONES), 1),
                        new FluidStack(CastingFluids.MOLTEN_STONE.getFluid(), 1000), 900)
                .unlockedBy("has_item", has(Tags.Items.COBBLESTONES))
                .save(consumer, "casting:melting/stone/from_cobblestone");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.BLOCK_MOLD, 1), new SizedIngredient(Ingredient.of(Items.STONE), 1),
                        new FluidStack(CastingFluids.MOLTEN_STONE.getFluid(), 1000))
                .unlockedBy("has_item", has(Tags.Items.STONES)).save(consumer, "casting:solidifier/stone/block");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.GEAR_MOLD, 1), new SizedIngredient(Ingredient.of(stoneGearTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_STONE.getFluid(), 4000))
                .unlockedBy("has_item", has(stoneGearTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(stoneGearTag))), "casting:solidifier/stone/gear");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.ROD_MOLD, 1), new SizedIngredient(Ingredient.of(stoneRodTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_STONE.getFluid(), 90))
                .unlockedBy("has_item", has(stoneRodTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(stoneRodTag))), "casting:solidifier/stone/rod");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.PLATE_MOLD, 1), new SizedIngredient(Ingredient.of(stonePlateTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_STONE.getFluid(), 90))
                .unlockedBy("has_item", has(Tags.Items.STONES)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(stonePlateTag))), "casting:solidifier/stone/plate");

        //Electrum Processing

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(electrumBlockTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_ELECTRUM.getFluid(), 810), 1000)
                .unlockedBy("has_item", has(electrumBlockTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(electrumBlockTag))), "casting:melting/electrum/from_block");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(electrumIngotTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_ELECTRUM.getFluid(), 90), 1000)
                .unlockedBy("has_item", has(electrumIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(electrumIngotTag))), "casting:melting/electrum/from_ingot");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(electrumDustTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_ELECTRUM.getFluid(), 90), 1000)
                .unlockedBy("has_item", has(electrumDustTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(electrumDustTag))), "casting:melting/electrum/from_dust");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(electrumNuggetTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_ELECTRUM.getFluid(), 10), 1000)
                .unlockedBy("has_item", has(electrumNuggetTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(electrumNuggetTag))), "casting:melting/electrum/from_nugget");

        MixingRecipeBuilder.MixingRecipesBuilder(
                        new FluidStack(CastingFluids.MOLTEN_GOLD.getFluid(), 90),
                        new FluidStack(CastingFluids.MOLTEN_SILVER.getFluid(), 90),
                        null,
                        null,
                        null,
                        null,
                        new FluidStack(CastingFluids.MOLTEN_ELECTRUM.getFluid(), 180))
                .unlockedBy("has_item", has(electrumIngotTag))
                .save(consumer.withConditions(new NotCondition(new TagEmptyCondition(electrumIngotTag))), "casting:mixing/electrum");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.BLOCK_MOLD, 1), new SizedIngredient(Ingredient.of(electrumBlockTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_ELECTRUM.getFluid(), 810))
                .unlockedBy("has_item", has(electrumBlockTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(electrumBlockTag))), "casting:solidifier/electrum/block");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.INGOT_MOLD, 1), new SizedIngredient(Ingredient.of(electrumIngotTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_ELECTRUM.getFluid(), 90))
                .unlockedBy("has_item", has(electrumIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(electrumIngotTag))), "casting:solidifier/electrum/ingot");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.NUGGET_MOLD, 1), new SizedIngredient(Ingredient.of(electrumNuggetTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_ELECTRUM.getFluid(), 10))
                .unlockedBy("has_item", has(electrumIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(electrumNuggetTag))), "casting:solidifier/electrum/nugget");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.GEAR_MOLD, 1), new SizedIngredient(Ingredient.of(electrumGearTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_ELECTRUM.getFluid(), 360))
                .unlockedBy("has_item", has(electrumIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(electrumGearTag))), "casting:solidifier/electrum/gear");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.ROD_MOLD, 1), new SizedIngredient(Ingredient.of(electrumRodTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_ELECTRUM.getFluid(), 90))
                .unlockedBy("has_item", has(electrumIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(electrumRodTag))), "casting:solidifier/electrum/rod");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.PLATE_MOLD, 1), new SizedIngredient(Ingredient.of(electrumPlateTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_ELECTRUM.getFluid(), 90))
                .unlockedBy("has_item", has(electrumIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(electrumPlateTag))), "casting:solidifier/electrum/plate");


        //Invar Processing

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(invarBlockTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_INVAR.getFluid(), 810), 1000)
                .unlockedBy("has_item", has(invarBlockTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(invarBlockTag))), "casting:melting/invar/from_block");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(invarIngotTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_INVAR.getFluid(), 90), 1000)
                .unlockedBy("has_item", has(invarIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(invarIngotTag))), "casting:melting/invar/from_ingot");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(invarDustTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_INVAR.getFluid(), 90), 1000)
                .unlockedBy("has_item", has(invarDustTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(invarDustTag))), "casting:melting/invar/from_dust");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(invarNuggetTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_INVAR.getFluid(), 10), 1000)
                .unlockedBy("has_item", has(invarNuggetTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(invarNuggetTag))), "casting:melting/invar/from_nugget");

        MixingRecipeBuilder.MixingRecipesBuilder(
                        new FluidStack(CastingFluids.MOLTEN_IRON.getFluid(), 180),
                        new FluidStack(CastingFluids.MOLTEN_NICKEL.getFluid(), 90),
                        null,
                        null,
                        null,
                        null,
                        new FluidStack(CastingFluids.MOLTEN_INVAR.getFluid(), 270))
                .unlockedBy("has_item", has(invarIngotTag))
                .save(consumer.withConditions(new NotCondition(new TagEmptyCondition(invarIngotTag))), "casting:mixing/invar");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.BLOCK_MOLD, 1), new SizedIngredient(Ingredient.of(invarBlockTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_INVAR.getFluid(), 810))
                .unlockedBy("has_item", has(invarBlockTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(invarBlockTag))), "casting:solidifier/invar/block");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.INGOT_MOLD, 1), new SizedIngredient(Ingredient.of(invarIngotTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_INVAR.getFluid(), 90))
                .unlockedBy("has_item", has(invarIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(invarIngotTag))), "casting:solidifier/invar/ingot");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.NUGGET_MOLD, 1), new SizedIngredient(Ingredient.of(invarNuggetTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_INVAR.getFluid(), 10))
                .unlockedBy("has_item", has(invarIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(invarNuggetTag))), "casting:solidifier/invar/nugget");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.GEAR_MOLD, 1), new SizedIngredient(Ingredient.of(invarGearTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_INVAR.getFluid(), 360))
                .unlockedBy("has_item", has(invarIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(invarGearTag))), "casting:solidifier/invar/gear");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.ROD_MOLD, 1), new SizedIngredient(Ingredient.of(invarRodTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_INVAR.getFluid(), 90))
                .unlockedBy("has_item", has(invarIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(invarRodTag))), "casting:solidifier/invar/rod");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.PLATE_MOLD, 1), new SizedIngredient(Ingredient.of(invarPlateTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_INVAR.getFluid(), 90))
                .unlockedBy("has_item", has(invarIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(invarPlateTag))), "casting:solidifier/invar/plate");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.DUST_MOLD, 1), new SizedIngredient(Ingredient.of(invarDustTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_INVAR.getFluid(), 90))
                .unlockedBy("has_item", has(invarDustTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(invarDustTag))), "casting:solidifier/invar/dust");

        //Nickel

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(nickelOreTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_NICKEL.getFluid(), 100), 1000)
                .unlockedBy("has_item", has(nickelOreTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(nickelOreTag))), "casting:melting/nickel/from_ore");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(nickelRawOreTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_NICKEL.getFluid(), 100), 1000)
                .unlockedBy("has_item", has(nickelRawOreTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(nickelRawOreTag))), "casting:melting/nickel/from_raw_ore");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(nickelRawOreBlockTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_NICKEL.getFluid(), 900), 1000)
                .unlockedBy("has_item", has(nickelRawOreBlockTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(nickelRawOreBlockTag))), "casting:melting/nickel/from_raw_ore_block");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(nickelBlockTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_NICKEL.getFluid(), 810), 1000)
                .unlockedBy("has_item", has(nickelBlockTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(nickelBlockTag))), "casting:melting/nickel/from_block");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(nickelIngotTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_NICKEL.getFluid(), 90), 1000)
                .unlockedBy("has_item", has(nickelIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(nickelIngotTag))), "casting:melting/nickel/from_ingot");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(nickelDustTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_NICKEL.getFluid(), 90), 1000)
                .unlockedBy("has_item", has(nickelDustTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(nickelDustTag))), "casting:melting/nickel/from_dust");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(nickelNuggetTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_NICKEL.getFluid(), 10), 1000)
                .unlockedBy("has_item", has(nickelNuggetTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(nickelNuggetTag))), "casting:melting/nickel/from_nugget");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.BLOCK_MOLD, 1), new SizedIngredient(Ingredient.of(nickelBlockTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_NICKEL.getFluid(), 810))
                .unlockedBy("has_item", has(nickelBlockTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(nickelBlockTag))), "casting:solidifier/nickel/block");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.INGOT_MOLD, 1), new SizedIngredient(Ingredient.of(nickelIngotTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_NICKEL.getFluid(), 90))
                .unlockedBy("has_item", has(nickelIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(nickelIngotTag))), "casting:solidifier/nickel/ingot");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.NUGGET_MOLD, 1), new SizedIngredient(Ingredient.of(nickelNuggetTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_NICKEL.getFluid(), 10))
                .unlockedBy("has_item", has(nickelIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(nickelNuggetTag))), "casting:solidifier/nickel/nugget");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.GEAR_MOLD, 1), new SizedIngredient(Ingredient.of(nickelGearTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_NICKEL.getFluid(), 360))
                .unlockedBy("has_item", has(nickelIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(nickelGearTag))), "casting:solidifier/nickel/gear");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.ROD_MOLD, 1), new SizedIngredient(Ingredient.of(nickelRodTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_NICKEL.getFluid(), 90))
                .unlockedBy("has_item", has(nickelIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(nickelRodTag))), "casting:solidifier/nickel/rod");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.PLATE_MOLD, 1), new SizedIngredient(Ingredient.of(nickelPlateTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_NICKEL.getFluid(), 90))
                .unlockedBy("has_item", has(nickelIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(nickelPlateTag))), "casting:solidifier/nickel/plate");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.DUST_MOLD, 1), new SizedIngredient(Ingredient.of(nickelDustTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_NICKEL.getFluid(), 90))
                .unlockedBy("has_item", has(nickelDustTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(nickelDustTag))), "casting:solidifier/nickel/dust");


        //Zinc

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(zincOreTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_ZINC.getFluid(), 100), 1000)
                .unlockedBy("has_item", has(zincOreTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(zincOreTag))), "casting:melting/zinc/from_ore");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(zincRawOreTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_ZINC.getFluid(), 100), 1000)
                .unlockedBy("has_item", has(zincRawOreTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(zincRawOreTag))), "casting:melting/zinc/from_raw_ore");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(zincRawOreBlockTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_ZINC.getFluid(), 900), 1000)
                .unlockedBy("has_item", has(zincRawOreBlockTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(zincRawOreBlockTag))), "casting:melting/zinc/from_raw_ore_block");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(zincBlockTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_ZINC.getFluid(), 810), 1000)
                .unlockedBy("has_item", has(zincBlockTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(zincBlockTag))), "casting:melting/zinc/from_block");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(zincIngotTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_ZINC.getFluid(), 90), 1000)
                .unlockedBy("has_item", has(zincIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(zincIngotTag))), "casting:melting/zinc/from_ingot");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(zincDustTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_ZINC.getFluid(), 90), 1000)
                .unlockedBy("has_item", has(zincDustTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(zincDustTag))), "casting:melting/zinc/from_dust");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(zincNuggetTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_ZINC.getFluid(), 10), 1000)
                .unlockedBy("has_item", has(zincNuggetTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(zincNuggetTag))), "casting:melting/zinc/from_nugget");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.BLOCK_MOLD, 1), new SizedIngredient(Ingredient.of(zincBlockTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_ZINC.getFluid(), 810))
                .unlockedBy("has_item", has(zincBlockTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(zincBlockTag))), "casting:solidifier/zinc/block");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.INGOT_MOLD, 1), new SizedIngredient(Ingredient.of(zincIngotTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_ZINC.getFluid(), 90))
                .unlockedBy("has_item", has(zincIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(zincIngotTag))), "casting:solidifier/zinc/ingot");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.NUGGET_MOLD, 1), new SizedIngredient(Ingredient.of(zincNuggetTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_ZINC.getFluid(), 10))
                .unlockedBy("has_item", has(zincIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(zincNuggetTag))), "casting:solidifier/zinc/nugget");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.GEAR_MOLD, 1), new SizedIngredient(Ingredient.of(zincGearTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_ZINC.getFluid(), 360))
                .unlockedBy("has_item", has(zincIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(zincGearTag))), "casting:solidifier/zinc/gear");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.ROD_MOLD, 1), new SizedIngredient(Ingredient.of(zincRodTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_ZINC.getFluid(), 90))
                .unlockedBy("has_item", has(zincIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(zincRodTag))), "casting:solidifier/zinc/rod");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.PLATE_MOLD, 1), new SizedIngredient(Ingredient.of(zincPlateTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_ZINC.getFluid(), 90))
                .unlockedBy("has_item", has(zincIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(zincPlateTag))), "casting:solidifier/zinc/plate");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.DUST_MOLD, 1), new SizedIngredient(Ingredient.of(zincDustTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_ZINC.getFluid(), 90))
                .unlockedBy("has_item", has(zincDustTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(zincDustTag))), "casting:solidifier/zinc/dust");

        //Iridium

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(iridiumOreTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_IRIDIUM.getFluid(), 100), 1000)
                .unlockedBy("has_item", has(iridiumOreTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(iridiumOreTag))), "casting:melting/iridium/from_ore");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(iridiumRawOreTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_IRIDIUM.getFluid(), 100), 1000)
                .unlockedBy("has_item", has(iridiumRawOreTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(iridiumRawOreTag))), "casting:melting/iridium/from_raw_ore");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(iridiumRawOreBlockTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_IRIDIUM.getFluid(), 900), 1000)
                .unlockedBy("has_item", has(iridiumRawOreBlockTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(iridiumRawOreBlockTag))), "casting:melting/iridium/from_raw_ore_block");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(iridiumBlockTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_IRIDIUM.getFluid(), 810), 1000)
                .unlockedBy("has_item", has(iridiumBlockTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(iridiumBlockTag))), "casting:melting/iridium/from_block");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(iridiumIngotTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_IRIDIUM.getFluid(), 90), 1000)
                .unlockedBy("has_item", has(iridiumIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(iridiumIngotTag))), "casting:melting/iridium/from_ingot");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(iridiumDustTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_IRIDIUM.getFluid(), 90), 1000)
                .unlockedBy("has_item", has(iridiumDustTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(iridiumDustTag))), "casting:melting/iridium/from_dust");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(iridiumNuggetTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_IRIDIUM.getFluid(), 10), 1000)
                .unlockedBy("has_item", has(iridiumNuggetTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(iridiumNuggetTag))), "casting:melting/iridium/from_nugget");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.BLOCK_MOLD, 1), new SizedIngredient(Ingredient.of(iridiumBlockTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_IRIDIUM.getFluid(), 810))
                .unlockedBy("has_item", has(iridiumBlockTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(iridiumBlockTag))), "casting:solidifier/iridium/block");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.INGOT_MOLD, 1), new SizedIngredient(Ingredient.of(iridiumIngotTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_IRIDIUM.getFluid(), 90))
                .unlockedBy("has_item", has(iridiumIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(iridiumIngotTag))), "casting:solidifier/iridium/ingot");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.NUGGET_MOLD, 1), new SizedIngredient(Ingredient.of(iridiumNuggetTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_IRIDIUM.getFluid(), 10))
                .unlockedBy("has_item", has(iridiumIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(iridiumNuggetTag))), "casting:solidifier/iridium/nugget");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.GEAR_MOLD, 1), new SizedIngredient(Ingredient.of(iridiumGearTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_IRIDIUM.getFluid(), 360))
                .unlockedBy("has_item", has(iridiumIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(iridiumGearTag))), "casting:solidifier/iridium/gear");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.ROD_MOLD, 1), new SizedIngredient(Ingredient.of(iridiumRodTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_IRIDIUM.getFluid(), 90))
                .unlockedBy("has_item", has(iridiumIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(iridiumRodTag))), "casting:solidifier/iridium/rod");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.PLATE_MOLD, 1), new SizedIngredient(Ingredient.of(iridiumPlateTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_IRIDIUM.getFluid(), 90))
                .unlockedBy("has_item", has(iridiumIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(iridiumPlateTag))), "casting:solidifier/iridium/plate");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.DUST_MOLD, 1), new SizedIngredient(Ingredient.of(iridiumDustTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_IRIDIUM.getFluid(), 90))
                .unlockedBy("has_item", has(iridiumDustTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(iridiumDustTag))), "casting:solidifier/iridium/dust");

        //Platinum

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(platinumOreTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_PLATINUM.getFluid(), 100), 1000)
                .unlockedBy("has_item", has(platinumOreTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(platinumOreTag))), "casting:melting/platinum/from_ore");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(platinumRawOreTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_PLATINUM.getFluid(), 100), 1000)
                .unlockedBy("has_item", has(platinumRawOreTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(platinumRawOreTag))), "casting:melting/platinum/from_raw_ore");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(platinumRawOreBlockTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_PLATINUM.getFluid(), 900), 1000)
                .unlockedBy("has_item", has(platinumRawOreBlockTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(platinumRawOreBlockTag))), "casting:melting/platinum/from_raw_ore_block");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(platinumBlockTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_PLATINUM.getFluid(), 810), 1000)
                .unlockedBy("has_item", has(platinumBlockTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(platinumBlockTag))), "casting:melting/platinum/from_block");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(platinumIngotTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_PLATINUM.getFluid(), 90), 1000)
                .unlockedBy("has_item", has(platinumIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(platinumIngotTag))), "casting:melting/platinum/from_ingot");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(platinumDustTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_PLATINUM.getFluid(), 90), 1000)
                .unlockedBy("has_item", has(platinumDustTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(platinumDustTag))), "casting:melting/platinum/from_dust");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(platinumNuggetTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_PLATINUM.getFluid(), 10), 1000)
                .unlockedBy("has_item", has(platinumNuggetTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(platinumNuggetTag))), "casting:melting/platinum/from_nugget");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.BLOCK_MOLD, 1), new SizedIngredient(Ingredient.of(platinumBlockTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_PLATINUM.getFluid(), 810))
                .unlockedBy("has_item", has(platinumBlockTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(platinumBlockTag))), "casting:solidifier/platinum/block");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.INGOT_MOLD, 1), new SizedIngredient(Ingredient.of(platinumIngotTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_PLATINUM.getFluid(), 90))
                .unlockedBy("has_item", has(platinumIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(platinumIngotTag))), "casting:solidifier/platinum/ingot");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.NUGGET_MOLD, 1), new SizedIngredient(Ingredient.of(platinumNuggetTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_PLATINUM.getFluid(), 10))
                .unlockedBy("has_item", has(platinumIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(platinumNuggetTag))), "casting:solidifier/platinum/nugget");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.GEAR_MOLD, 1), new SizedIngredient(Ingredient.of(platinumGearTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_PLATINUM.getFluid(), 360))
                .unlockedBy("has_item", has(platinumIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(platinumGearTag))), "casting:solidifier/platinum/gear");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.ROD_MOLD, 1), new SizedIngredient(Ingredient.of(platinumRodTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_PLATINUM.getFluid(), 90))
                .unlockedBy("has_item", has(platinumIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(platinumRodTag))), "casting:solidifier/platinum/rod");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.PLATE_MOLD, 1), new SizedIngredient(Ingredient.of(platinumPlateTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_PLATINUM.getFluid(), 90))
                .unlockedBy("has_item", has(platinumIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(platinumPlateTag))), "casting:solidifier/platinum/plate");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.DUST_MOLD, 1), new SizedIngredient(Ingredient.of(platinumDustTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_PLATINUM.getFluid(), 90))
                .unlockedBy("has_item", has(platinumDustTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(platinumDustTag))), "casting:solidifier/platinum/dust");

        //Aluminum

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(aluminumOreTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_ALUMINUM.getFluid(), 100), 1000)
                .unlockedBy("has_item", has(aluminumOreTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(aluminumOreTag))), "casting:melting/aluminum/from_ore");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(aluminumRawOreTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_ALUMINUM.getFluid(), 100), 1000)
                .unlockedBy("has_item", has(aluminumRawOreTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(aluminumRawOreTag))), "casting:melting/aluminum/from_raw_ore");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(aluminumRawOreBlockTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_ALUMINUM.getFluid(), 900), 1000)
                .unlockedBy("has_item", has(aluminumRawOreBlockTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(aluminumRawOreBlockTag))), "casting:melting/aluminum/from_raw_ore_block");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(aluminumBlockTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_ALUMINUM.getFluid(), 810), 1000)
                .unlockedBy("has_item", has(aluminumBlockTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(aluminumBlockTag))), "casting:melting/aluminum/from_block");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(aluminumIngotTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_ALUMINUM.getFluid(), 90), 1000)
                .unlockedBy("has_item", has(aluminumIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(aluminumIngotTag))), "casting:melting/aluminum/from_ingot");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(aluminumDustTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_ALUMINUM.getFluid(), 90), 1000)
                .unlockedBy("has_item", has(aluminumDustTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(aluminumDustTag))), "casting:melting/aluminum/from_dust");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(aluminumNuggetTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_ALUMINUM.getFluid(), 10), 1000)
                .unlockedBy("has_item", has(aluminumNuggetTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(aluminumNuggetTag))), "casting:melting/aluminum/from_nugget");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.BLOCK_MOLD, 1), new SizedIngredient(Ingredient.of(aluminumBlockTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_ALUMINUM.getFluid(), 810))
                .unlockedBy("has_item", has(aluminumBlockTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(aluminumBlockTag))), "casting:solidifier/aluminum/block");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.INGOT_MOLD, 1), new SizedIngredient(Ingredient.of(aluminumIngotTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_ALUMINUM.getFluid(), 90))
                .unlockedBy("has_item", has(aluminumIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(aluminumIngotTag))), "casting:solidifier/aluminum/ingot");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.NUGGET_MOLD, 1), new SizedIngredient(Ingredient.of(aluminumNuggetTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_ALUMINUM.getFluid(), 10))
                .unlockedBy("has_item", has(aluminumIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(aluminumNuggetTag))), "casting:solidifier/aluminum/nugget");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.GEAR_MOLD, 1), new SizedIngredient(Ingredient.of(aluminumGearTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_ALUMINUM.getFluid(), 360))
                .unlockedBy("has_item", has(aluminumIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(aluminumGearTag))), "casting:solidifier/aluminum/gear");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.ROD_MOLD, 1), new SizedIngredient(Ingredient.of(aluminumRodTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_ALUMINUM.getFluid(), 90))
                .unlockedBy("has_item", has(aluminumIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(aluminumRodTag))), "casting:solidifier/aluminum/rod");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.PLATE_MOLD, 1), new SizedIngredient(Ingredient.of(aluminumPlateTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_ALUMINUM.getFluid(), 90))
                .unlockedBy("has_item", has(aluminumIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(aluminumPlateTag))), "casting:solidifier/aluminum/plate");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.DUST_MOLD, 1), new SizedIngredient(Ingredient.of(aluminumDustTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_ALUMINUM.getFluid(), 90))
                .unlockedBy("has_item", has(aluminumDustTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(aluminumDustTag))), "casting:solidifier/aluminum/dust");

        //Singalum Processing

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(signalumBlockTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_SIGNALUM.getFluid(), 810), 1000)
                .unlockedBy("has_item", has(signalumBlockTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(signalumBlockTag))), "casting:melting/signalum/from_block");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(signalumIngotTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_SIGNALUM.getFluid(), 90), 1000)
                .unlockedBy("has_item", has(signalumIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(signalumIngotTag))), "casting:melting/signalum/from_ingot");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(signalumDustTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_SIGNALUM.getFluid(), 90), 1000)
                .unlockedBy("has_item", has(signalumDustTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(signalumDustTag))), "casting:melting/signalum/from_dust");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(signalumNuggetTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_SIGNALUM.getFluid(), 10), 1000)
                .unlockedBy("has_item", has(signalumNuggetTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(signalumNuggetTag))), "casting:melting/signalum/from_nugget");

        MixingRecipeBuilder.MixingRecipesBuilder(
                        new FluidStack(CastingFluids.MOLTEN_COPPER.getFluid(), 270),
                        new FluidStack(CastingFluids.MOLTEN_SILVER.getFluid(), 90),
                        new FluidStack(CastingFluids.MOLTEN_REDSTONE.getFluid(), 360),
                        null,
                        null,
                        null,
                        new FluidStack(CastingFluids.MOLTEN_SIGNALUM.getFluid(), 360))
                .unlockedBy("has_item", has(signalumIngotTag))
                .save(consumer.withConditions(new NotCondition(new TagEmptyCondition(signalumIngotTag))), "casting:mixing/signalum");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.BLOCK_MOLD, 1), new SizedIngredient(Ingredient.of(signalumBlockTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_SIGNALUM.getFluid(), 810))
                .unlockedBy("has_item", has(signalumBlockTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(signalumBlockTag))), "casting:solidifier/signalum/block");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.INGOT_MOLD, 1), new SizedIngredient(Ingredient.of(signalumIngotTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_SIGNALUM.getFluid(), 90))
                .unlockedBy("has_item", has(signalumIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(signalumIngotTag))), "casting:solidifier/signalum/ingot");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.NUGGET_MOLD, 1), new SizedIngredient(Ingredient.of(signalumNuggetTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_SIGNALUM.getFluid(), 10))
                .unlockedBy("has_item", has(signalumNuggetTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(signalumNuggetTag))), "casting:solidifier/signalum/nugget");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.GEAR_MOLD, 1), new SizedIngredient(Ingredient.of(signalumGearTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_SIGNALUM.getFluid(), 360))
                .unlockedBy("has_item", has(signalumGearTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(signalumGearTag))), "casting:solidifier/signalum/gear");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.ROD_MOLD, 1), new SizedIngredient(Ingredient.of(signalumRodTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_SIGNALUM.getFluid(), 90))
                .unlockedBy("has_item", has(signalumRodTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(signalumRodTag))), "casting:solidifier/signalum/rod");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.PLATE_MOLD, 1), new SizedIngredient(Ingredient.of(signalumPlateTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_SIGNALUM.getFluid(), 90))
                .unlockedBy("has_item", has(signalumPlateTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(signalumPlateTag))), "casting:solidifier/signalum/plate");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.DUST_MOLD, 1), new SizedIngredient(Ingredient.of(signalumDustTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_SIGNALUM.getFluid(), 90))
                .unlockedBy("has_item", has(signalumDustTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(signalumDustTag))), "casting:solidifier/signalum/dust");

        //Lumium Processing

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(lumiumBlockTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_LUMIUM.getFluid(), 810), 1000)
                .unlockedBy("has_item", has(lumiumBlockTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(lumiumBlockTag))), "casting:melting/lumium/from_block");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(lumiumIngotTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_LUMIUM.getFluid(), 90), 1000)
                .unlockedBy("has_item", has(lumiumIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(lumiumIngotTag))), "casting:melting/lumium/from_ingot");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(lumiumDustTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_LUMIUM.getFluid(), 90), 1000)
                .unlockedBy("has_item", has(lumiumDustTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(lumiumDustTag))), "casting:melting/lumium/from_dust");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(lumiumNuggetTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_LUMIUM.getFluid(), 10), 1000)
                .unlockedBy("has_item", has(lumiumNuggetTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(lumiumNuggetTag))), "casting:melting/lumium/from_nugget");

        MixingRecipeBuilder.MixingRecipesBuilder(
                        new FluidStack(CastingFluids.MOLTEN_TIN.getFluid(), 270),
                        new FluidStack(CastingFluids.MOLTEN_SILVER.getFluid(), 90),
                        new FluidStack(CastingFluids.MOLTEN_GLOWSTONE.getFluid(), 500),
                        null,
                        null,
                        null,
                        new FluidStack(CastingFluids.MOLTEN_LUMIUM.getFluid(), 360))
                .unlockedBy("has_item", has(lumiumIngotTag))
                .save(consumer.withConditions(new NotCondition(new TagEmptyCondition(lumiumIngotTag))), "casting:mixing/lumium");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.BLOCK_MOLD, 1), new SizedIngredient(Ingredient.of(lumiumBlockTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_LUMIUM.getFluid(), 810))
                .unlockedBy("has_item", has(lumiumBlockTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(lumiumBlockTag))), "casting:solidifier/lumium/block");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.INGOT_MOLD, 1), new SizedIngredient(Ingredient.of(lumiumIngotTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_LUMIUM.getFluid(), 90))
                .unlockedBy("has_item", has(lumiumIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(lumiumIngotTag))), "casting:solidifier/lumium/ingot");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.NUGGET_MOLD, 1), new SizedIngredient(Ingredient.of(lumiumNuggetTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_LUMIUM.getFluid(), 10))
                .unlockedBy("has_item", has(lumiumNuggetTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(lumiumNuggetTag))), "casting:solidifier/lumium/nugget");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.GEAR_MOLD, 1), new SizedIngredient(Ingredient.of(lumiumGearTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_LUMIUM.getFluid(), 360))
                .unlockedBy("has_item", has(lumiumGearTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(lumiumGearTag))), "casting:solidifier/lumium/gear");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.ROD_MOLD, 1), new SizedIngredient(Ingredient.of(lumiumRodTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_LUMIUM.getFluid(), 90))
                .unlockedBy("has_item", has(lumiumRodTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(lumiumRodTag))), "casting:solidifier/lumium/rod");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.PLATE_MOLD, 1), new SizedIngredient(Ingredient.of(lumiumPlateTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_LUMIUM.getFluid(), 90))
                .unlockedBy("has_item", has(lumiumPlateTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(lumiumPlateTag))), "casting:solidifier/lumium/plate");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.DUST_MOLD, 1), new SizedIngredient(Ingredient.of(lumiumDustTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_LUMIUM.getFluid(), 90))
                .unlockedBy("has_item", has(lumiumDustTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(lumiumDustTag))), "casting:solidifier/lumium/dust");

        //Enderium Processing

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(enderiumBlockTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_ENDERIUM.getFluid(), 810), 1000)
                .unlockedBy("has_item", has(enderiumBlockTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(enderiumBlockTag))), "casting:melting/enderium/from_block");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(enderiumIngotTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_ENDERIUM.getFluid(), 90), 1000)
                .unlockedBy("has_item", has(enderiumIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(enderiumIngotTag))), "casting:melting/enderium/from_ingot");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(enderiumDustTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_ENDERIUM.getFluid(), 90), 1000)
                .unlockedBy("has_item", has(enderiumDustTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(enderiumDustTag))), "casting:melting/enderium/from_dust");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(enderiumNuggetTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_ENDERIUM.getFluid(), 10), 1000)
                .unlockedBy("has_item", has(enderiumNuggetTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(enderiumNuggetTag))), "casting:melting/enderium/from_nugget");

        MixingRecipeBuilder.MixingRecipesBuilder(
                        new FluidStack(CastingFluids.MOLTEN_LEAD.getFluid(), 270),
                        new FluidStack(CastingFluids.MOLTEN_PLATINUM.getFluid(), 90),
                        new FluidStack(CastingFluids.MOLTEN_ENDER.getFluid(), 160),
                        null,
                        null,
                        null,
                        new FluidStack(CastingFluids.MOLTEN_ENDERIUM.getFluid(), 360))
                .unlockedBy("has_item", has(enderiumIngotTag))
                .save(consumer.withConditions(new NotCondition(new TagEmptyCondition(enderiumIngotTag))), "casting:mixing/enderium");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.BLOCK_MOLD, 1), new SizedIngredient(Ingredient.of(enderiumBlockTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_ENDERIUM.getFluid(), 810))
                .unlockedBy("has_item", has(enderiumBlockTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(enderiumBlockTag))), "casting:solidifier/enderium/block");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.INGOT_MOLD, 1), new SizedIngredient(Ingredient.of(enderiumIngotTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_ENDERIUM.getFluid(), 90))
                .unlockedBy("has_item", has(enderiumIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(enderiumIngotTag))), "casting:solidifier/enderium/ingot");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.NUGGET_MOLD, 1), new SizedIngredient(Ingredient.of(enderiumNuggetTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_ENDERIUM.getFluid(), 10))
                .unlockedBy("has_item", has(enderiumNuggetTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(enderiumNuggetTag))), "casting:solidifier/enderium/nugget");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.GEAR_MOLD, 1), new SizedIngredient(Ingredient.of(enderiumGearTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_ENDERIUM.getFluid(), 360))
                .unlockedBy("has_item", has(enderiumGearTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(enderiumGearTag))), "casting:solidifier/enderium/gear");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.ROD_MOLD, 1), new SizedIngredient(Ingredient.of(enderiumRodTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_ENDERIUM.getFluid(), 90))
                .unlockedBy("has_item", has(enderiumRodTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(enderiumRodTag))), "casting:solidifier/enderium/rod");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.PLATE_MOLD, 1), new SizedIngredient(Ingredient.of(enderiumPlateTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_ENDERIUM.getFluid(), 90))
                .unlockedBy("has_item", has(enderiumPlateTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(enderiumPlateTag))), "casting:solidifier/enderium/plate");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.DUST_MOLD, 1), new SizedIngredient(Ingredient.of(enderiumDustTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_ENDERIUM.getFluid(), 90))
                .unlockedBy("has_item", has(enderiumDustTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(enderiumDustTag))), "casting:solidifier/enderium/dust");

        //Constantan Processing

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(constantanBlockTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_CONSTANTAN.getFluid(), 810), 1000)
                .unlockedBy("has_item", has(constantanBlockTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(constantanBlockTag))), "casting:melting/constantan/from_block");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(constantanIngotTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_CONSTANTAN.getFluid(), 90), 1000)
                .unlockedBy("has_item", has(constantanIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(constantanIngotTag))), "casting:melting/constantan/from_ingot");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(constantanDustTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_CONSTANTAN.getFluid(), 90), 1000)
                .unlockedBy("has_item", has(constantanDustTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(constantanDustTag))), "casting:melting/constantan/from_dust");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(constantanNuggetTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_CONSTANTAN.getFluid(), 10), 1000)
                .unlockedBy("has_item", has(constantanNuggetTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(constantanNuggetTag))), "casting:melting/constantan/from_nugget");

        MixingRecipeBuilder.MixingRecipesBuilder(
                        new FluidStack(CastingFluids.MOLTEN_COPPER.getFluid(), 90),
                        new FluidStack(CastingFluids.MOLTEN_NICKEL.getFluid(), 90),
                        null,
                        null,
                        null,
                        null,
                        new FluidStack(CastingFluids.MOLTEN_CONSTANTAN.getFluid(), 180))
                .unlockedBy("has_item", has(constantanIngotTag))
                .save(consumer.withConditions(new NotCondition(new TagEmptyCondition(constantanIngotTag))), "casting:mixing/constantan");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.BLOCK_MOLD, 1), new SizedIngredient(Ingredient.of(constantanBlockTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_CONSTANTAN.getFluid(), 810))
                .unlockedBy("has_item", has(constantanBlockTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(constantanBlockTag))), "casting:solidifier/constantan/block");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.INGOT_MOLD, 1), new SizedIngredient(Ingredient.of(constantanIngotTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_CONSTANTAN.getFluid(), 90))
                .unlockedBy("has_item", has(constantanIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(constantanIngotTag))), "casting:solidifier/constantan/ingot");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.NUGGET_MOLD, 1), new SizedIngredient(Ingredient.of(constantanNuggetTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_CONSTANTAN.getFluid(), 10))
                .unlockedBy("has_item", has(constantanNuggetTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(constantanNuggetTag))), "casting:solidifier/constantan/nugget");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.GEAR_MOLD, 1), new SizedIngredient(Ingredient.of(constantanGearTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_CONSTANTAN.getFluid(), 360))
                .unlockedBy("has_item", has(constantanGearTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(constantanGearTag))), "casting:solidifier/constantan/gear");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.ROD_MOLD, 1), new SizedIngredient(Ingredient.of(constantanRodTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_CONSTANTAN.getFluid(), 90))
                .unlockedBy("has_item", has(constantanRodTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(constantanRodTag))), "casting:solidifier/constantan/rod");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.PLATE_MOLD, 1), new SizedIngredient(Ingredient.of(constantanPlateTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_CONSTANTAN.getFluid(), 90))
                .unlockedBy("has_item", has(constantanPlateTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(constantanPlateTag))), "casting:solidifier/constantan/plate");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.DUST_MOLD, 1), new SizedIngredient(Ingredient.of(constantanDustTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_CONSTANTAN.getFluid(), 90))
                .unlockedBy("has_item", has(constantanDustTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(constantanDustTag))), "casting:solidifier/constantan/dust");

        //Brass Processing

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(brassBlockTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_BRASS.getFluid(), 810), 1000)
                .unlockedBy("has_item", has(brassBlockTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(brassBlockTag))), "casting:melting/brass/from_block");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(brassIngotTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_BRASS.getFluid(), 90), 1000)
                .unlockedBy("has_item", has(brassIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(brassIngotTag))), "casting:melting/brass/from_ingot");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(brassDustTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_BRASS.getFluid(), 90), 1000)
                .unlockedBy("has_item", has(brassDustTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(brassDustTag))), "casting:melting/brass/from_dust");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(brassNuggetTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_BRASS.getFluid(), 10), 1000)
                .unlockedBy("has_item", has(brassNuggetTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(brassNuggetTag))), "casting:melting/brass/from_nugget");

        MixingRecipeBuilder.MixingRecipesBuilder(
                        new FluidStack(CastingFluids.MOLTEN_COPPER.getFluid(), 270),
                        new FluidStack(CastingFluids.MOLTEN_ZINC.getFluid(), 90),
                        null,
                        null,
                        null,
                        null,
                        new FluidStack(CastingFluids.MOLTEN_BRASS.getFluid(), 360))
                .unlockedBy("has_item", has(brassIngotTag))
                .save(consumer.withConditions(new NotCondition(new TagEmptyCondition(brassIngotTag))), "casting:mixing/brass");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.BLOCK_MOLD, 1), new SizedIngredient(Ingredient.of(brassBlockTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_BRASS.getFluid(), 810))
                .unlockedBy("has_item", has(brassBlockTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(brassBlockTag))), "casting:solidifier/brass/block");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.INGOT_MOLD, 1), new SizedIngredient(Ingredient.of(brassIngotTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_BRASS.getFluid(), 90))
                .unlockedBy("has_item", has(brassIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(brassIngotTag))), "casting:solidifier/brass/ingot");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.NUGGET_MOLD, 1), new SizedIngredient(Ingredient.of(brassNuggetTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_BRASS.getFluid(), 10))
                .unlockedBy("has_item", has(brassNuggetTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(brassNuggetTag))), "casting:solidifier/brass/nugget");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.GEAR_MOLD, 1), new SizedIngredient(Ingredient.of(brassGearTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_BRASS.getFluid(), 360))
                .unlockedBy("has_item", has(brassGearTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(brassGearTag))), "casting:solidifier/brass/gear");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.ROD_MOLD, 1), new SizedIngredient(Ingredient.of(brassRodTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_BRASS.getFluid(), 90))
                .unlockedBy("has_item", has(brassRodTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(brassRodTag))), "casting:solidifier/brass/rod");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.PLATE_MOLD, 1), new SizedIngredient(Ingredient.of(brassPlateTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_BRASS.getFluid(), 90))
                .unlockedBy("has_item", has(brassPlateTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(brassPlateTag))), "casting:solidifier/brass/plate");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.DUST_MOLD, 1), new SizedIngredient(Ingredient.of(brassDustTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_BRASS.getFluid(), 90))
                .unlockedBy("has_item", has(brassDustTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(brassDustTag))), "casting:solidifier/brass/dust");

        //Ender Processing

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(Items.ENDER_PEARL), 1),
                        new FluidStack(CastingFluids.MOLTEN_ENDER.getFluid(), 80), 700)
                .unlockedBy("has_item", has(Items.ENDER_PEARL)).save(consumer, "casting:melting/ender/from_gem");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(com.benbenlaw.opolisutilities.item.ModItems.ENDER_PEARL_FRAGMENT), 1),
                        new FluidStack(CastingFluids.MOLTEN_ENDER.getFluid(), 10), 700)
                .unlockedBy("has_item", has(Items.ENDER_PEARL)).save(consumer, "casting:melting/ender/from_fragment");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.NUGGET_MOLD, 1), new SizedIngredient(Ingredient.of(com.benbenlaw.opolisutilities.item.ModItems.ENDER_PEARL_FRAGMENT), 1),
                        new FluidStack(CastingFluids.MOLTEN_ENDER.getFluid(), 10))
                .unlockedBy("has_item", has(com.benbenlaw.opolisutilities.item.ModItems.ENDER_PEARL_FRAGMENT)).save(consumer, "casting:solidifier/ender/fragment");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.BALL_MOLD, 1), new SizedIngredient(Ingredient.of(Items.ENDER_PEARL), 1),
                        new FluidStack(CastingFluids.MOLTEN_ENDER.getFluid(), 80))
                .unlockedBy("has_item", has(Items.ENDER_PEARL)).save(consumer, "casting:solidifier/ender/gem");

        //Glowstone Processing

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(Items.GLOWSTONE_DUST), 1),
                        new FluidStack(CastingFluids.MOLTEN_GLOWSTONE.getFluid(), 250), 1000)
                .unlockedBy("has_item", has(Items.GLOWSTONE_DUST)).save(consumer, "casting:melting/glowstone/from_dust");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(Items.GLOWSTONE), 1),
                        new FluidStack(CastingFluids.MOLTEN_GLOWSTONE.getFluid(), 1000), 1000)
                .unlockedBy("has_item", has(Items.GLOWSTONE)).save(consumer, "casting:melting/glowstone/from_block");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.DUST_MOLD, 1), new SizedIngredient(Ingredient.of(Items.GLOWSTONE_DUST), 1),
                        new FluidStack(CastingFluids.MOLTEN_GLOWSTONE.getFluid(), 250))
                .unlockedBy("has_item", has(Items.GLOWSTONE_DUST)).save(consumer, "casting:solidifier/glowstone/dust");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.BLOCK_MOLD, 1), new SizedIngredient(Ingredient.of(Items.GLOWSTONE), 1),
                        new FluidStack(CastingFluids.MOLTEN_GLOWSTONE.getFluid(), 1000))
                .unlockedBy("has_item", has(Items.GLOWSTONE)).save(consumer, "casting:solidifier/glowstone/block");

        //Silicon

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(silicon), 1),
                        new FluidStack(CastingFluids.MOLTEN_SILICON.getFluid(), 250), 1200)
                .unlockedBy("has_item", has(silicon)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(silicon))), "casting:melting/silicon/from_item");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(siliconDust), 1),
                        new FluidStack(CastingFluids.MOLTEN_SILICON.getFluid(), 250), 1200)
                .unlockedBy("has_item", has(siliconDust)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(siliconDust))), "casting:melting/silicon/from_dust");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.DUST_MOLD, 1), new SizedIngredient(Ingredient.of(siliconDust), 1),
                        new FluidStack(CastingFluids.MOLTEN_SILICON.getFluid(), 250))
                .unlockedBy("has_item", has(siliconDust)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(siliconDust))), "casting:solidifier/silicon/dust");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.BALL_MOLD, 1), new SizedIngredient(Ingredient.of(silicon), 1),
                        new FluidStack(CastingFluids.MOLTEN_SILICON.getFluid(), 250))
                .unlockedBy("has_item", has(silicon)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(silicon))), "casting:solidifier/silicon/item");


        //Soul

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(ItemTags.SOUL_FIRE_BASE_BLOCKS), 1),
                        new FluidStack(CastingFluids.MOLTEN_SOUL.getFluid(), 1000), 1200)
                .unlockedBy("has_item", has(Items.SOUL_SAND)).save(consumer, "casting:melting/soul/from_sand");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.BLOCK_MOLD, 1), new SizedIngredient(Ingredient.of(ItemTags.SOUL_FIRE_BASE_BLOCKS), 1),
                        new FluidStack(CastingFluids.MOLTEN_SOUL.getFluid(), 1000))
                .unlockedBy("has_item", has(Items.SOUL_SAND)).save(consumer, "casting:solidifier/soul/soul_sand");

        //End Stone

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(Items.END_STONE), 1),
                        new FluidStack(CastingFluids.MOLTEN_END_STONE.getFluid(), 1000), 1200)
                .unlockedBy("has_item", has(Items.END_STONE)).save(consumer, "casting:melting/end_stone/from_block");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.BLOCK_MOLD, 1), new SizedIngredient(Ingredient.of(Items.END_STONE), 1),
                        new FluidStack(CastingFluids.MOLTEN_END_STONE.getFluid(), 1000))
                .unlockedBy("has_item", has(Items.END_STONE)).save(consumer, "casting:solidifier/end_stone/end_stone");

        //Conductive Alloy Processing

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(conductiveAlloyBlockTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_CONDUCTIVE_ALLOY.getFluid(), 810), 1000)
                .unlockedBy("has_item", has(conductiveAlloyBlockTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(conductiveAlloyBlockTag))), "casting:melting/conductive_alloy/from_block");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(conductiveAlloyIngotTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_CONDUCTIVE_ALLOY.getFluid(), 90), 1000)
                .unlockedBy("has_item", has(conductiveAlloyIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(conductiveAlloyIngotTag))), "casting:melting/conductive_alloy/from_ingot");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(conductiveAlloyDustTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_CONDUCTIVE_ALLOY.getFluid(), 90), 1000)
                .unlockedBy("has_item", has(conductiveAlloyDustTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(conductiveAlloyDustTag))), "casting:melting/conductive_alloy/from_dust");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(conductiveAlloyNuggetTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_CONDUCTIVE_ALLOY.getFluid(), 10), 1000)
                .unlockedBy("has_item", has(conductiveAlloyNuggetTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(conductiveAlloyNuggetTag))), "casting:melting/conductive_alloy/from_nugget");

        MixingRecipeBuilder.MixingRecipesBuilder(
                        new FluidStack(CastingFluids.MOLTEN_REDSTONE.getFluid(), 90),
                        new FluidStack(CastingFluids.MOLTEN_IRON.getFluid(), 90),
                        new FluidStack(CastingFluids.MOLTEN_COPPER_ALLOY.getFluid(), 90),
                        null,
                        null,
                        null,
                        new FluidStack(CastingFluids.MOLTEN_CONDUCTIVE_ALLOY.getFluid(), 90))
                .unlockedBy("has_item", has(conductiveAlloyIngotTag))
                .save(consumer.withConditions(new NotCondition(new TagEmptyCondition(conductiveAlloyIngotTag))), "casting:mixing/conductive_alloy");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.BLOCK_MOLD, 1), new SizedIngredient(Ingredient.of(conductiveAlloyBlockTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_CONDUCTIVE_ALLOY.getFluid(), 810))
                .unlockedBy("has_item", has(conductiveAlloyBlockTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(conductiveAlloyBlockTag))), "casting:solidifier/conductive_alloy/block");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.INGOT_MOLD, 1), new SizedIngredient(Ingredient.of(conductiveAlloyIngotTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_CONDUCTIVE_ALLOY.getFluid(), 90))
                .unlockedBy("has_item", has(conductiveAlloyIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(conductiveAlloyIngotTag))), "casting:solidifier/conductive_alloy/ingot");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.NUGGET_MOLD, 1), new SizedIngredient(Ingredient.of(conductiveAlloyNuggetTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_CONDUCTIVE_ALLOY.getFluid(), 10))
                .unlockedBy("has_item", has(conductiveAlloyNuggetTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(conductiveAlloyNuggetTag))), "casting:solidifier/conductive_alloy/nugget");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.GEAR_MOLD, 1), new SizedIngredient(Ingredient.of(conductiveAlloyGearTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_CONDUCTIVE_ALLOY.getFluid(), 360))
                .unlockedBy("has_item", has(conductiveAlloyGearTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(conductiveAlloyGearTag))), "casting:solidifier/conductive_alloy/gear");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.ROD_MOLD, 1), new SizedIngredient(Ingredient.of(conductiveAlloyRodTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_CONDUCTIVE_ALLOY.getFluid(), 90))
                .unlockedBy("has_item", has(conductiveAlloyRodTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(conductiveAlloyRodTag))), "casting:solidifier/conductive_alloy/rod");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.PLATE_MOLD, 1), new SizedIngredient(Ingredient.of(conductiveAlloyPlateTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_CONDUCTIVE_ALLOY.getFluid(), 90))
                .unlockedBy("has_item", has(conductiveAlloyPlateTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(conductiveAlloyPlateTag))), "casting:solidifier/conductive_alloy/plate");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.DUST_MOLD, 1), new SizedIngredient(Ingredient.of(conductiveAlloyDustTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_CONDUCTIVE_ALLOY.getFluid(), 90))
                .unlockedBy("has_item", has(conductiveAlloyDustTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(conductiveAlloyDustTag))), "casting:solidifier/conductive_alloy/dust");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.BALL_MOLD, 1), new SizedIngredient(Ingredient.of(BuiltInRegistries.ITEM.get(ResourceLocation.parse("enderio:conductive_alloy_grinding_ball")).getDefaultInstance()), 24),
                        new FluidStack(CastingFluids.MOLTEN_CONDUCTIVE_ALLOY.getFluid(), 450))
                .unlockedBy("has_item", has(conductiveAlloyIngotTag)).save(consumer.withConditions(new ModLoadedCondition("enderio")), "casting:solidifier/conductive_alloy/ball");

        //Energetic Alloy Processing

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(energeticAlloyBlockTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_ENERGETIC_ALLOY.getFluid(), 810), 1000)
                .unlockedBy("has_item", has(energeticAlloyBlockTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(energeticAlloyBlockTag))), "casting:melting/energetic_alloy/from_block");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(energeticAlloyIngotTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_ENERGETIC_ALLOY.getFluid(), 90), 1000)
                .unlockedBy("has_item", has(energeticAlloyIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(energeticAlloyIngotTag))), "casting:melting/energetic_alloy/from_ingot");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(energeticAlloyDustTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_ENERGETIC_ALLOY.getFluid(), 90), 1000)
                .unlockedBy("has_item", has(energeticAlloyDustTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(energeticAlloyDustTag))), "casting:melting/energetic_alloy/from_dust");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(energeticAlloyNuggetTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_ENERGETIC_ALLOY.getFluid(), 10), 1000)
                .unlockedBy("has_item", has(energeticAlloyNuggetTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(energeticAlloyNuggetTag))), "casting:melting/energetic_alloy/from_nugget");

        MixingRecipeBuilder.MixingRecipesBuilder(
                        new FluidStack(CastingFluids.MOLTEN_REDSTONE.getFluid(), 90),
                        new FluidStack(CastingFluids.MOLTEN_GOLD.getFluid(), 90),
                        new FluidStack(CastingFluids.MOLTEN_GLOWSTONE.getFluid(), 250),
                        null,
                        null,
                        null,
                        new FluidStack(CastingFluids.MOLTEN_ENERGETIC_ALLOY.getFluid(), 90))
                .unlockedBy("has_item", has(energeticAlloyIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(energeticAlloyIngotTag))), "casting:mixing/energetic_alloy");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.BLOCK_MOLD, 1), new SizedIngredient(Ingredient.of(energeticAlloyBlockTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_ENERGETIC_ALLOY.getFluid(), 810))
                .unlockedBy("has_item", has(energeticAlloyBlockTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(energeticAlloyBlockTag))), "casting:solidifier/energetic_alloy/block");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.INGOT_MOLD, 1), new SizedIngredient(Ingredient.of(energeticAlloyIngotTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_ENERGETIC_ALLOY.getFluid(), 90))
                .unlockedBy("has_item", has(energeticAlloyIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(energeticAlloyIngotTag))), "casting:solidifier/energetic_alloy/ingot");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.NUGGET_MOLD, 1), new SizedIngredient(Ingredient.of(energeticAlloyNuggetTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_ENERGETIC_ALLOY.getFluid(), 10))
                .unlockedBy("has_item", has(energeticAlloyNuggetTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(energeticAlloyNuggetTag))), "casting:solidifier/energetic_alloy/nugget");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.GEAR_MOLD, 1), new SizedIngredient(Ingredient.of(energeticAlloyGearTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_ENERGETIC_ALLOY.getFluid(), 360))
                .unlockedBy("has_item", has(energeticAlloyGearTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(energeticAlloyGearTag))), "casting:solidifier/energetic_alloy/gear");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.ROD_MOLD, 1), new SizedIngredient(Ingredient.of(energeticAlloyRodTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_ENERGETIC_ALLOY.getFluid(), 90))
                .unlockedBy("has_item", has(energeticAlloyRodTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(energeticAlloyRodTag))), "casting:solidifier/energetic_alloy/rod");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.PLATE_MOLD, 1), new SizedIngredient(Ingredient.of(energeticAlloyPlateTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_ENERGETIC_ALLOY.getFluid(), 90))
                .unlockedBy("has_item", has(energeticAlloyPlateTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(energeticAlloyPlateTag))), "casting:solidifier/energetic_alloy/plate");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.DUST_MOLD, 1), new SizedIngredient(Ingredient.of(energeticAlloyDustTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_ENERGETIC_ALLOY.getFluid(), 90))
                .unlockedBy("has_item", has(energeticAlloyDustTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(energeticAlloyDustTag))), "casting:solidifier/energetic_alloy/dust");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.BALL_MOLD, 1), new SizedIngredient(Ingredient.of(BuiltInRegistries.ITEM.get(ResourceLocation.parse("enderio:energetic_alloy_grinding_ball")).getDefaultInstance()), 24),
                        new FluidStack(CastingFluids.MOLTEN_ENERGETIC_ALLOY.getFluid(), 450))
                .unlockedBy("has_item", has(energeticAlloyIngotTag)).save(consumer.withConditions(new ModLoadedCondition("enderio")), "casting:solidifier/energetic_alloy/ball");

        //Vibrant Alloy Processing

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(vibrantAlloyBlockTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_VIBRANT_ALLOY.getFluid(), 810), 1000)
                .unlockedBy("has_item", has(vibrantAlloyBlockTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(vibrantAlloyBlockTag))), "casting:melting/vibrant_alloy/from_block");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(vibrantAlloyIngotTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_VIBRANT_ALLOY.getFluid(), 90), 1000)
                .unlockedBy("has_item", has(vibrantAlloyIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(vibrantAlloyIngotTag))), "casting:melting/vibrant_alloy/from_ingot");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(vibrantAlloyDustTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_VIBRANT_ALLOY.getFluid(), 90), 1000)
                .unlockedBy("has_item", has(vibrantAlloyDustTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(vibrantAlloyDustTag))), "casting:melting/vibrant_alloy/from_dust");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(vibrantAlloyNuggetTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_VIBRANT_ALLOY.getFluid(), 10), 1000)
                .unlockedBy("has_item", has(vibrantAlloyNuggetTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(vibrantAlloyNuggetTag))), "casting:melting/vibrant_alloy/from_nugget");

        MixingRecipeBuilder.MixingRecipesBuilder(
                        new FluidStack(CastingFluids.MOLTEN_ENDER.getFluid(), 80),
                        new FluidStack(CastingFluids.MOLTEN_ENERGETIC_ALLOY.getFluid(), 90),
                        null,
                        null,
                        null,
                        null,
                        new FluidStack(CastingFluids.MOLTEN_VIBRANT_ALLOY.getFluid(), 90))
                .unlockedBy("has_item", has(vibrantAlloyIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(vibrantAlloyIngotTag))), "casting:mixing/vibrant_alloy");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.BLOCK_MOLD, 1), new SizedIngredient(Ingredient.of(vibrantAlloyBlockTag), 1),
                new FluidStack(CastingFluids.MOLTEN_VIBRANT_ALLOY.getFluid(), 810)).unlockedBy("has_item", has(vibrantAlloyBlockTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(vibrantAlloyBlockTag))), "casting:solidifier/vibrant_alloy/block");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.INGOT_MOLD, 1), new SizedIngredient(Ingredient.of(vibrantAlloyIngotTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_VIBRANT_ALLOY.getFluid(), 90))
                .unlockedBy("has_item", has(vibrantAlloyIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(vibrantAlloyIngotTag))), "casting:solidifier/vibrant_alloy/ingot");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.NUGGET_MOLD, 1), new SizedIngredient(Ingredient.of(vibrantAlloyNuggetTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_VIBRANT_ALLOY.getFluid(), 10))
                .unlockedBy("has_item", has(vibrantAlloyNuggetTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(vibrantAlloyNuggetTag))), "casting:solidifier/vibrant_alloy/nugget");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.GEAR_MOLD, 1), new SizedIngredient(Ingredient.of(vibrantAlloyGearTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_VIBRANT_ALLOY.getFluid(), 360))
                .unlockedBy("has_item", has(vibrantAlloyGearTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(vibrantAlloyGearTag))), "casting:solidifier/vibrant_alloy/gear");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.ROD_MOLD, 1), new SizedIngredient(Ingredient.of(vibrantAlloyRodTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_VIBRANT_ALLOY.getFluid(), 90))
                .unlockedBy("has_item", has(vibrantAlloyRodTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(vibrantAlloyRodTag))), "casting:solidifier/vibrant_alloy/rod");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.PLATE_MOLD, 1), new SizedIngredient(Ingredient.of(vibrantAlloyPlateTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_VIBRANT_ALLOY.getFluid(), 90))
                .unlockedBy("has_item", has(vibrantAlloyPlateTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(vibrantAlloyPlateTag))), "casting:solidifier/vibrant_alloy/plate");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.DUST_MOLD, 1), new SizedIngredient(Ingredient.of(vibrantAlloyDustTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_VIBRANT_ALLOY.getFluid(), 90))
                .unlockedBy("has_item", has(vibrantAlloyDustTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(vibrantAlloyDustTag))), "casting:solidifier/vibrant_alloy/dust");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.BALL_MOLD, 1), new SizedIngredient(Ingredient.of(BuiltInRegistries.ITEM.get(ResourceLocation.parse("enderio:vibrant_alloy_grinding_ball")).getDefaultInstance()), 24),
                        new FluidStack(CastingFluids.MOLTEN_VIBRANT_ALLOY.getFluid(), 450))
                .unlockedBy("has_item", has(vibrantAlloyIngotTag)).save(consumer.withConditions(new ModLoadedCondition("enderio")), "casting:solidifier/vibrant_alloy/ball");

        //Pulsating Alloy Processing

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(pulsatingAlloyBlockTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_PULSATING_ALLOY.getFluid(), 810), 1000)
                .unlockedBy("has_item", has(pulsatingAlloyBlockTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(pulsatingAlloyBlockTag))), "casting:melting/pulsating_alloy/from_block");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(pulsatingAlloyIngotTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_PULSATING_ALLOY.getFluid(), 90), 1000)
                .unlockedBy("has_item", has(pulsatingAlloyIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(pulsatingAlloyIngotTag))), "casting:melting/pulsating_alloy/from_ingot");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(pulsatingAlloyDustTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_PULSATING_ALLOY.getFluid(), 90), 1000)
                .unlockedBy("has_item", has(pulsatingAlloyDustTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(pulsatingAlloyDustTag))), "casting:melting/pulsating_alloy/from_dust");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(pulsatingAlloyNuggetTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_PULSATING_ALLOY.getFluid(), 10), 1000)
                .unlockedBy("has_item", has(pulsatingAlloyNuggetTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(pulsatingAlloyNuggetTag))), "casting:melting/pulsating_alloy/from_nugget");

        MixingRecipeBuilder.MixingRecipesBuilder(
                        new FluidStack(CastingFluids.MOLTEN_ENDER.getFluid(), 80),
                        new FluidStack(CastingFluids.MOLTEN_IRON.getFluid(), 90),
                        null,
                        null,
                        null,
                        null,
                        new FluidStack(CastingFluids.MOLTEN_PULSATING_ALLOY.getFluid(), 90))
                .unlockedBy("has_item", has(pulsatingAlloyIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(pulsatingAlloyIngotTag))), "casting:mixing/pulsating_alloy");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.BLOCK_MOLD, 1), new SizedIngredient(Ingredient.of(pulsatingAlloyBlockTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_PULSATING_ALLOY.getFluid(), 810))
                .unlockedBy("has_item", has(pulsatingAlloyBlockTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(pulsatingAlloyBlockTag))), "casting:solidifier/pulsating_alloy/block");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.INGOT_MOLD, 1), new SizedIngredient(Ingredient.of(pulsatingAlloyIngotTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_PULSATING_ALLOY.getFluid(), 90))
                .unlockedBy("has_item", has(pulsatingAlloyIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(pulsatingAlloyIngotTag))), "casting:solidifier/pulsating_alloy/ingot");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.NUGGET_MOLD, 1), new SizedIngredient(Ingredient.of(pulsatingAlloyNuggetTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_PULSATING_ALLOY.getFluid(), 10))
                .unlockedBy("has_item", has(pulsatingAlloyNuggetTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(pulsatingAlloyNuggetTag))), "casting:solidifier/pulsating_alloy/nugget");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.GEAR_MOLD, 1), new SizedIngredient(Ingredient.of(pulsatingAlloyGearTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_PULSATING_ALLOY.getFluid(), 360))
                .unlockedBy("has_item", has(pulsatingAlloyGearTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(pulsatingAlloyGearTag))), "casting:solidifier/pulsating_alloy/gear");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.ROD_MOLD, 1), new SizedIngredient(Ingredient.of(pulsatingAlloyRodTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_PULSATING_ALLOY.getFluid(), 90))
                .unlockedBy("has_item", has(pulsatingAlloyRodTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(pulsatingAlloyRodTag))), "casting:solidifier/pulsating_alloy/rod");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.PLATE_MOLD, 1), new SizedIngredient(Ingredient.of(pulsatingAlloyPlateTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_PULSATING_ALLOY.getFluid(), 90))
                .unlockedBy("has_item", has(pulsatingAlloyPlateTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(pulsatingAlloyPlateTag))), "casting:solidifier/pulsating_alloy/plate");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.DUST_MOLD, 1), new SizedIngredient(Ingredient.of(pulsatingAlloyDustTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_PULSATING_ALLOY.getFluid(), 90))
                .unlockedBy("has_item", has(pulsatingAlloyDustTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(pulsatingAlloyDustTag))), "casting:solidifier/pulsating_alloy/dust");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.BALL_MOLD, 1), new SizedIngredient(Ingredient.of(BuiltInRegistries.ITEM.get(ResourceLocation.parse("enderio:pulsating_alloy_grinding_ball")).getDefaultInstance()), 24),
                        new FluidStack(CastingFluids.MOLTEN_PULSATING_ALLOY.getFluid(), 450))
                .unlockedBy("has_item", has(pulsatingAlloyIngotTag)).save(consumer.withConditions(new ModLoadedCondition("enderio")), "casting:solidifier/pulsating_alloy/ball");

        //Soularium Processing

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(soulariumBlockTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_SOULARIUM.getFluid(), 810), 1000)
                .unlockedBy("has_item", has(soulariumBlockTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(soulariumBlockTag))), "casting:melting/soularium/from_block");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(soulariumIngotTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_SOULARIUM.getFluid(), 90), 1000)
                .unlockedBy("has_item", has(soulariumIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(soulariumIngotTag))), "casting:melting/soularium/from_ingot");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(soulariumDustTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_SOULARIUM.getFluid(), 90), 1000)
                .unlockedBy("has_item", has(soulariumDustTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(soulariumDustTag))), "casting:melting/soularium/from_dust");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(soulariumNuggetTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_SOULARIUM.getFluid(), 10), 1000)
                .unlockedBy("has_item", has(soulariumNuggetTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(soulariumNuggetTag))), "casting:melting/soularium/from_nugget");

        MixingRecipeBuilder.MixingRecipesBuilder(
                        new FluidStack(CastingFluids.MOLTEN_GOLD.getFluid(), 90),
                        new FluidStack(CastingFluids.MOLTEN_SOUL.getFluid(), 1000),
                        null,
                        null,
                        null,
                        null,
                        new FluidStack(CastingFluids.MOLTEN_SOULARIUM.getFluid(), 90))
                .unlockedBy("has_item", has(soulariumIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(soulariumIngotTag))), "casting:mixing/soularium");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.BLOCK_MOLD, 1), new SizedIngredient(Ingredient.of(soulariumBlockTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_SOULARIUM.getFluid(), 810))
                .unlockedBy("has_item", has(soulariumBlockTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(soulariumBlockTag))), "casting:solidifier/soularium/block");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.INGOT_MOLD, 1), new SizedIngredient(Ingredient.of(soulariumIngotTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_SOULARIUM.getFluid(), 90))
                .unlockedBy("has_item", has(soulariumIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(soulariumIngotTag))), "casting:solidifier/soularium/ingot");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.NUGGET_MOLD, 1), new SizedIngredient(Ingredient.of(soulariumNuggetTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_SOULARIUM.getFluid(), 10))
                .unlockedBy("has_item", has(soulariumNuggetTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(soulariumNuggetTag))), "casting:solidifier/soularium/nugget");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.GEAR_MOLD, 1), new SizedIngredient(Ingredient.of(soulariumGearTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_SOULARIUM.getFluid(), 360))
                .unlockedBy("has_item", has(soulariumGearTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(soulariumGearTag))), "casting:solidifier/soularium/gear");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.ROD_MOLD, 1), new SizedIngredient(Ingredient.of(soulariumRodTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_SOULARIUM.getFluid(), 90))
                .unlockedBy("has_item", has(soulariumRodTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(soulariumRodTag))), "casting:solidifier/soularium/rod");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.PLATE_MOLD, 1), new SizedIngredient(Ingredient.of(soulariumPlateTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_SOULARIUM.getFluid(), 90))
                .unlockedBy("has_item", has(soulariumPlateTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(soulariumPlateTag))), "casting:solidifier/soularium/plate");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.DUST_MOLD, 1), new SizedIngredient(Ingredient.of(soulariumDustTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_SOULARIUM.getFluid(), 90))
                .unlockedBy("has_item", has(soulariumDustTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(soulariumDustTag))), "casting:solidifier/soularium/dust");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.BALL_MOLD, 1), new SizedIngredient(Ingredient.of(BuiltInRegistries.ITEM.get(ResourceLocation.parse("enderio:soularium_grinding_ball")).getDefaultInstance()), 24),
                        new FluidStack(CastingFluids.MOLTEN_SOULARIUM.getFluid(), 450))
                .unlockedBy("has_item", has(soulariumIngotTag)).save(consumer.withConditions(new ModLoadedCondition("enderio")), "casting:solidifier/soularium/ball");

        //Dark Steel Processing

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(darkSteelBlockTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_DARK_STEEL.getFluid(), 810), 1000)
                .unlockedBy("has_item", has(darkSteelBlockTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(darkSteelBlockTag))), "casting:melting/dark_steel/from_block");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(darkSteelIngotTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_DARK_STEEL.getFluid(), 90), 1000)
                .unlockedBy("has_item", has(darkSteelIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(darkSteelIngotTag))), "casting:melting/dark_steel/from_ingot");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(darkSteelDustTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_DARK_STEEL.getFluid(), 90), 1000)
                .unlockedBy("has_item", has(darkSteelDustTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(darkSteelDustTag))), "casting:melting/dark_steel/from_dust");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(darkSteelNuggetTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_DARK_STEEL.getFluid(), 10), 1000)
                .unlockedBy("has_item", has(darkSteelNuggetTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(darkSteelNuggetTag))), "casting:melting/dark_steel/from_nugget");

        MixingRecipeBuilder.MixingRecipesBuilder(
                        new FluidStack(CastingFluids.MOLTEN_IRON.getFluid(), 90),
                        new FluidStack(CastingFluids.MOLTEN_COAL.getFluid(), 160),
                        new FluidStack(CastingFluids.MOLTEN_OBSIDIAN.getFluid(), 1000),
                        null,
                        null,
                        null,
                        new FluidStack(CastingFluids.MOLTEN_DARK_STEEL.getFluid(), 90))
                .unlockedBy("has_item", has(darkSteelIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(darkSteelIngotTag))), "casting:mixing/dark_steel");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.BLOCK_MOLD, 1), new SizedIngredient(Ingredient.of(darkSteelBlockTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_DARK_STEEL.getFluid(), 810))
                .unlockedBy("has_item", has(darkSteelBlockTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(darkSteelBlockTag))), "casting:solidifier/dark_steel/block");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.INGOT_MOLD, 1), new SizedIngredient(Ingredient.of(darkSteelIngotTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_DARK_STEEL.getFluid(), 90))
                .unlockedBy("has_item", has(darkSteelIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(darkSteelIngotTag))), "casting:solidifier/dark_steel/ingot");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.NUGGET_MOLD, 1), new SizedIngredient(Ingredient.of(darkSteelNuggetTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_DARK_STEEL.getFluid(), 10))
                .unlockedBy("has_item", has(darkSteelNuggetTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(darkSteelNuggetTag))), "casting:solidifier/dark_steel/nugget");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.GEAR_MOLD, 1), new SizedIngredient(Ingredient.of(darkSteelGearTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_DARK_STEEL.getFluid(), 360))
                .unlockedBy("has_item", has(darkSteelGearTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(darkSteelGearTag))), "casting:solidifier/dark_steel/gear");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.ROD_MOLD, 1), new SizedIngredient(Ingredient.of(darkSteelRodTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_DARK_STEEL.getFluid(), 90))
                .unlockedBy("has_item", has(darkSteelRodTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(darkSteelRodTag))), "casting:solidifier/dark_steel/rod");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.PLATE_MOLD, 1), new SizedIngredient(Ingredient.of(darkSteelPlateTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_DARK_STEEL.getFluid(), 90))
                .unlockedBy("has_item", has(darkSteelPlateTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(darkSteelPlateTag))), "casting:solidifier/dark_steel/plate");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.DUST_MOLD, 1), new SizedIngredient(Ingredient.of(darkSteelDustTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_DARK_STEEL.getFluid(), 90))
                .unlockedBy("has_item", has(darkSteelDustTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(darkSteelDustTag))), "casting:solidifier/dark_steel/dust");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.BALL_MOLD, 1), new SizedIngredient(Ingredient.of(BuiltInRegistries.ITEM.get(ResourceLocation.parse("enderio:dark_steel_grinding_ball")).getDefaultInstance()), 24),
                        new FluidStack(CastingFluids.MOLTEN_DARK_STEEL.getFluid(), 450))
                .unlockedBy("has_item", has(darkSteelIngotTag)).save(consumer.withConditions(new ModLoadedCondition("enderio")), "casting:solidifier/dark_steel/ball");

        //End Steel Processing

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(endSteelBlockTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_END_STEEL.getFluid(), 810), 1000)
                .unlockedBy("has_item", has(endSteelBlockTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(endSteelBlockTag))), "casting:melting/end_steel/from_block");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(endSteelIngotTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_END_STEEL.getFluid(), 90), 1000)
                .unlockedBy("has_item", has(endSteelIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(endSteelIngotTag))), "casting:melting/end_steel/from_ingot");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(endSteelDustTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_END_STEEL.getFluid(), 90), 1000)
                .unlockedBy("has_item", has(endSteelDustTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(endSteelDustTag))), "casting:melting/end_steel/from_dust");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(endSteelNuggetTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_END_STEEL.getFluid(), 10), 1000)
                .unlockedBy("has_item", has(endSteelNuggetTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(endSteelNuggetTag))), "casting:melting/end_steel/from_nugget");

        MixingRecipeBuilder.MixingRecipesBuilder(
                        new FluidStack(CastingFluids.MOLTEN_END_STONE.getFluid(), 1000),
                        new FluidStack(CastingFluids.MOLTEN_DARK_STEEL.getFluid(), 90),
                        new FluidStack(CastingFluids.MOLTEN_OSMIUM.getFluid(), 1000),
                        null,
                        null,
                        null,
                        new FluidStack(CastingFluids.MOLTEN_END_STEEL.getFluid(), 90))
                .unlockedBy("has_item", has(endSteelIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(endSteelIngotTag))), "casting:mixing/end_steel");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.BLOCK_MOLD, 1), new SizedIngredient(Ingredient.of(endSteelBlockTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_END_STEEL.getFluid(), 810))
                .unlockedBy("has_item", has(endSteelBlockTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(endSteelBlockTag))), "casting:solidifier/end_steel/block");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.INGOT_MOLD, 1), new SizedIngredient(Ingredient.of(endSteelIngotTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_END_STEEL.getFluid(), 90))
                .unlockedBy("has_item", has(endSteelIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(endSteelIngotTag)), new NotCondition(new TagEmptyCondition(endSteelIngotTag))), "casting:solidifier/end_steel/ingot");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.NUGGET_MOLD, 1), new SizedIngredient(Ingredient.of(endSteelNuggetTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_END_STEEL.getFluid(), 10))
                .unlockedBy("has_item", has(endSteelNuggetTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(endSteelNuggetTag)), new NotCondition(new TagEmptyCondition(endSteelNuggetTag))), "casting:solidifier/end_steel/nugget");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.GEAR_MOLD, 1), new SizedIngredient(Ingredient.of(endSteelGearTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_END_STEEL.getFluid(), 360))
                .unlockedBy("has_item", has(endSteelGearTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(endSteelGearTag)), new NotCondition(new TagEmptyCondition(endSteelGearTag))), "casting:solidifier/end_steel/gear");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.ROD_MOLD, 1), new SizedIngredient(Ingredient.of(endSteelRodTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_END_STEEL.getFluid(), 90))
                .unlockedBy("has_item", has(endSteelRodTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(endSteelRodTag)), new NotCondition(new TagEmptyCondition(endSteelRodTag))), "casting:solidifier/end_steel/rod");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.PLATE_MOLD, 1), new SizedIngredient(Ingredient.of(endSteelPlateTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_END_STEEL.getFluid(), 90))
                .unlockedBy("has_item", has(endSteelPlateTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(endSteelPlateTag)), new NotCondition(new TagEmptyCondition(endSteelPlateTag))), "casting:solidifier/end_steel/plate");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.DUST_MOLD, 1), new SizedIngredient(Ingredient.of(endSteelDustTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_END_STEEL.getFluid(), 90))
                .unlockedBy("has_item", has(endSteelDustTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(endSteelDustTag)), new NotCondition(new TagEmptyCondition(endSteelDustTag))), "casting:solidifier/end_steel/dust");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.BALL_MOLD, 1), new SizedIngredient(Ingredient.of(BuiltInRegistries.ITEM.get(ResourceLocation.parse("enderio:end_steel_grinding_ball")).getDefaultInstance()), 24),
                        new FluidStack(CastingFluids.MOLTEN_END_STEEL.getFluid(), 450))
                .unlockedBy("has_item", has(endSteelIngotTag)).save(consumer.withConditions(new ModLoadedCondition("enderio")), "casting:solidifier/end_steel/ball");

        //Redstone Alloy Processing

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(redstoneAlloyBlockTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_REDSTONE_ALLOY.getFluid(), 810), 1000)
                .unlockedBy("has_item", has(redstoneAlloyBlockTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(redstoneAlloyBlockTag))), "casting:melting/redstone_alloy/from_block");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(redstoneAlloyIngotTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_REDSTONE_ALLOY.getFluid(), 90), 1000)
                .unlockedBy("has_item", has(redstoneAlloyIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(redstoneAlloyIngotTag))), "casting:melting/redstone_alloy/from_ingot");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(redstoneAlloyDustTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_REDSTONE_ALLOY.getFluid(), 90), 1000)
                .unlockedBy("has_item", has(redstoneAlloyDustTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(redstoneAlloyDustTag))), "casting:melting/redstone_alloy/from_dust");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(redstoneAlloyNuggetTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_REDSTONE_ALLOY.getFluid(), 10), 1000)
                .unlockedBy("has_item", has(redstoneAlloyNuggetTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(redstoneAlloyNuggetTag))), "casting:melting/redstone_alloy/from_nugget");

        MixingRecipeBuilder.MixingRecipesBuilder(
                        new FluidStack(CastingFluids.MOLTEN_SILICON.getFluid(), 250),
                        new FluidStack(CastingFluids.MOLTEN_REDSTONE.getFluid(), 90),
                        null,
                        null,
                        null,
                        null,
                        new FluidStack(CastingFluids.MOLTEN_REDSTONE_ALLOY.getFluid(), 90))
                .unlockedBy("has_item", has(redstoneAlloyIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(redstoneAlloyIngotTag))), "casting:mixing/redstone_alloy");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.BLOCK_MOLD, 1), new SizedIngredient(Ingredient.of(redstoneAlloyBlockTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_REDSTONE_ALLOY.getFluid(), 810))
                .unlockedBy("has_item", has(redstoneAlloyBlockTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(redstoneAlloyBlockTag))), "casting:solidifier/redstone_alloy/block");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.INGOT_MOLD, 1), new SizedIngredient(Ingredient.of(redstoneAlloyIngotTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_REDSTONE_ALLOY.getFluid(), 90))
                .unlockedBy("has_item", has(redstoneAlloyIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(redstoneAlloyIngotTag)), new NotCondition(new TagEmptyCondition(redstoneAlloyIngotTag))), "casting:solidifier/redstone_alloy/ingot");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.NUGGET_MOLD, 1), new SizedIngredient(Ingredient.of(redstoneAlloyNuggetTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_REDSTONE_ALLOY.getFluid(), 10))
                .unlockedBy("has_item", has(redstoneAlloyNuggetTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(redstoneAlloyNuggetTag)), new NotCondition(new TagEmptyCondition(redstoneAlloyNuggetTag))), "casting:solidifier/redstone_alloy/nugget");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.GEAR_MOLD, 1), new SizedIngredient(Ingredient.of(redstoneAlloyGearTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_REDSTONE_ALLOY.getFluid(), 360))
                .unlockedBy("has_item", has(redstoneAlloyGearTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(redstoneAlloyGearTag)), new NotCondition(new TagEmptyCondition(redstoneAlloyGearTag))), "casting:solidifier/redstone_alloy/gear");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.ROD_MOLD, 1), new SizedIngredient(Ingredient.of(redstoneAlloyRodTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_REDSTONE_ALLOY.getFluid(), 90))
                .unlockedBy("has_item", has(redstoneAlloyRodTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(redstoneAlloyRodTag)), new NotCondition(new TagEmptyCondition(redstoneAlloyRodTag))), "casting:solidifier/redstone_alloy/rod");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.PLATE_MOLD, 1), new SizedIngredient(Ingredient.of(redstoneAlloyPlateTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_REDSTONE_ALLOY.getFluid(), 90))
                .unlockedBy("has_item", has(redstoneAlloyPlateTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(redstoneAlloyPlateTag)), new NotCondition(new TagEmptyCondition(redstoneAlloyPlateTag))), "casting:solidifier/redstone_alloy/plate");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.DUST_MOLD, 1), new SizedIngredient(Ingredient.of(redstoneAlloyDustTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_REDSTONE_ALLOY.getFluid(), 90))
                .unlockedBy("has_item", has(redstoneAlloyDustTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(redstoneAlloyDustTag)), new NotCondition(new TagEmptyCondition(redstoneAlloyDustTag))), "casting:solidifier/redstone_alloy/dust");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.BALL_MOLD, 1), new SizedIngredient(Ingredient.of(BuiltInRegistries.ITEM.get(ResourceLocation.parse("enderio:redstone_alloy_grinding_ball")).getDefaultInstance()), 24),
                        new FluidStack(CastingFluids.MOLTEN_REDSTONE_ALLOY.getFluid(), 450))
                .unlockedBy("has_item", has(redstoneAlloyIngotTag)).save(consumer.withConditions(new ModLoadedCondition("enderio")), "casting:solidifier/redstone_alloy/ball");

        //Copper Alloy Processing

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(copperAlloyBlockTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_COPPER_ALLOY.getFluid(), 810), 1000)
                .unlockedBy("has_item", has(copperAlloyBlockTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(copperAlloyBlockTag))), "casting:melting/copper_alloy/from_block");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(copperAlloyIngotTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_COPPER_ALLOY.getFluid(), 90), 1000)
                .unlockedBy("has_item", has(copperAlloyIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(copperAlloyIngotTag))), "casting:melting/copper_alloy/from_ingot");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(copperAlloyDustTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_COPPER_ALLOY.getFluid(), 90), 1000)
                .unlockedBy("has_item", has(copperAlloyDustTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(copperAlloyDustTag))), "casting:melting/copper_alloy/from_dust");

        MeltingRecipeBuilder.MeltingRecipesBuilder(new SizedIngredient(Ingredient.of(copperAlloyNuggetTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_COPPER_ALLOY.getFluid(), 10), 1000)
                .unlockedBy("has_item", has(copperAlloyNuggetTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(copperAlloyNuggetTag))), "casting:melting/copper_alloy/from_nugget");

        MixingRecipeBuilder.MixingRecipesBuilder(
                        new FluidStack(CastingFluids.MOLTEN_SILICON.getFluid(), 250),
                        new FluidStack(CastingFluids.MOLTEN_COPPER.getFluid(), 90),
                        null,
                        null,
                        null,
                        null,
                        new FluidStack(CastingFluids.MOLTEN_COPPER_ALLOY.getFluid(), 90))
                .unlockedBy("has_item", has(copperAlloyIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(copperAlloyIngotTag))), "casting:mixing/copper_alloy");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.BLOCK_MOLD, 1), new SizedIngredient(Ingredient.of(copperAlloyBlockTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_COPPER_ALLOY.getFluid(), 810))
                .unlockedBy("has_item", has(copperAlloyBlockTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(copperAlloyBlockTag))), "casting:solidifier/copper_alloy/block");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.INGOT_MOLD, 1), new SizedIngredient(Ingredient.of(copperAlloyIngotTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_COPPER_ALLOY.getFluid(), 90))
                .unlockedBy("has_item", has(copperAlloyIngotTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(copperAlloyIngotTag)), new NotCondition(new TagEmptyCondition(copperAlloyIngotTag))), "casting:solidifier/copper_alloy/ingot");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.NUGGET_MOLD, 1), new SizedIngredient(Ingredient.of(copperAlloyNuggetTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_COPPER_ALLOY.getFluid(), 10))
                .unlockedBy("has_item", has(copperAlloyNuggetTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(copperAlloyNuggetTag)), new NotCondition(new TagEmptyCondition(copperAlloyNuggetTag))), "casting:solidifier/copper_alloy/nugget");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.GEAR_MOLD, 1), new SizedIngredient(Ingredient.of(copperAlloyGearTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_COPPER_ALLOY.getFluid(), 360))
                .unlockedBy("has_item", has(copperAlloyGearTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(copperAlloyGearTag)), new NotCondition(new TagEmptyCondition(copperAlloyGearTag))), "casting:solidifier/copper_alloy/gear");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.ROD_MOLD, 1), new SizedIngredient(Ingredient.of(copperAlloyRodTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_COPPER_ALLOY.getFluid(), 90))
                .unlockedBy("has_item", has(copperAlloyRodTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(copperAlloyRodTag)), new NotCondition(new TagEmptyCondition(copperAlloyRodTag))), "casting:solidifier/copper_alloy/rod");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.PLATE_MOLD, 1), new SizedIngredient(Ingredient.of(copperAlloyPlateTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_COPPER_ALLOY.getFluid(), 90))
                .unlockedBy("has_item", has(copperAlloyPlateTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(copperAlloyPlateTag)), new NotCondition(new TagEmptyCondition(copperAlloyPlateTag))), "casting:solidifier/copper_alloy/plate");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.DUST_MOLD, 1), new SizedIngredient(Ingredient.of(copperAlloyDustTag), 1),
                        new FluidStack(CastingFluids.MOLTEN_COPPER_ALLOY.getFluid(), 90))
                .unlockedBy("has_item", has(copperAlloyDustTag)).save(consumer.withConditions(new NotCondition(new TagEmptyCondition(copperAlloyDustTag)), new NotCondition(new TagEmptyCondition(copperAlloyDustTag))), "casting:solidifier/copper_alloy/dust");

        SolidifierRecipeBuilder.SolidifierRecipesBuilder(SizedIngredient.of(CastingTags.Items.BALL_MOLD, 1), new SizedIngredient(Ingredient.of(BuiltInRegistries.ITEM.get(ResourceLocation.parse("enderio:copper_alloy_grinding_ball")).getDefaultInstance()), 24),
                        new FluidStack(CastingFluids.MOLTEN_COPPER_ALLOY.getFluid(), 450))
                .unlockedBy("has_item", has(copperAlloyIngotTag)).save(consumer.withConditions(new ModLoadedCondition("enderio")), "casting:solidifier/copper_alloy/ball");


    }

    public void toolModifierRecipes(RecipeOutput consumer, SizedIngredient ingredient, FluidStack fluid, String effect) {

        String recipeFrom = "";

        if (ingredient == null) {
            recipeFrom = "equipment_modifier/" + effect + "_from_fluid";

        }
        if (fluid == null) {
            recipeFrom = "equipment_modifier/" + effect + "_from_item";
        }

        if (fluid != null && ingredient != null) {
            recipeFrom = "equipment_modifier/" + effect + "_from_fluid_and_item";
        }

        ToolModifierRecipeBuilder.ToolModifierRecipesBuilder(
                        ingredient,
                        fluid,
                        effect)
                .unlockedBy("has_item", has(ModBlocks.EQUIPMENT_MODIFIER.get()))
                .save(consumer, ResourceLocation.fromNamespaceAndPath(Casting.MOD_ID, recipeFrom));
    }
}
