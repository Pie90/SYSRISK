library(reshape)
library(dplyr)
library(tibble)
library(corpcor)
library(ggplot2)
library(RColorBrewer)


# Upload full dataset without FAOSTAT Flags --> Encoding WINDOWS-1252 (Turkish) to recognize ô character
    dataset_no_flag <- read.csv(file = "faostat_data/Bulk_downloads/Trade_DetailedTradeMatrix_E_All_Data_NOFLAG.csv", header = T, fileEncoding = "WINDOWS-1252")

# Delete unnecessary years to keep range 2013-2016 and compute mean value
    dataset_no_flag_2013_2016 <- dataset_no_flag[,-c(10:36)]
    dataset_no_flag_2013_2016$mean_2013_2016 <- rowMeans(dataset_no_flag_2013_2016[c("Y2013","Y2014","Y2015", "Y2016")], na.rm=T) 

# Load reference datasets for the products (items) names and list of countrues
    selected_items_list <- read.csv(file = "Correction_procedure_calculation_based_on_kcal/List_items.csv", header = T)
    selected_countries_list <- read.csv(file = "Countries datasets/full_list_163_selected_countries.csv", header = T, fileEncoding = "WINDOWS-1252", dec = ",")



# ------------ IMPORT FAOSTAT BILATERAL TRADE DATA --------------------------- 

    # - 1 - Crop  products

        selected_items_list_crops <- subset(selected_items_list, Type == "Crop")
          
        # create empty array for final data storage --> No. countries == 163 --> nrow and ncol; No. items crops == 114 (nLayers)
        dataset_import_array_cast_complete_cases <-  array(dim = c(163,163,length(selected_items_list_crops$Item)),
                                                           dimnames=list(Reporter_country = paste0(selected_countries_list$country, "_Reporter"),
                                                                         Partner_country= paste0(selected_countries_list$country, "_Partner"),
                                                                         Crop_Item = selected_items_list_crops$Item))     


# Here starts the procedure to imput the data into the final matricial dataset.

    # Step 1: subset the original dataset to select only: a) Each single Item, b) The quantity imported by the Reporter Countries, c) the chosen reference year
        for (i in 1:length(selected_items_list_crops$Item)) {
          exchages_selected_item <- subset(dataset_no_flag_2013_2016, Item == as.vector(selected_items_list_crops$Item)[i]) # Subset for the item i
          import_selected_item <- subset(exchages_selected_item, Element == "Import Quantity")                              # subset for the Import Quantity
          dataset_import_array_cast <- dcast(import_selected_item, Reporter.Countries ~ Partner.Countries, value.var = "mean_2013_2016")  # Cast (creates a "pivot (matrix) table") the data for the chose year
 
    # Step 2: For  each item, extract the name of each Reporter country contained in the original dataset (rowname) and find the Index of the extracted Reporter country in the final dataset
            for (k in 1:length(dataset_import_array_cast$Reporter.Countries)) {
              row_cast <- as.vector(dataset_import_array_cast$Reporter.Countries[k])                                # Extract the name of the Reporter country at each row
              Index_row <- which(rownames(dataset_import_array_cast_complete_cases)==paste0(row_cast, "_Reporter")) # Match the name with the reference list of countries and find row_index
 
          # Step 3: For each Reporter country (rowname), extract the name of the Partner country (colname) and find the index of the extracted Partner country in the final dataset
                for (t in 2:ncol(dataset_import_array_cast)) {
                    col_cast <- colnames(dataset_import_array_cast)[t]                                                     # Extract the name of the Partner country at each coloums starting from coloumn no.2 (1st country)
                    Index_col <- which(colnames(dataset_import_array_cast_complete_cases)==paste0(col_cast, "_Partner"))   # Match the name with the reference list of countries and find col_index
              
            # Step 3: If both Indexes contains a values (i.e. length==1), then the county in the original dataset correspond to one of the countries listed in the reference file.
              # for these countries, extract the data in the original dataset and assign them to the new position in the final dataset
              # ATT! 

            #This step is skipped if on Index length is !=1, which means that we do not consider that country in the analysis (e.g. because it is a small island, or a minor country)
                      if (length(Index_row)==1 & length(Index_col)==1 & !is.na(dataset_import_array_cast[k,t])) {
                          dataset_import_array_cast_complete_cases[Index_row, Index_col, i] <- dataset_import_array_cast[k,t]
                      }
                }
            }
          }


# - 2 - Livestock products

selected_items_list_livestock <- subset(selected_items_list, Type == "Livestock")

    # create empty array for final data storage --> No. countries == 163 --> nrow and ncol; No. items livestock == 23 (nLayers)
    dataset_import_array_cast_complete_cases <-  array(dim = c(163,163,length(selected_items_list_livestock$Item)),
                                                       dimnames = list(Reporter_country = paste0(selected_countries_list$country, "_Reporter"),
                                                                       Partner_country= paste0(selected_countries_list$country, "_Partner"),
                                                                       Variable = selected_items_list_livestock$Item))     

# Here starts the procedure to imput the data into the final matricial dataset.

    # Step 1: subset the original dataset to select only: a) Each single Item, b) The quantity imported by the Reporter Countries, c) the chosen reference year
    for (i in 1:length(selected_items_list_livestock$Item)) {
      exchages_selected_item <- subset(dataset_no_flag_2013_2016, Item == as.vector(selected_items_list_livestock$Item)[i]) # Subset for the item i
      import_selected_item <- subset(exchages_selected_item, Element == "Import Quantity")                              # subset for the Import Quantity
      dataset_import_array_cast <- dcast(import_selected_item, Reporter.Countries ~ Partner.Countries, value.var = "mean_2013_2016")  # Cast (creates a "pivot (matrix) table") the data for the chose year
  
      # Step 2: For  each item, extract the name of each Reporter country contained in the original dataset (rowname) and find the Index of the extracted Reporter country in the final dataset
      for (k in 1:length(dataset_import_array_cast$Reporter.Countries)) {
        row_cast <- as.vector(dataset_import_array_cast$Reporter.Countries[k])                                # Extract the name of the Reporter country at each row
        Index_row <- which(rownames(dataset_import_array_cast_complete_cases)==paste0(row_cast, "_Reporter")) # Match the name with the reference list of countries and find row_index
    
            # Step 3: For each Reporter country (rowname), extract the name of the Partner country (colname) and find the index of the extracted Partner country in the final dataset
            for (t in 2:ncol(dataset_import_array_cast)) {
              col_cast <- colnames(dataset_import_array_cast)[t]                                                     # Extract the name of the Partner country at each coloums starting from coloumn no.2 (1st country)
              Index_col <- which(colnames(dataset_import_array_cast_complete_cases)==paste0(col_cast, "_Partner"))   # Match the name with the reference list of countries and find col_index
      
          # Step 3: If both Indexes contains a values (i.e. length==1), then the county in the original dataset correspond to one of the countries listed in the reference file.
          # for these countries, extract the data in the original dataset and assign them to the new position in the final dataset
          # ATT! 
      
              #This step is skipped if on Index length is !=1, which means that we do not consider that country in the analysis (e.g. because it is a small island, or a minor country)
              if (length(Index_row)==1 & length(Index_col)==1 & !is.na(dataset_import_array_cast[k,t])) {
                dataset_import_array_cast_complete_cases[Index_row, Index_col, i] <- dataset_import_array_cast[k,t]
              }
            }
          }
        }


# ------------ TRANSFORM BILATERAL TRADE DATA INTO PRIMARY PRODUCTS ------------------------
        
    ## Crop Products
        
        dataset_import_array_cast_complete_cases_crop <- readRDS("dataset_import_array_cast_complete_cases_Y2013_16_crops.rds")
        dataset_import_array_cast_complete_cases_crop[is.na(dataset_import_array_cast_complete_cases_crop)] <- 0
        
        
        dataset_import_array_cast_complete_cases_primary_crops <- array(dim = c(163,163,length(selected_items_list_crops$Item)+2), 
                                                                        dimnames = list(Reporter = paste0(selected_countries_list$country, "_Reporter"),
                                                                                        Partner = paste0(selected_countries_list$country, "_Partner"),
                                                                                        PrimaryCommodity = c(as.character(selected_items_list_crops$Commodity.matching.crop.item.name), "Sugar beet", "Sugar cane")))
        
        for (i in 1:length(selected_items_list_crops$Item)) {
          # Including coefficient kcal_ratio to transform processed items to primary commodities
          dataset_import_array_cast_complete_cases_primary_crops[,,i] <- dataset_import_array_cast_complete_cases_crop[,,i]*selected_items_list_crops$Coef_processed_to_commodity_kcal_based[i]
        }
        
        
        ## ASSIGN SUGARGENERIC TO DOMINANT SOURCING IN PRODUCING COUNTRY (PARTNER)
        

        for (i in 1:length(selected_countries_list$country)) {
          for (j in 1:length(selected_countries_list$country)) {
          if (selected_countries_list$SugarDominantCrop[i]=="Sugar beet") {
            dataset_import_array_cast_complete_cases_primary_crops[j,i,115] <- sum(dataset_import_array_cast_complete_cases_primary_crops[j,i,which(selected_items_list_crops$Commodity.matching.crop.item.name == "SugarGeneric")])
          } else {
            dataset_import_array_cast_complete_cases_primary_crops[j,i,116] <- sum(dataset_import_array_cast_complete_cases_primary_crops[j,i,which(selected_items_list_crops$Commodity.matching.crop.item.name == "SugarGeneric")])
            }
          }
        }

        dataset_import_array_cast_complete_cases_crop[is.na(dataset_import_array_cast_complete_cases_crop)] <- 0
                
       dataset_import_array_cast_complete_cases_primary_crops <- (cast(melt(dataset_import_array_cast_complete_cases_primary_crops), Reporter ~ Partner ~ PrimaryCommodity, fun.aggregate = sum, na.rm=T)) # Aggregate primary products along the 3rd dimension
       dataset_import_array_cast_complete_cases_primary_crops <- dataset_import_array_cast_complete_cases_primary_crops[,,-which(dimnames(dataset_import_array_cast_complete_cases_primary_crops)[[3]]=="SugarGeneric")]
       
               
       # set diagonal (i.e. error data --> self-imported quantities) to zero
       for (i in 1:59) {
         for (k in 1:163) {
           dataset_import_array_cast_complete_cases_primary_crops[k,k,i] <- 0
         }
       }
       
       saveRDS(dataset_import_array_cast_complete_cases_primary_crops, file="Imported_primary_aggregated_crop_commodities_kcal_procedure.rds")
       dataset_import_array_cast_complete_cases_primary_crops <- readRDS("Imported_primary_aggregated_crop_commodities_kcal_procedure.rds")

    ## Livestock Products
        
       dataset_import_array_cast_complete_cases_livestock <- readRDS("dataset_import_array_cast_complete_cases_Y2013_16_livestock.rds")
       dataset_import_array_cast_complete_cases_primary_livestock <- array(dim = c(163,163,length(selected_items_list_livestock$Item)), 
                                                                        dimnames = list(Reporter = paste0(selected_countries_list$country, "_Reporter"),
                                                                                        Partner = paste0(selected_countries_list$country, "_Partner"),
                                                                                        PrimaryCommodity = selected_items_list_livestock$Commodity.matching.crop.item.name))
       
       for (i in 1:length(selected_items_list_livestock$Item)) {
         # Including coefficient εi to transform processed items to primary commodities
         dataset_import_array_cast_complete_cases_primary_livestock[,,i] <- dataset_import_array_cast_complete_cases_livestock[,,i]*selected_items_list_livestock$Coef_processed_to_commodity_kcal_based[i]
       }

       dataset_import_array_cast_complete_cases_primary_livestock <- (cast(melt(dataset_import_array_cast_complete_cases_primary_livestock), Reporter ~ Partner ~ PrimaryCommodity, fun.aggregate = sum, na.rm=T)) # Aggregate primary products along the 3rd dimension
       
       # set diagonal (i.e. error data --> self-imported quantities) to zero
       for (i in 1:5) {
         for (k in 1:163) {
           dataset_import_array_cast_complete_cases_primary_livestock[k,k,i] <- 0
         }
       }
       
# ------------ LOAD CROP YIELDS ---------------------------  

      # Upload full dataset without FAOSTAT Flags --> Encoding WINDOWS-1252 (Turkish) to recognize ô character
      yields_dataset_no_flag <- read.csv(file = "faostat_data/Bulk_downloads/Production_Crops_E_All_Data_NOFLAG.csv", header = T, fileEncoding = "WINDOWS-1252")
      
      # Delete unnecessary years to keep ranfe 2013-2016
      yields_dataset_no_flag_2013_2017 <- yields_dataset_no_flag[,-c(8:59)]

      # create empty array for final data storage --> No. countries == 163 --> nrow and ncol=1; No. items == 120 (nLayers) 
      # --> the real no. items is lower (primary commodities) but I build here a dataset with 120 layers in order to be able to directly divide the yield dataset with the mass traded
      dataset_yield_array_cast_complete_cases <-  array(dim = c(163,1,59),
                                                        dimnames = list(Country = selected_countries_list$country,
                                                                        Variable = "Yield",
                                                                        Crop_primary_items = sort(unique(selected_items_list_crops$Commodity.matching.crop.item.name))[-52]))     

      
      for (i in 1:59) {
        production_reference_item <- subset(yields_dataset_no_flag_2013_2017, Item == as.vector(sort(unique(selected_items_list_crops$Commodity.matching.crop.item.name))[-52])[i])       # Subset for the item i
        yield_reference_item <- subset(production_reference_item, Element == "Yield")                                     # subset for the yiled
        yield_reference_item[8:12] <- yield_reference_item[8:12]/10000                                                    # trasnform hg in tons
      
            # Step 2: For  each item, extract the name of each Reporter country contained in the original dataset (rowname) and find the Index of the extracted Reporter country in the final dataset
           if (length(yield_reference_item$Area)!=0) { 
            for (k in 1:length(yield_reference_item$Area)) {
              row_cast <- as.vector(yield_reference_item$Area[k])                                # Extract the name of the Reporter country at each row
              Index_row <- which(rownames(dataset_yield_array_cast_complete_cases)==row_cast)    # Match the name with the reference list of countries and find row_index
          
                  # Step 3: If row Indexe contains a values (i.e. length==1), then the county in the original dataset correspond to one of the countries listed in the reference file.
                  # for these countries, extract the data in the original dataset and assign them to the new position in the final dataset
                  # ATT! This step is skipped if on Index length is !=1, which means that we do not consider that country in the analysis (e.g. because it is a small island, or a minor country)
                  if (length(Index_row)==1 & !is.na(yield_reference_item$Y2016[k])) {
                    dataset_yield_array_cast_complete_cases[Index_row, 1, i] <- mean(as.numeric(yield_reference_item[k, 9:11]))  # take mean yield of 2013-2017
                  }
            }
           }
        }

      # PROCEDURE TO ADD AVERAGE CONTINENTAL YIELDS WHERE NA DATA (this does not solve problem for crops with missing yield reference)  
      for (i in 1:59) {     # For each commodity reference items i (number of layers)
        
        avg_cont <- melt(dataset_yield_array_cast_complete_cases[,,i])
        avg_cont$region <- selected_countries_list$large_global_region
        mean <- avg_cont %>% group_by(region) %>% summarise(avg=mean(value, na.rm=T))
        
        for (k in 1:length(selected_countries_list$country)) {                                       # for each given country k (rows of each layer)
          if (is.na(dataset_yield_array_cast_complete_cases[k,1,i]) | dataset_yield_array_cast_complete_cases[k,1,i]==0) {                               # if there is no yield information of the given country...
            dataset_yield_array_cast_complete_cases[k,1,i] <- as.numeric(mean[which(sort(unique(selected_countries_list$large_global_region))==selected_countries_list$large_global_region[k]),2])  # Compute average (without NA) of the yeild of the given item and global region and assign it to the coutry selected. 
          }
        }
      }


      # PROCEDURE TO ADD AVERAGE GLOBAL YIELDS WHERE NA DATA is still present (this does not solve problem for crops with missing yield reference)  
      for (i in 1:59) {     # For each commodity reference items i (number of layers)
          avg_cont <- melt(dataset_yield_array_cast_complete_cases[,,i])
          mean <- summarise(avg_cont, avg=mean(value, na.rm=T))
        
        for (k in 1:length(selected_countries_list$country)) {                                       # for each given country k (rows of each layer)
          if (is.na(dataset_yield_array_cast_complete_cases[k,1,i])) {                               # if there is no yield information of the given country...
            dataset_yield_array_cast_complete_cases[k,1,i] <- as.numeric(mean)  # Compute global average (without NA) of the yeild of the given item and assign it to the coutry selected. 
          }
        }
      }
      
            ## ALFALFA yields (from Earthstat, year 2000)      

            library(raster)
            
            alfa <- raster("/home/pbarbieri/Documents/Pietro/Datasets/EarthStat Datasets/Harvested area and yield/single crops/YieldPerHectare/alfalfa_YieldPerHectare.tif")
            DM <- 0.2
            map <- raster("/home/pbarbieri/Documents/Pietro/Datasets/World Shapefiles/Country codes/country_UNI_spatial_corrected_Pietro.nc")

            alfa <- reclassify((alfa * DM), cbind(-Inf, 0,NA))
            a <- zonal(alfa, map, fun='mean', na.rm=T)
            ind <- match(selected_countries_list$CNTRY_CODE, a[,1])
            national_yield <- a[ind,]
            national_yield[national_yield==0] <- NA
            national_yield <- as.data.frame(national_yield)
            national_yield[,3] <- selected_countries_list$country
            
            for (k in 1:length(selected_countries_list$country)) {
                dataset_yield_array_cast_complete_cases[k,1,1] <- national_yield[k,2]
            }
    
              for (k in 1:length(selected_countries_list$country)) {                               # for each given country k (rows of each layer)
              if (is.na(dataset_yield_array_cast_complete_cases[k,1,1])) {                       # if there is no yield information of the given country...
                index_country_continent <- which(selected_countries_list$large_global_region == selected_countries_list$large_global_region[k])      # ...select index of all countries belonging to the same global region than the given country
                dataset_yield_array_cast_complete_cases[k,1,1] <- mean(as.numeric(dataset_yield_array_cast_complete_cases[index_country_continent,1,1]), na.rm=T)  # Compute average (without NA) of the yeild of the given item and global region and assign it to the coutry selected. 
              }
            }
            
                for (k in 1:length(selected_countries_list$country)) {                               # for each given country k (rows of each layer)
                  if (is.na(dataset_yield_array_cast_complete_cases[k,1,1])) {                       # if there is no yield information of the given country...
                    dataset_yield_array_cast_complete_cases[k,1,1] <- mean(as.numeric(dataset_yield_array_cast_complete_cases[,1,1]), na.rm=T)  # Compute global average (without NA) of the yeild of the given item and assign it to the coutry selected. 
                  }
                }
     
 
                       ## FODDER (generic) yields (from Earthstat, year 2000)      ----
            
            fod_1 <- raster("/home/pbarbieri/Documents/Pietro/Datasets/EarthStat Datasets/Harvested area and yield/single crops/YieldPerHectare/alfalfa_YieldPerHectare.tif")
            fod_2 <- raster("/home/pbarbieri/Documents/Pietro/Datasets/EarthStat Datasets/Harvested area and yield/single crops/YieldPerHectare/clover_YieldPerHectare.tif")
            fod_3 <- raster("/home/pbarbieri/Documents/Pietro/Datasets/EarthStat Datasets/Harvested area and yield/single crops/YieldPerHectare/fornes_YieldPerHectare.tif")
            fod_4 <- raster("/home/pbarbieri/Documents/Pietro/Datasets/EarthStat Datasets/Harvested area and yield/single crops/YieldPerHectare/fornes_YieldPerHectare.tif")
            fod_5 <- raster("/home/pbarbieri/Documents/Pietro/Datasets/EarthStat Datasets/Harvested area and yield/single crops/YieldPerHectare/vetch_YieldPerHectare.tif")

            mean_fod <- stack(fod_1, fod_2, fod_3, fod_4, fod_5)
            mean_fod <- reclassify((mean_fod * DM), cbind(-Inf, 0,NA))
            mean_fod <- mean(mean_fod, na.rm=T)

            map <- raster("/home/pbarbieri/Documents/Pietro/Datasets/World Shapefiles/Country codes/country_UNI_spatial_corrected_Pietro.nc")

            b <- zonal(mean_fod, map, fun='mean', na.rm=T)
            ind <- match(selected_countries_list$CNTRY_CODE, b[,1])
            national_yield <- b[ind,]
            national_yield[national_yield==0] <- NA
            national_yield <- as.data.frame(national_yield)
            national_yield[,3] <- selected_countries_list$country


            for (k in 1:length(selected_countries_list$country)) {
              dataset_yield_array_cast_complete_cases[k,1,18] <- national_yield[k,2]
            }

            for (k in 1:length(selected_countries_list$country)) {                               # for each given country k (rows of each layer)
              if (is.na(dataset_yield_array_cast_complete_cases[k,1,18])) {                       # if there is no yield information of the given country...
                index_country_continent <- which(selected_countries_list$large_global_region == selected_countries_list$large_global_region[k])      # ...select index of all countries belonging to the same global region than the given country
                dataset_yield_array_cast_complete_cases[k,1,18] <- mean(as.numeric(dataset_yield_array_cast_complete_cases[index_country_continent,1,18]), na.rm=T)  # Compute average (without NA) of the yeild of the given item and global region and assign it to the coutry selected.
              }
            }

            for (k in 1:length(selected_countries_list$country)) {                               # for each given country k (rows of each layer)
              if (is.na(dataset_yield_array_cast_complete_cases[k,1,18])) {                       # if there is no yield information of the given country...
                dataset_yield_array_cast_complete_cases[k,1,18] <- mean(as.numeric(dataset_yield_array_cast_complete_cases[,1,18]), na.rm=T)  # Compute global average (without NA) of the yeild of the given item and assign it to the coutry selected.
              }
            }

            
# ------------ LOAD P-Fertilizers DATA BY COUNTRY --------------------------- 
            
            # P fertiliser data
            
            P_fertilisers_dataset <- read.csv("Countries datasets/full_list_163_selected_countries_P_fertiliser_use_UPDATED_ROW.csv", header=T, fileEncoding = "WINDOWS-1252", check.names=FALSE)  #in kg P/ha
 
            
            dataset_p_fertilisers_array <-  array(dim = c(163,1,length(unique(selected_items_list_crops$Commodity.matching.crop.item.name)[-52])),
                                                  dimnames = list(Country = selected_countries_list$country,
                                                                  Variable = "P_fert_kg_ha",
                                                                  Primary_crop_item = sort(unique(selected_items_list_crops$Commodity.matching.crop.item.name))[-52]))   
            
            for (i in 1:59) {
              p_fert_reference_item <- P_fertilisers_dataset[, as.vector(selected_items_list_crops$Proxy_fertilisation_rate[which(selected_items_list_crops$Commodity.matching.crop.item.name==as.vector(dimnames(dataset_p_fertilisers_array)[[3]])[i])])[1]]       # select water consumption of item i
              
              # Step 2: For  each item, extract the name of each Reporter country contained in the original dataset (rowname) and find the Index of the extracted Reporter country in the final dataset
              for (k in 1:length(water_blue_green_total$Country)) {
                row_cast <- as.vector(water_blue_green_total$Country[k])                                # Extract the name of the Reporter country at each row
                Index_row <- which(rownames(dataset_p_fertilisers_array)==row_cast)                   # Match the name with the reference list of countries and find row_index
                
                # Step 3: If row Indexe contains a values (i.e. length==1), then the county in the original dataset correspond to one of the countries listed in the reference file.
                # for these countries, extract the data in the original dataset and assign them to the new position in the final dataset
                # ATT! This step is skipped if on Index length is !=1, which means that we do not consider that country in the analysis (e.g. because it is a small island, or a minor country)
                if (length(Index_row)==1) {
                  dataset_p_fertilisers_array[Index_row, 1, i] <- p_fert_reference_item[k]/1000  # transform in tons P / ha
                }
              }
            }
            
                              
            
# ------------ PREPARE LIVESTOCK FEED DATASETS ------------------ 

      # ATT!! FEED INFO share of crops taken from average of HERRERO et al. and Bowmannn Exploring changes in world ruminant production systems
            
      ## 1- Feed efficiency
      
      # create empty array for final data storage --> No. countries == 163 --> nrow and ncol=1; No. unique items == 5 (nLayers)  --> primary commodities 
      dataset_feed_efficiency_array_cast_complete_cases <-  array(dim = c(163,1,length(unique(selected_items_list_livestock$Commodity.matching.crop.item.name))),
                                                                  dimnames = list(Country = selected_countries_list$country,
                                                                                  Variable = "Feed_efficiency_KG_KG_primary",
                                                                                  Feed_item = unique(selected_items_list_livestock$Commodity.matching.crop.item.name)))     

      dataset_feed_efficiency_array_cast_complete_cases[,,which(unique(selected_items_list_livestock$Commodity.matching.crop.item.name)=="Milk, whole fresh cow")] <- selected_countries_list$Feed.efficiency..kg.feed.kg.protein..ï...milk.cow    
      dataset_feed_efficiency_array_cast_complete_cases[,,which(unique(selected_items_list_livestock$Commodity.matching.crop.item.name)=="Meat, cattle")] <- selected_countries_list$Feed.efficiency..kg.feed.kg.protein..ï...cattle.meat    
      dataset_feed_efficiency_array_cast_complete_cases[,,which(unique(selected_items_list_livestock$Commodity.matching.crop.item.name)=="Meat, pig")] <- selected_countries_list$Feed.efficiency..kg.feed.kg.protein..ï...pig.meat
      dataset_feed_efficiency_array_cast_complete_cases[,,which(unique(selected_items_list_livestock$Commodity.matching.crop.item.name)=="Meat, chicken")] <- selected_countries_list$Feed.efficiency..kg.feed.kg.protein..ï...chicken.meat.egg 
      dataset_feed_efficiency_array_cast_complete_cases[,,which(unique(selected_items_list_livestock$Commodity.matching.crop.item.name)=="Eggs, hen, in shell")] <- selected_countries_list$Feed.efficiency..kg.feed.kg.protein..ï...chicken.meat.egg
      
      
      ## 2- Feed provenience (grass, grain)
      
      # create empty array for final data storage --> No. countries == 163 --> nrow and ncol=1; No. unique items == 5 (nLayers) --> primary commodities 
      dataset_feed_provenience_array_cast_complete_cases <-  array(dim = c(163,1,length(unique(selected_items_list_livestock$Commodity.matching.crop.item.name)),2),
                                                                   dimnames = list(Country = selected_countries_list$country,
                                                                                   Variable = "Feed_provenience",
                                                                                   Livestock_Item = unique(selected_items_list_livestock$Commodity.matching.crop.item.name),
                                                                                   Feed_type = c("Grass", "Grain")))


      dataset_feed_provenience_array_cast_complete_cases[,,which(unique(selected_items_list_livestock$Commodity.matching.crop.item.name)=="Milk, whole fresh cow"),1] <- selected_countries_list$X..feed.as.grass.ï...dairy.cows/100   #transfrom in percent 0-1
      dataset_feed_provenience_array_cast_complete_cases[,,which(unique(selected_items_list_livestock$Commodity.matching.crop.item.name)=="Meat, cattle"),1] <- selected_countries_list$X..feed.as.grass.ï...beef/100    
      dataset_feed_provenience_array_cast_complete_cases[,,which(unique(selected_items_list_livestock$Commodity.matching.crop.item.name)=="Meat, pig"),1] <- 0   
      dataset_feed_provenience_array_cast_complete_cases[,,which(unique(selected_items_list_livestock$Commodity.matching.crop.item.name)=="Meat, chicken"),1] <- 0 
      dataset_feed_provenience_array_cast_complete_cases[,,which(unique(selected_items_list_livestock$Commodity.matching.crop.item.name)=="Eggs, hen, in shell"),1] <- 0
      dataset_feed_provenience_array_cast_complete_cases[,,which(unique(selected_items_list_livestock$Commodity.matching.crop.item.name)=="Milk, whole fresh cow"),2] <- selected_countries_list$X..feed.as.grain..ï...dairy.cows/100  
      dataset_feed_provenience_array_cast_complete_cases[,,which(unique(selected_items_list_livestock$Commodity.matching.crop.item.name)=="Meat, cattle"),2] <- selected_countries_list$X..feed.as.grain..ï...beef/100   
      dataset_feed_provenience_array_cast_complete_cases[,,which(unique(selected_items_list_livestock$Commodity.matching.crop.item.name)=="Meat, pig"),2] <- selected_countries_list$X..feed.as.grain..pigs/100
      dataset_feed_provenience_array_cast_complete_cases[,,which(unique(selected_items_list_livestock$Commodity.matching.crop.item.name)=="Meat, chicken"),2] <- selected_countries_list$X..feed.as.grain..poultry/100
      dataset_feed_provenience_array_cast_complete_cases[,,which(unique(selected_items_list_livestock$Commodity.matching.crop.item.name)=="Eggs, hen, in shell"),2] <- selected_countries_list$X..feed.as.grain..poultry/100

      
      ## 3 - Repartition of grain feed into crop species
      
      # create empty array for final data storage --> No. countries == 163 --> nrow and ncol=1; No. items == 5 (nLayers) 
      dataset_feed_crop_species_share_array <-  array(dim = c(163,8,length(unique(selected_items_list_livestock$Commodity.matching.crop.item.name))),
                                                      dimnames = list(Country = selected_countries_list$country,
                                                                      Feed_crop = c("Barley", "Maize", "Peas, dry", "Rice, paddy", "Rapeseed", "Sorghum", "Soybeans", "Wheat"),
                                                                      Livestock_item = unique(selected_items_list_livestock$Commodity.matching.crop.item.name)))     

  ### Bovines based items
     
      bovine_crops <- read.csv("Countries datasets/Repartition_feed_from_crops_milk_and_meat_bovines.csv")
      pig_crops <- read.csv("Countries datasets/Repartition_feed_from_crops_pigs.csv")
      poultry_crops <- read.csv("Countries datasets/Repartition_feed_from_crops_poultry.csv")
      
        for (k in 1:8) {
          dataset_feed_crop_species_share_array[,k,1] <- bovine_crops[,(5+k)]
          dataset_feed_crop_species_share_array[,k,3] <- bovine_crops[,(5+k)]
        }

        for (k in 1:8) {
          dataset_feed_crop_species_share_array[,k,5] <- pig_crops[,(5+k)]
        }

        for (k in 1:8) {
          dataset_feed_crop_species_share_array[,k,2] <- poultry_crops[,(5+k)]
        }

      

# ------------ ESTIMATE DOMESTIC PRODUCTION in PRIMARY PRODUCTS  --------------------------- 
        
   # Primary crops commodities
        
     # Upload full dataset without FAOSTAT Flags --> Encoding WINDOWS-1252 (Turkish) to recognize ô character
      dataset_production_no_flag <- read.csv(file = "faostat_data/Bulk_downloads/Production_Crops_E_All_Data_NOFLAG.csv", header = T, fileEncoding = "WINDOWS-1252")
        
        # Delete unnecessary years to keep range 2013-2016 and compute mean value
        dataset_production_no_flag_2013_2016 <- dataset_production_no_flag[,-c(8:59, 64)]
        dataset_production_no_flag_2013_2016$mean_2013_2016 <- rowMeans(dataset_production_no_flag_2013_2016[c("Y2013","Y2014","Y2015", "Y2016")], na.rm=T) 
        

        # create empty array for final data storage --> No. countries == 163 --> nrow and ncol=1; No. items == 120 (nLayers) 
        # --> the real no. items is lower (primary commodities) but I build here a dataset with 120 layers in order to be able to directly divide the yield dataset with the mass traded
        dataset_production_crops <-  array(dim = c(163,1,59),
                                                               dimnames = list(Country=selected_countries_list$country,
                                                                               Variable=c("Local_primary_crops_production"),
                                                                               PrimaryCommodity=sort(unique(selected_items_list_crops$Commodity.matching.crop.item.name))[-52]))     

        
        for (i in 1:59) {
          production_reference_item <- subset(dataset_production_no_flag_2013_2016, Item == as.vector(sort(unique(selected_items_list_crops$Commodity.matching.crop.item.name))[-52])[i])       # Subset for the item i
          production_reference_item <- subset(production_reference_item, Element == "Production")                                # subset for the production data in tonnes

          # Step 2: For  each item, extract the name of each Reporter country contained in the original dataset (rowname) and find the Index of the extracted Reporter country in the final dataset
          if (length(production_reference_item$Area)!=0) { 
            for (k in 1:length(production_reference_item$Area)) {
              row_cast <- as.vector(production_reference_item$Area[k])                                # Extract the name of the Country at each row
              Index_row <- which(rownames(dataset_production_crops)==row_cast)         # Match the name with the reference list of countries and find row_index
              
              # Step 3: If row Indexes contains a values (i.e. length==1), then the county in the original dataset correspond to one of the countries listed in the reference file.
              # for these countries, extract the data in the original dataset and assign them to the new position in the final dataset
              # ATT! This step is skipped if on Index length is !=1, which means that we do not consider that country in the analysis (e.g. because it is a small island, or a minor country)
              if (length(Index_row)==1 & !is.na(production_reference_item$mean_2013_2016[k])) {
                dataset_production_crops[Index_row, 1, i] <- production_reference_item[k,12] 
              }
              else                 dataset_production_crops[Index_row, 1, i] <- 0  
            }
          }
        }
    
        
        dataset_production_crops[is.na(dataset_production_crops)] <- 0
    
        dataset_production_crops_diagonal <- array(dim = c(163,163,59),
                                                   dimnames = list(Country_reporter=paste0(selected_countries_list$country,"_reporter"),
                                                                   Country_partner=paste0(selected_countries_list$country,"_partner"),
                                                                   PrimaryCommodity=sort(unique(selected_items_list_crops$Commodity.matching.crop.item.name))[-52]))     
        
        for (i in 1:59) {  
          dataset_production_crops_diagonal[,,i] <- diag(dataset_production_crops[,,i])
       }
       
       
      
       
       # Primary livestock commodities
       
           # Upload full dataset without FAOSTAT Flags --> Encoding WINDOWS-1252 (Turkish) to recognize ô character
           dataset_production_livestock_no_flag <- read.csv(file = "faostat_data/Bulk_downloads/Production_LivestockPrimary_E_All_Data_NOFLAG.csv", header = T, fileEncoding = "WINDOWS-1252")
           
           # Delete unnecessary years to keep range 2013-2016 and compute mean value
           dataset_production_livestock_no_flag_2013_2016 <- dataset_production_livestock_no_flag[,-c(8:59, 64)]
           dataset_production_livestock_no_flag_2013_2016$mean_2013_2016 <- rowMeans(dataset_production_livestock_no_flag_2013_2016[c("Y2013","Y2014","Y2015", "Y2016")], na.rm=T) 
       
           dataset_production_livestock <-  array(dim = c(163,1,length(unique(selected_items_list_livestock$Commodity.matching.crop.item.name))),
                                                              dimnames = list(Country=selected_countries_list$country,
                                                                              Variable=c("Local_primary_livestock_production"),
                                                                              PrimaryCommodity=sort(unique(selected_items_list_livestock$Commodity.matching.crop.item.name))))     
           

           for (i in 1:length(unique(selected_items_list_livestock$Commodity.matching.crop.item.name))) {
             production_reference_item <- subset(dataset_production_livestock_no_flag_2013_2016, Item == as.vector(sort(unique(selected_items_list_livestock$Commodity.matching.crop.item.name)))[i])       # Subset for the item i
             production_reference_item <- subset(production_reference_item, Element == "Production")                                # subset for the production data in tonnes
             
           
             # Step 2: For  each item, extract the name of each Reporter country contained in the original dataset (rowname) and find the Index of the extracted Reporter country in the final dataset
             if (length(production_reference_item$Area)!=0) { 
               for (k in 1:length(production_reference_item$Area)) {
                 row_cast <- as.vector(production_reference_item$Area[k])                                # Extract the name of the Country at each row
                 Index_row <- which(rownames(dataset_production_livestock)==row_cast)         # Match the name with the reference list of countries and find row_index
                 
                 # Step 3: If row Indexes contains a values (i.e. length==1), then the county in the original dataset correspond to one of the countries listed in the reference file.
                 # for these countries, extract the data in the original dataset and assign them to the new position in the final dataset
                 # ATT! This step is skipped if on Index length is !=1, which means that we do not consider that country in the analysis (e.g. because it is a small island, or a minor country)
                 if (length(Index_row)==1 & !is.na(production_reference_item$mean_2013_2016[k])) {
                   dataset_production_livestock[Index_row, 1, i] <- production_reference_item[k,12] 
                 }
                 else                 dataset_production_livestock[Index_row, 1, i] <- 0  
               }
             }
           }
           
           
           dataset_production_livestock[is.na(dataset_production_livestock)] <- 0
           
           dataset_production_livestock_diagonal <- array(dim = c(163,163,length(unique(selected_items_list_livestock$Commodity.matching.crop.item.name))),
                                                           dimnames = list(Country_reporter=paste0(selected_countries_list$country,"_reporter"),
                                                                           Country_partner=paste0(selected_countries_list$country,"_partner"),
                                                                           PrimaryCommodity=sort(unique(selected_items_list_livestock$Commodity.matching.crop.item.name))))     
           
           for (i in 1:5) {
             dataset_production_livestock_diagonal[,,i] <- diag(dataset_production_livestock[,,i])
           }
           
           ### DMI Livestock (Production + sum(Imports) --> Domestic material input
           
           dataset_DMI_livestock <- array(dim = c(163,1,length(unique(selected_items_list_livestock$Commodity.matching.crop.item.name))),
                                           dimnames = list(Country_reporter=paste0(selected_countries_list$country,"_reporter"),
                                                           Variable="DMI",
                                                           PrimaryCommodity=sort(unique(selected_items_list_livestock$Commodity.matching.crop.item.name))))
           
           dataset_DMI_livestock_diagonal <- array(dim = c(163,163,length(unique(selected_items_list_livestock$Commodity.matching.crop.item.name))),
                                                   dimnames = list(Country_reporter=paste0(selected_countries_list$country,"_reporter"),
                                                                   Country_partner=paste0(selected_countries_list$country,"_partner"),
                                                                   PrimaryCommodity=sort(unique(selected_items_list_livestock$Commodity.matching.crop.item.name))))
           
           
           for (i in 1:5) {
             dataset_DMI_livestock[,,i] <- dataset_production_livestock[,,i] + rowSums(dataset_import_array_cast_complete_cases_primary_livestock[,,i], na.rm=T)
             dataset_DMI_livestock_diagonal[,,i] <- diag(dataset_DMI_livestock[,,i])
             }
            
         
           
# ------------ CORRECT TRADE DATA FOR REEXPORTS procedure according to Kastner et al (2011) --------------------------
           
           # Livestock primary products
           
           A_livestock <- array(dim = c(163,163,length(unique(selected_items_list_livestock$Commodity.matching.crop.item.name))),
                                dimnames = list(Country_reporter=paste0(selected_countries_list$country,"_reporter"),
                                                Country_partner=paste0(selected_countries_list$country,"_partner"),
                                                PrimaryCommodity=sort(unique(selected_items_list_livestock$Commodity.matching.crop.item.name))))
          
           R_livestock  <- array(dim = c(163,163,length(unique(selected_items_list_livestock$Commodity.matching.crop.item.name))),
                                 dimnames = list(Country_reporter=paste0(selected_countries_list$country,"_reporter"),
                                                 Country_partner=paste0(selected_countries_list$country,"_partner"),
                                                 PrimaryCommodity=sort(unique(selected_items_list_livestock$Commodity.matching.crop.item.name))))
          
            c_livestock <- array(dim = c(163,163,length(unique(selected_items_list_livestock$Commodity.matching.crop.item.name))),
                                dimnames = list(Country_reporter=paste0(selected_countries_list$country,"_reporter"),
                                                Country_partner=paste0(selected_countries_list$country,"_partner"),
                                                PrimaryCommodity=sort(unique(selected_items_list_livestock$Commodity.matching.crop.item.name))))
           
            
            R_final_livestock <- array(dim = c(163,163,length(unique(selected_items_list_livestock$Commodity.matching.crop.item.name))),
                                       dimnames = list(Country_reporter=paste0(selected_countries_list$country,"_reporter"),
                                                       Country_partner=paste0(selected_countries_list$country,"_partner"),
                                                       PrimaryCommodity=sort(unique(selected_items_list_livestock$Commodity.matching.crop.item.name))))
            

           for (i in 1:5) {
             Inv_X <- (dataset_DMI_livestock_diagonal[,,i])   # We cannot invert a matrix with zero on the diagonal --> the country has a DMI=0 and so a DMI's export share=0
             diag(Inv_X[,])[diag(Inv_X[,])==0] <- 1           # We give value 1 to all coutries with 0 DMI. Its inverse is still 1
             Inv_X <- solve(Inv_X)                            # We calculate the inverse
             diag(Inv_X[,])[diag(Inv_X[,])==1] <- 0           # We assigne back value zero to country with the inverted values equal to 1
             A_livestock[,,i] <- dataset_import_array_cast_complete_cases_primary_livestock[,,i] %*% Inv_X    # We calculate the matrix reporting the share of exports from country j to country i in country j's DMI
           }
       
           
           for (i in 1:5) {
             R_livestock[,,i] <- solve(diag(163)-A_livestock[,,i]) %*% dataset_production_livestock_diagonal[,,i]
           }
             
          for (i in 1:5) {
             c_livestock[,,i] <- diag(1/dataset_DMI_livestock[,,i] * (dataset_DMI_livestock[,,i] - colSums(dataset_import_array_cast_complete_cases_primary_livestock[,,i], na.rm=T)))
          }
           
          for (i in 1:5) {  
             R_final_livestock[,,i] <- c_livestock[,,i] %*% R_livestock[,,i]   # Matrix where the elements Rij are the part of the DMI xi of country i that is produced in country j --> if country xi DMI==0 then all the row corresponding to country i is NA (or Zero)
          }
            
            
            # check share of negative values
            for (i in 1:5) {  
              data <- R_final_livestock[,,i]
              print(sum(round(data[which(data<0)], digit=0), na.rm=T)/sum(round(data[which(data>0)], digit=0), na.rm=T))
            }

            
            R_final_livestock[which(R_final_livestock<0)] <- 0   #Correct for negative value, if any
            

            ## Transform primary livestock commodities into crop primary commodities used for feed
            
            virtual_feed_total_crops_mass_for_livestock <- array(dim = c(163,163,length(unique(selected_items_list_livestock$Commodity.matching.crop.item.name))),
                                                        dimnames = list(Country_reporter = paste0(selected_countries_list$country, "_Reporter"),
                                                                        Country_partner = paste0(selected_countries_list$country, "_Partner"),
                                                                        PrimaryCommodity=sort(unique(selected_items_list_livestock$Commodity.matching.crop.item.name))))

            
            virtual_feed_primary_crops_mass_per_livestock <- array(dim = c(163,163,length(unique(selected_items_list_livestock$Commodity.matching.crop.item.name)), 8),
                                                                 dimnames = list(Country_reporter = paste0(selected_countries_list$country, "_Reporter"),
                                                                                 Country_partner = paste0(selected_countries_list$country, "_Partner"),
                                                                                 PrimaryCommodity=sort(unique(selected_items_list_livestock$Commodity.matching.crop.item.name)),
                                                                                 Feed_crop = c("Barley", "Maize", "Peas, dry", "Rice, paddy", "Rapeseed", "Sorghum", "Soybeans", "Wheat")))
            

            for (i in 1:length(unique(selected_items_list_livestock$Commodity.matching.crop.item.name))) {
              #Sweep function to apply different values to data in different columns/rows
              
              virtual_feed_total_crops_mass_for_livestock[,,i] <- sweep((R_final_livestock[,,i]*selected_items_list_livestock$coef.from.commodity.to.protein[which(selected_items_list_livestock$Commodity.matching.crop.item.name==as.vector(dimnames(R_final_livestock)[[3]])[i])[1]]), 2, dataset_feed_efficiency_array_cast_complete_cases[,1,which(dimnames(dataset_feed_efficiency_array_cast_complete_cases)[[3]]==as.vector(dimnames(R_final_livestock)[[3]])[i])], "*") # dataframe, row-wise (1) or column-wise (2), vector with length=nrow/ncol, binary operator
              # For each country: Step A. Imported/Produced mass primary livestock item i * coef to protein == imported Protein mass livestock item i * Feed efficiency item i == Mass of needed feed to produce the traded protein mass of item i
                    for (k in 1:8) {
                      virtual_feed_primary_crops_mass_per_livestock[,,i,k] <- sweep(virtual_feed_total_crops_mass_for_livestock[,,i], 2, (dataset_feed_provenience_array_cast_complete_cases[,1,which(dimnames(dataset_feed_provenience_array_cast_complete_cases)[[3]]==as.vector(dimnames(virtual_feed_total_crops_mass_for_livestock)[[3]])[i]),2]*dataset_feed_crop_species_share_array[,k,which(dimnames(dataset_feed_provenience_array_cast_complete_cases)[[3]]==as.vector(dimnames(virtual_feed_total_crops_mass_for_livestock)[[3]])[i])]), "*") 
                     # Step B. Mass of needed feed to produce item i * share_grain_feed == mass of grain needed to produce item i * share grain crop species k = Grain mass imported/produced for each crop primary species k and livestock item i
                      }
            }
            
            virtual_feed_primary_crops_mass_total <- array(dim = c(163,163, 8),
                                                          dimnames = list(Country_reporter = paste0(selected_countries_list$country, "_Reporter"),
                                                                          Country_partner = paste0(selected_countries_list$country, "_Partner"),
                                                                          Feed_crop = c("Barley", "Maize", "Peas, dry", "Rice, paddy", "Rapeseed", "Sorghum", "Soybeans", "Wheat")))
            
            
            virtual_feed_primary_crops_mass_total <- apply(virtual_feed_primary_crops_mass_per_livestock, MARGIN=c(1,2,4), sum, na.rm=T)
    
            
                    
      # Cropland primary products
            
            ## Calculate total cropland products (produced + Imported ??)
            
            dataset_import_array_cast_complete_cases_primary_crops # Diagonal is empty!
            virtual_feed_primary_crops_mass_total # Diagonal contains theoretical feed mass produced in country i in order to raise animal to produce in country i
            
            total_bilateral_trade_primary_crop_products <- dataset_import_array_cast_complete_cases_primary_crops
            
            for (i in 1:8) {
              total_bilateral_trade_primary_crop_products[,,which(sort(unique(selected_items_list_crops$Commodity.matching.crop.item.name))[-52]==as.vector(dimnames(virtual_feed_primary_crops_mass_total)[[3]][i]))] <- virtual_feed_primary_crops_mass_total[,,i] + total_bilateral_trade_primary_crop_products[,,which(sort(unique(selected_items_list_crops$Commodity.matching.crop.item.name))[-52]==as.vector(dimnames(virtual_feed_primary_crops_mass_total)[[3]][i]))]
              diag(total_bilateral_trade_primary_crop_products[,,which(sort(unique(selected_items_list_crops$Commodity.matching.crop.item.name))[-52]==as.vector(dimnames(virtual_feed_primary_crops_mass_total)[[3]][i]))]) <- 0
              }     
      
            
            
            
            ### DMI croplands (Production + sum(Imports) --> Domestic material input
            
            dataset_DMI_crops <- array(dim = c(163,1,59),
                                           dimnames = list(Country_reporter=paste0(selected_countries_list$country,"_reporter"),
                                                           Variable="DMI",
                                                           PrimaryCommodity=sort(unique(selected_items_list_crops$Commodity.matching.crop.item.name))[-52]))
            
            dataset_DMI_crops_diagonal <- array(dim = c(163,163,59),
                                                    dimnames = list(Country_reporter=paste0(selected_countries_list$country,"_reporter"),
                                                                    Country_partner=paste0(selected_countries_list$country,"_partner"),
                                                                    PrimaryCommodity=sort(unique(selected_items_list_crops$Commodity.matching.crop.item.name))[-52]))
            
            
            for (i in 1:59) {
              no_diag <- total_bilateral_trade_primary_crop_products[,,i]  # Values on the diagonal for crop items used as FEED represent the internal consumpption of that crop item as feed 
                                                                           # (therefore either a share of the internal production, either an imported quantity.
                                                                           # Therefore they have to be discounted form the DMI calculation to avoid double counted, since DMI=production + imports)
              diag(no_diag) <- 0
              dataset_DMI_crops[,,i] <- dataset_production_crops[,,i] + rowSums(no_diag[,], na.rm=T)
              dataset_DMI_crops_diagonal[,,i] <- diag(dataset_DMI_crops[,,i])
            }
            

           ## REEXPORTS accounting 
            
            
            A_crop <- array(dim = c(163,163,length(unique(selected_items_list_crops$Commodity.matching.crop.item.name)[-52])),
                                 dimnames = list(Country_reporter=paste0(selected_countries_list$country,"_reporter"),
                                                 Country_partner=paste0(selected_countries_list$country,"_partner"),
                                                 PrimaryCommodity=sort(unique(selected_items_list_crops$Commodity.matching.crop.item.name))[-52]))
            
            R_crop  <- array(dim = c(163,163,length(unique(selected_items_list_crops$Commodity.matching.crop.item.name)[-52])),
                                  dimnames = list(Country_reporter=paste0(selected_countries_list$country,"_reporter"),
                                                  Country_partner=paste0(selected_countries_list$country,"_partner"),
                                                  PrimaryCommodity=sort(unique(selected_items_list_crops$Commodity.matching.crop.item.name))[-52]))   
            
            c_crop <- array(dim = c(163,163,length(unique(selected_items_list_crops$Commodity.matching.crop.item.name)[-52])),
                                 dimnames = list(Country_reporter=paste0(selected_countries_list$country,"_reporter"),
                                                 Country_partner=paste0(selected_countries_list$country,"_partner"),
                                                 PrimaryCommodity=sort(unique(selected_items_list_crops$Commodity.matching.crop.item.name))[-52]))
            
            
            R_final_crop <- array(dim = c(163,163,length(unique(selected_items_list_crops$Commodity.matching.crop.item.name)[-52])),
                                       dimnames = list(Country_reporter=paste0(selected_countries_list$country,"_reporter"),
                                                       Country_partner=paste0(selected_countries_list$country,"_partner"),
                                                       PrimaryCommodity=sort(unique(selected_items_list_crops$Commodity.matching.crop.item.name))[-52]))
            
            
            
            for (i in 1:59) {
              Inv_X <- (dataset_DMI_crops_diagonal[,,i])       # We cannot invert a matrix with zero on the diagonal --> the country has a DMI=0 and so a DMI's export share=0
              diag(Inv_X[,])[diag(Inv_X[,])==0] <- 1           # We give value 1 to all coutries with 0 DMI. Its inverse is still 1
              Inv_X <- solve(Inv_X)                            # We calculate the inverse
              diag(Inv_X[,])[diag(Inv_X[,])==1] <- 0           # We assigne back value zero to country with the inverted values equal to 1
              A_crop[,,i] <- total_bilateral_trade_primary_crop_products[,,i] %*% Inv_X    # We calculate the matrix reporting the share of exports from country j to country i in country j's DMI
            }

            for (i in 1:59) {
              R_crop[,,i] <- solve(diag(163)-A_crop[,,i]) %*% dataset_production_crops_diagonal[,,i]    
              #R_crop[which(R_crop[,,1]<0)] <- 0
              }
            
            for (i in 1:59) {
              c_crop[,,i] <- diag(1/dataset_DMI_crops[,,i] * (dataset_DMI_crops[,,i] - colSums(total_bilateral_trade_primary_crop_products[,,i], na.rm=T)))
            
              }
            
            for (i in 1:59) {  
              R_final_crop[,,i] <- c_crop[,,i] %*% R_crop[,,i]   # Matrix where the elements Rijare the part of the DMI xi of country i that is produced in country j --> if country xi DMI==0 then all the row corresponding to country i is NA (or Zero)
            }
            
            # check share of negative values
            for (i in 1:59) {  
              data <- R_final_crop[,,i]
              q <- sum(round(data[which(data<0)], digit=0), na.rm=T)/sum(round(data[which(data>0)], digit=0), na.rm=T)
              if (!is.na(q) & q<(-0.02)) { print(i) & print(q) }
            }
            
            R_final_crop[which(R_final_crop<0)] <- 0   #Correct for negative value, if any
            
            
            R_final_crop_integer <- round(R_final_crop, digits = 0)  
            
            
            
# ------------ ESTIMATE VIRTUAL RESOURCES USE for Domestic Production (crop primary items) ----------------

      
          ## Calculation of virtual local used P fertilizers
          
          virtual_P_fert_produced_crops <- array(dim = c(163,1,length(unique(selected_items_list_crops$Commodity.matching.crop.item.name)[-52])),
                                                dimnames = list(Country_producer = paste0(selected_countries_list$country, "_Producer"),
                                                                Variable = "P_fert_produced_in_crops",
                                                                Primary_crop_item = sort(unique(selected_items_list_crops$Commodity.matching.crop.item.name))[-52]))
          
          for (i in 1:length(unique(selected_items_list_crops$Commodity.matching.crop.item.name)[-52])) {
            #Sweep function to apply different values to data in different columns/rows
           virtual_P_fert_produced_crops[,,i] <- (colSums(R_final_crop_integer[,,i], na.rm=T)/dataset_yield_array_cast_complete_cases[,,i]) * dataset_p_fertilisers_array[,1,i] # dataframe, row-wise (1) or column-wise (2), vector with length=nrow/ncol, binary operator
           # virtual_P_fert_produced_crops[,,i] <- sweep((R_final_crop_integer[,,i]/dataset_yield_array_cast_complete_cases[,,i]), 2, dataset_p_fertilizers_array[,1,i], "*") # dataframe, row-wise (1) or column-wise (2), vector with length=nrow/ncol, binary operator
            
          }

#  ----------- ESTIMATE VIRTUAL RESOURCES TRADE (imports) FLOWS --------------------------- 
        

        ## Calculation of virtual traded (imported) P fertilisers in tons P
        
        virtual_imported_p_fert_crops <- array(dim = c(163,163,length(unique(selected_items_list_crops$Commodity.matching.crop.item.name)[-52])),
                                                     dimnames = list(County_reporter = paste0(selected_countries_list$country, "_Reporter"),
                                                                     Country_partner = paste0(selected_countries_list$country, "_Partner"),
                                                                     Primary_item = sort(unique(selected_items_list_crops$Commodity.matching.crop.item.name))[-52]))
        
        for (i in 1:length(unique(selected_items_list_crops$Commodity.matching.crop.item.name)[-52])) {
          virtual_imported_p_fert_crops[,,i] <- sweep((R_final_crop_integer_[,,i]/dataset_yield_array_cast_complete_cases[,,i]), 2, dataset_p_fertilisers_array[,1,i], "*") # dataframe, row-wise (1) or column-wise (2), vector with length=nrow/ncol, binary operator
          
        }
        
    


        
        
# ------------ LOAD DATA on DOMESTIC AVAILABLE RESSOURCES (P and water) BY COUNTRY  ----------------
        
        ## Water resources
        
        water_available_resources_dataset <- read.csv("water/aquastat_water_resources_by_country.csv")
        Total_reneawable_water_by_country <- subset(water_available_resources_dataset, Variable.Name == "Total renewable water resources")
        Total_reneawable_water_by_country <- subset(Total_reneawable_water_by_country, Year=="2012")
        Total_exploitable_water_by_country <- subset(water_available_resources_dataset, Variable.Name == "Total exploitable water resources")

        
        # --> the real no. items is lower (primary commodities) but I build here a dataset with 120 layers in order to be able to directly divide the yield dataset with the mass traded
        dataset_total_reneawable_water_by_country <-  array(dim = c(163,1))     
        # Assign countries to row and column names --> Row correspond to Reporter Countries, while columns to the Partner countries
        dimnames(dataset_total_reneawable_water_by_country)[[1]] <- selected_countries_list$country
        dimnames(dataset_total_reneawable_water_by_country)[[2]] <- c("Total_reneawable_water")
        

             for (k in 1:length(Total_reneawable_water_by_country$Area)) {
              row_cast <- as.vector(Total_reneawable_water_by_country$Area[k])                                # Extract the name of the Reporter country at each row
              Index_row <- which(rownames(dataset_total_reneawable_water_by_country)==row_cast)    # Match the name with the reference list of countries and find row_index
              
              # Step 3: If row Index contains a values (i.e. length==1), then the county in the original dataset correspond to one of the countries listed in the reference file.
              # for these countries, extract the data in the original dataset and assign them to the new position in the final dataset
              # ATT! This step is skipped if on Index length is !=1, which means that we do not consider that country in the analysis (e.g. because it is a small island, or a minor country)
              if (length(Index_row)==1 & !is.na(Total_reneawable_water_by_country$Value[k])) {
                dataset_total_reneawable_water_by_country[Index_row, 1] <- as.numeric(Total_reneawable_water_by_country[k, 6])  # take mean yield of 2013-2017
              }
            }

        
        #na_index <- names(dataset_total_renewable_water_by_country[which(is.na(dataset_total_renewable_water_by_country)),])
        dataset_total_reneawable_water_by_country[30,] <- as.numeric(Total_reneawable_water_by_country[36, 6])   # china
        dataset_total_reneawable_water_by_country[137,] <- as.numeric(Total_reneawable_water_by_country[156, 6])  # Sudan
        dataset_total_reneawable_water_by_country[53,] <- 134   # French Guiana, from "Review of World Water Resources by Country - fao.org"

       
        # P resources (?) 
        
        total_P2O5_available_ressources <- read.csv("Rock Phospate/Internal_resources_P2O5.csv", header=T, fileEncoding = "WINDOWS-1252", check.names=FALSE)
        total_P2O5_available_ressources$P_mined <- total_P2O5_available_ressources$`P205 [tons]`/2.29
        
        
# ------------ CALCULATE INDICATORS  ----------------
        

     ### P fertilizers - Crops - Import/Export
                    
                    total_P_fert_imports_per_country_crops <- apply(virtual_imported_p_fert_crops, MARGIN = c(1,2), FUN = 'sum', na.rm=T)  # in tons
                    total_P_fert_imports_per_country_crops <- rowSums(total_P_fert_imports_per_country_crops)
                    
                    virtual_P_fert_exported_crops <-  array(dim = c(163,163,length(unique(selected_items_list_crops$Commodity.matching.crop.item.name)[-52])),
                                                            dimnames = list(Country_partner = paste0(selected_countries_list$country, "_Partner"),
                                                                            Country_reporter = paste0(selected_countries_list$country, "_Reporter"),
                                                                            Primary_crop_item = sort(unique(selected_items_list_crops$Commodity.matching.crop.item.name))[-52]))
                    
                    for (i in 1:length(unique(selected_items_list_crops$Commodity.matching.crop.item.name)[-52])) {
                      virtual_P_fert_exported_crops[,,i] <- t(virtual_imported_p_fert_crops[,,i])
                    }
                    
                    total_P_fert_export_per_country_crops <- apply(virtual_P_fert_exported_crops, MARGIN = c(1,2), FUN = 'sum', na.rm=T) 
                    total_P_fert_export_per_country_crops <- rowSums(total_P_fert_export_per_country_crops)
                    
                    ### P fertilizer - Crops - Produced
                    
                    total_P_fert_produced_per_country_crops <- apply(virtual_P_fert_produced_crops, MARGIN = c(1,2), FUN = 'sum', na.rm=T)  # in m3
                    total_P_fert_produced_per_country_crops <- rowSums(total_P_fert_produced_per_country_crops)
                    
                    
                    ### P fertilizers - Total net consumption
                    
                    net_P_fert_consuption_total <- total_P_fert_produced_per_country_crops + total_P_fert_imports_per_country_crops - total_P_fert_export_per_country_crops
                    