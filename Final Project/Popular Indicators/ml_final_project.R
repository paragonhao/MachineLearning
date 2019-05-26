suppressMessages(require(data.table))

rm(list=ls())

# clean up and reshape the data
world_bank_raw_data <-as.data.table(read.csv("world_bank_data.csv", header = T, sep = ","))
world_bank_raw_data <- world_bank_raw_data[ 1:5425,]
world_bank_raw_data[, Series.Code := NULL]
world_bank_raw_data[, Country.Name := NULL]

colnames(world_bank_raw_data) <- c("Factor_name", "Country_code", 1960:2018)
reshaped_wb_data <- melt(world_bank_raw_data, id.vars = c("Country_code", "Factor_name"), 
                         variable.name = "year", 
                         value.name = "Factor_value")
reshaped_wb_data[Factor_value=="..", Factor_value := NA]

#reshaped_wb_data[is.na(Factor_value)]

