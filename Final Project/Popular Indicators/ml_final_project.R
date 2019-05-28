suppressMessages(require(data.table))

rm(list=ls())
options(max.print=100)

# clean up and reshape the data
world_bank_raw_data <-as.data.table(read.csv("world_bank_data.csv", header = T, sep = ","))
world_bank_raw_data <- world_bank_raw_data[ 1:5425,]

factorNames <- unique(world_bank_raw_data[, .(Series.Name, Series.Code )])

world_bank_raw_data[, Series.Name := NULL]
world_bank_raw_data[, Country.Name := NULL]

# Y : GDP growth (annual %) NY.GDP.MKTP.KD.ZG

# factors: 
# 1. population growth SP.POP.GROW
# 2. Energy use (kg of oil equivalent per capita) EG.USE.PCAP.KG.OE
# 3. Inflation, consumer prices (annual %) FP.CPI.TOTL.ZG
# 4. Exports of goods and services (% of GDP) NE.EXP.GNFS.ZS
# 5. Imports of goods and services (% of GDP)  NE.IMP.GNFS.ZS
# 6. High-technology exports (% of manufactured exports) TX.VAL.TECH.MF.ZS
# 7. Foreign direct investment, net inflows (% of GDP) BN.KLT.DINV.CD
# 8. Gross capital formation (% of GDP)  NE.GDI.TOTL.ZS

colnames(world_bank_raw_data) <- c("Factor_name", "Country_code", 1960:2018)
reshaped_wb_data <- melt(world_bank_raw_data, id.vars = c("Country_code", "Factor_name"), 
                         variable.name = "year", 
                         value.name = "Factor_value")
reshaped_wb_data[Factor_value=="..", Factor_value := NA]

factorsInModel <- c("NY.GDP.MKTP.KD.ZG", "SP.POP.GROW", "EG.USE.PCAP.KG.OE", 
                    "FP.CPI.TOTL.ZG", "NE.EXP.GNFS.ZS", "NE.IMP.GNFS.ZS", 
                    "TX.VAL.TECH.MF.ZS", "BN.KLT.DINV.CD","NE.GDI.TOTL.ZS") 

reshaped_wb_data <- reshaped_wb_data[Factor_name %in% factorsInModel,]

# reshape factors to columns
final_data <- dcast(reshaped_wb_data, Country_code +  year ~ Factor_name , value.var="Factor_value")




