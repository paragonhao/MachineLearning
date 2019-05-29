suppressMessages(require(data.table))
suppressMessages(require(lfe))
suppressMessages(require(foreign))
suppressMessages(require(stargazer))


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
# 2. Energy use (kg of oil equivalent per capita) EG.USE.PCAP.KG.OE, consider taking out as not a lot of data
# 3. Inflation, consumer prices (annual %) FP.CPI.TOTL.ZG
# 4. Exports of goods and services (% of GDP) NE.EXP.GNFS.ZS
# 5. Imports of goods and services (% of GDP)  NE.IMP.GNFS.ZS
# 6. High-technology exports (% of manufactured exports) TX.VAL.TECH.MF.ZS, consider taking out as not a lot of data
# 7. Foreign direct investment, net inflows (% of GDP) BN.KLT.DINV.CD
# 8. Gross capital formation (% of GDP)  NE.GDI.TOTL.ZS

colnames(world_bank_raw_data) <- c("Factor_name", "Country_code", 1960:2018)
reshaped_wb_data <- melt(world_bank_raw_data, id.vars = c("Country_code", "Factor_name"), 
                         variable.name = "year", 
                         value.name = "Factor_value")
reshaped_wb_data[Factor_value=="..", Factor_value := NA]

factorsInModel <- c("NY.GDP.MKTP.KD.ZG", "SP.POP.GROW",
                    "FP.CPI.TOTL.ZG", "NE.EXP.GNFS.ZS", "NE.IMP.GNFS.ZS", "BN.KLT.DINV.CD","NE.GDI.TOTL.ZS") 

reshaped_wb_data <- reshaped_wb_data[Factor_name %in% factorsInModel,]

# reshape factors to columns
final_data <- dcast(reshaped_wb_data, Country_code +  year ~ Factor_name , value.var="Factor_value")

# sort the data
setorder(final_data, Country_code, year)

# lagg all the t-1 variable to match with GDP at t
for(i in factorsInModel){
  final_data[, `:=`(paste0(i,"_lagged"), shift(as.numeric(get(i)))), by=c("Country_code")]
}

# make sure observation is actually next year
final_data[, year := as.numeric(as.character(year))]
final_data <- final_data[year!= 2018]
final_data[, `:=`(next_year, shift(year, type="lead")), by=c("Country_code")]
final_data[next_year != (year + 1), `:=`(NY.GDP.MKTP.KD.ZG, NA)]


factorsInModel_lagged <- paste0(factorsInModel,"_lagged")

# lagg all the t-1 variable to match with GDP at t
for(i in factorsInModel_lagged){
  final_data <- final_data[!is.na(get(i))]
}

final_data[,NY.GDP.MKTP.KD.ZG:= as.numeric(NY.GDP.MKTP.KD.ZG)]

# convert all lagged variable as numeric
for(i in factorsInModel_lagged){
  final_data <- final_data[!is.na(get(i))]
}

r1 <- felm(NY.GDP.MKTP.KD.ZG ~ NY.GDP.MKTP.KD.ZG_lagged + FP.CPI.TOTL.ZG_lagged, data = final_data, na.action = na.omit)


