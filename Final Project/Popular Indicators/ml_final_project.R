suppressMessages(require(data.table))
suppressMessages(require(lfe))
suppressMessages(require(foreign))
suppressMessages(require(stargazer))


rm(list=ls())
options(max.print=100)

# clean up and reshape the data
world_bank_raw_data <-as.data.table(read.csv("world_bank_data.csv", header = T, sep = ","))
data_length <- dim(world_bank_raw_data)[1]
world_bank_raw_data <- world_bank_raw_data[ 1:data_length,]

factorNames <- unique(world_bank_raw_data[, .(Series.Name, Series.Code )])

world_bank_raw_data[, Series.Name := NULL]
world_bank_raw_data[, Country.Name := NULL]

# Y : GDP growth (annual %) NY.GDP.MKTP.KD.ZG

# factors: 
# 1. population growth SP.POP.GROW
# 3. Inflation, consumer prices (annual %) FP.CPI.TOTL.ZG
# 4. Exports of goods and services (% of GDP) NE.EXP.GNFS.ZS
# 5. Imports of goods and services (% of GDP)  NE.IMP.GNFS.ZS
# 7. Foreign direct investment, net inflows (% of GDP) BN.KLT.DINV.CD
# 8. Gross capital formation (% of GDP)  NE.GDI.TOTL.ZS

colnames(world_bank_raw_data) <- c("Factor_name", "Country_code", 1960:2018)
reshaped_wb_data <- melt(world_bank_raw_data, id.vars = c("Country_code", "Factor_name"), 
                         variable.name = "year", 
                         value.name = "Factor_value")
reshaped_wb_data[Factor_value=="..", Factor_value := NA]

factorsInModel <- c("NY.GDP.MKTP.KD.ZG", 
                    "SP.POP.GROW",
                    "FP.CPI.TOTL.ZG", 
                    "NE.EXP.GNFS.ZS", 
                    "NE.IMP.GNFS.ZS", 
                    "BN.KLT.DINV.CD",
                    "EN.ATM.CO2E.PC",
                    "SE.SEC.ENRR",
                    "SP.POP.GROW",
                    "TG.VAL.TOTL.GD.ZS")
selected_wb_data <- reshaped_wb_data[Factor_name %in% factorsInModel,]

# reshape factors to columns
final_data <- dcast(selected_wb_data, Country_code +  year ~ Factor_name , value.var="Factor_value")

# restrict to be after year 1970 
final_data <- final_data[, year := as.numeric(as.character(year))]
final_data <- final_data[year>1969,]

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
final_data[next_year != (year + 1), `:=`(NY.GDP.MKTP.KD.ZG_lagged, NA)]


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

# mean normalize data 
for(i in factorsInModel_lagged){
  final_data[, `:=`(paste0(i), (get(i) - mean(get(i),na.rm=T))/(max(get(i)) - min(get(i)))), by=c("Country_code")]
}


# regression with and without fixed effects
r1 <- felm(NY.GDP.MKTP.KD.ZG ~ NY.GDP.MKTP.KD.ZG_lagged + SP.POP.GROW_lagged + FP.CPI.TOTL.ZG_lagged+
             NE.EXP.GNFS.ZS_lagged + NE.IMP.GNFS.ZS_lagged + BN.KLT.DINV.CD_lagged + EN.ATM.CO2E.PC_lagged+
             SE.SEC.ENRR_lagged + SP.POP.GROW_lagged + TG.VAL.TOTL.GD.ZS_lagged, data = final_data, na.action = na.omit)

r2 <- felm(NY.GDP.MKTP.KD.ZG ~ NY.GDP.MKTP.KD.ZG_lagged + SP.POP.GROW_lagged + FP.CPI.TOTL.ZG_lagged+
             NE.EXP.GNFS.ZS_lagged + NE.IMP.GNFS.ZS_lagged + BN.KLT.DINV.CD_lagged + EN.ATM.CO2E.PC_lagged+
             SE.SEC.ENRR_lagged + SP.POP.GROW_lagged + TG.VAL.TOTL.GD.ZS_lagged | 0 | 0 | year, data = final_data, na.action = na.omit)

r3 <- felm(NY.GDP.MKTP.KD.ZG ~ NY.GDP.MKTP.KD.ZG_lagged + SP.POP.GROW_lagged + FP.CPI.TOTL.ZG_lagged+
             NE.EXP.GNFS.ZS_lagged + NE.IMP.GNFS.ZS_lagged + BN.KLT.DINV.CD_lagged + EN.ATM.CO2E.PC_lagged+
             SE.SEC.ENRR_lagged + SP.POP.GROW_lagged + TG.VAL.TOTL.GD.ZS_lagged | year | 0 | year + Country_code, data = final_data, na.action = na.omit)

stargazer(r1, r2,r3, type = "text", report = "vc*t", add.lines = 
            list(c("Year FE","N","Y","Y"),c("Country, Year Clustering", "N","N","Y")))

##################################################################    



###################### world GDP Construction ###################################
# reshape factors to columns
factorList <- c("NY.GDP.MKTP.CD", "NY.GDP.MKTP.KD.ZG")
world_GDP_data <- reshaped_wb_data[Factor_name %in% factorList,]
final_data_2 <- dcast(world_GDP_data, Country_code +  year ~ Factor_name , value.var="Factor_value")

# convert columns to numeric data
final_data_2[, NY.GDP.MKTP.CD:= as.numeric(NY.GDP.MKTP.CD)]
final_data_2[, NY.GDP.MKTP.KD.ZG := as.numeric(NY.GDP.MKTP.KD.ZG)]
final_data_2[, NY.GDP.MKTP.KD.ZG := NY.GDP.MKTP.KD.ZG/100]

final_data_2[, NY.GDP.MKTP.CD_lagged := shift(NY.GDP.MKTP.CD), by = c("Country_code")]
final_data_2 <- final_data_2[NY.GDP.MKTP.CD_lagged != "NA"]


# value weight portfolio, this might be wrong, let's see
valueWeight_world_GDP <- final_data_2[,list(GDP_Vw = weighted.mean( NY.GDP.MKTP.KD.ZG, NY.GDP.MKTP.CD_lagged, na.rm = TRUE)),
                            by=list(year)]

# equal weight portfolio, this might be wrong, let's see
equalWeight_world_GDP <- final_data_2[,list(GDP_Ew = mean( NY.GDP.MKTP.KD.ZG, na.rm = TRUE)),
                                      by=list(year)]

# sort
setorder(valueWeight_world_GDP, year)
setorder(equalWeight_world_GDP, year)
valueWeight_world_GDP <- valueWeight_world_GDP[-length(valueWeight_world_GDP$year)]
equalWeight_world_GDP <- equalWeight_world_GDP[-length(equalWeight_world_GDP$year)]



# import World GDP data from work
world_GDP_raw_data <-as.data.table(read.csv("world_GDP.csv", header = T, sep = ","))
world_GDP_raw_data[, Series.Name :=NULL]
world_GDP_raw_data[, Series.Code :=NULL]
world_GDP_raw_data[, Country.Name :=NULL]
world_GDP_raw_data[, Country.Code :=NULL]


colnames(world_GDP_raw_data) <- as.character(c(1961:2017))
world_GDP_data <- t(world_GDP_raw_data)/100

xyr <- as.numeric(as.character(valueWeight_world_GDP$year))
plot(x = xyr, y=valueWeight_world_GDP$GDP_Vw, type="l",col="blue", main = "World GDP growth", xlab = "year", ylab="GDP Growth")
lines(x= xyr, y= world_GDP_data[,1],  type="l",col="green")
lines(x= xyr, y= equalWeight_world_GDP$GDP_Ew,  type="l",col="red")
legend("bottomleft",legend=c("Value Weight","Equal Weight","Actual World Bank Data"),fill=c("blue","red","green"), cex = 0.8)
