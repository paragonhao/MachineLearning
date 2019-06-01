suppressMessages(require(data.table))
suppressMessages(require(lfe))
suppressMessages(require(foreign))
suppressMessages(require(stargazer))
suppressMessages(require(zoo))
suppressWarnings(require(dplyr))

rm(list=ls())

# clean up and reshape the data
world_bank_raw_data <-as.data.table(read.csv("world_bank_data.csv", header = T, sep = ","))
data_length <- dim(world_bank_raw_data)[1]
world_bank_raw_data <- world_bank_raw_data[ 1:data_length,]

factorNames <- unique(world_bank_raw_data[, .(Series.Name, Series.Code )])

world_bank_raw_data[, Series.Name := NULL]
world_bank_raw_data[, Country.Name := NULL]

# Y : GDP growth (annual %) NY.GDP.MKTP.KD.ZG

colnames(world_bank_raw_data) <- c("Factor_name", "Country_code", 1960:2018)
reshaped_wb_data <- melt(world_bank_raw_data, id.vars = c("Country_code", "Factor_name"), 
                         variable.name = "year", 
                         value.name = "Factor_value")
reshaped_wb_data[Factor_value=="..", Factor_value := NA]

country_top20 = c("USA", "CHN","JPN","DEU","GBR","FRA","IND","ITA","BRA","CAN","RUS
","KOR","ESP","AUS","MEX","IDN","NLD","SAU","TUR","CHE")

factorsInModel <- c("NY.GDP.MKTP.CD",
                    "NY.GDP.MKTP.KD.ZG", 
                    "SP.POP.GROW",
                    "FP.CPI.TOTL.ZG", 
                    "NE.EXP.GNFS.ZS", 
                    "NE.IMP.GNFS.ZS", 
                    "BN.KLT.DINV.CD",
                    "EN.ATM.CO2E.PC",
                    "TG.VAL.TOTL.GD.ZS")
selected_wb_data <- reshaped_wb_data[Factor_name %in% factorsInModel,]

# pick G20 goods, remove this line if want to get whole world data
selected_wb_data <- selected_wb_data[Country_code %in% country_top20,]

selected_wb_data[, year := as.numeric(as.character(year))]

# pick data from 1975 to 2015
g20_data <- selected_wb_data[year >1975 & year < 2015,]

# reshape factors to columns
final_g20_data <- dcast(g20_data, Country_code +  year ~ Factor_name , value.var="Factor_value")

# convert datatype to numeric
for(i in factorsInModel){
  final_g20_data[, `:=`(paste0(i), as.numeric(get(i)))]
}

# fill in missing values with median data
for(i in factorsInModel){
  final_g20_data[, `:=`(paste0(i), ifelse(is.na(get(i)), median(get(i), na.rm = T), get(i))), by=c("Country_code")]
}

# sort the data
setorder(final_g20_data, Country_code, year)

# lagg all the t-1 variable to match with GDP at t
for(i in factorsInModel){
  final_g20_data[, `:=`(paste0(i,"_lagged"), shift(as.numeric(get(i)))), by=c("Country_code")]
}

final_g20_data <- final_g20_data[NY.GDP.MKTP.CD_lagged != "NA"]

# value weight portfolio, this might be wrong, let's see
valueWeight_world_GDP <- final_g20_data[,list(GDP_Vw = weighted.mean(NY.GDP.MKTP.KD.ZG, NY.GDP.MKTP.CD_lagged, na.rm = TRUE)),
                                      by=list(year)]

# pick lagged models 
factorsInModel_lagged <- paste0(factorsInModel,"_lagged") 


# convert datatype to numeric
for(i in factorsInModel_lagged){
  final_g20_data[, `:=`(paste0(i), as.numeric(get(i)))]
}

# take out nas
for(i in factorsInModel_lagged){
  final_g20_data <- final_g20_data[!is.na(get(i))]
}

# # mean normalize data 
# for(i in factorsInModel_lagged){
#   final_g20_data[, `:=`(paste0(i), (get(i) - mean(get(i),na.rm=T))/(max(get(i)) - min(get(i))))]
# }
##################################################################

# regression with and without fixed effects
r1 <- felm(NY.GDP.MKTP.KD.ZG ~ NY.GDP.MKTP.KD.ZG_lagged + SP.POP.GROW_lagged + 
             FP.CPI.TOTL.ZG_lagged + NE.EXP.GNFS.ZS_lagged + 
             NE.IMP.GNFS.ZS_lagged + BN.KLT.DINV.CD_lagged + 
             EN.ATM.CO2E.PC_lagged + TG.VAL.TOTL.GD.ZS_lagged, data = final_g20_data, na.action = na.omit)

r2 <- felm(NY.GDP.MKTP.KD.ZG ~ NY.GDP.MKTP.KD.ZG_lagged + SP.POP.GROW_lagged + 
             FP.CPI.TOTL.ZG_lagged + NE.EXP.GNFS.ZS_lagged + 
             NE.IMP.GNFS.ZS_lagged + BN.KLT.DINV.CD_lagged + 
             EN.ATM.CO2E.PC_lagged + TG.VAL.TOTL.GD.ZS_lagged | 0 | 0 | year, data = final_g20_data, na.action = na.omit)

r3 <- felm(NY.GDP.MKTP.KD.ZG ~ NY.GDP.MKTP.KD.ZG_lagged + SP.POP.GROW_lagged + 
             FP.CPI.TOTL.ZG_lagged + NE.EXP.GNFS.ZS_lagged + 
             NE.IMP.GNFS.ZS_lagged + BN.KLT.DINV.CD_lagged + 
             EN.ATM.CO2E.PC_lagged + TG.VAL.TOTL.GD.ZS_lagged | 0 | 0 | year + Country_code, data = final_g20_data, na.action = na.omit)

stargazer(r1, r2, r3, type = "text", report = "vc*t", add.lines = 
            list(c("Year FE","N","Y","Y"),c("Country, Year Clustering", "N","N","Y")))

setorder(final_g20_data, Country_code, NY.GDP.MKTP.CD, NY.GDP.MKTP.CD_lagged)

in_sample_data <- final_g20_data[,.(NY.GDP.MKTP.KD.ZG_lagged, SP.POP.GROW_lagged,
                                      FP.CPI.TOTL.ZG_lagged, NE.EXP.GNFS.ZS_lagged,  
                                      NE.IMP.GNFS.ZS_lagged, BN.KLT.DINV.CD_lagged, 
                                      EN.ATM.CO2E.PC_lagged, TG.VAL.TOTL.GD.ZS_lagged)]

in_sample_prediction <- as.matrix(in_sample_data, ncol=8)  %*% matrix(as.numeric(r3$coefficients[2:9]))
in_sample_prediction <- cbind(in_sample_prediction, final_g20_data$year)
in_sample_prediction <- as.data.table(cbind(in_sample_prediction, final_g20_data$NY.GDP.MKTP.CD_lagged))
colnames(in_sample_prediction) <- c("GDP_growth", "year", "lagged_GDP_amt")

# value weight portfolio
valueWeight_g20_GDP <- in_sample_prediction[,list(GDP_Vw = weighted.mean(GDP_growth, lagged_GDP_amt, na.rm = T)),
                                      by=list(year)]

# sort the data 
setorder(valueWeight_g20_GDP, year)

###### ploting against G20 GDP
valueWeight_g20_GDP[, GDP_Vw := shift(GDP_Vw, type = "lead")]
valueWeight_world_GDP <- valueWeight_world_GDP[year >1976 & year < 2015,]
# predicted GDP Value-weight GDP
plot(x=valueWeight_g20_GDP$year, ylim=c(-4,6), main="G20 GDP One Year In-sample Prediction",
     xlab="GDP Year" , ylab="GDP Growth",
     y= valueWeight_g20_GDP$GDP_Vw + r3$coefficients[1], type = "b", col="blue")
# actual GDP 20 Value-weight GDP 
lines(x=valueWeight_world_GDP$year, y= valueWeight_world_GDP$GDP_Vw, type = "b", col="green")
legend("bottomleft",legend=c("Value Weight","Actual World Bank Data"), fill=c("blue", "green"))
##################################################################    


###################### world GDP Construction ###################################
# # reshape factors to columns
# factorList <- c("NY.GDP.MKTP.CD", "NY.GDP.MKTP.KD.ZG")
# world_GDP_data <- reshaped_wb_data[Factor_name %in% factorList,]
# final_data_2 <- dcast(world_GDP_data, Country_code +  year ~ Factor_name , value.var="Factor_value")
# 
# # convert columns to numeric data
# final_data_2[, NY.GDP.MKTP.CD:= as.numeric(NY.GDP.MKTP.CD)]
# final_data_2[, NY.GDP.MKTP.KD.ZG := as.numeric(NY.GDP.MKTP.KD.ZG)]
# final_data_2[, NY.GDP.MKTP.KD.ZG := NY.GDP.MKTP.KD.ZG/100]
# 
# final_data_2[, NY.GDP.MKTP.CD_lagged := shift(NY.GDP.MKTP.CD), by = c("Country_code")]
# final_data_2 <- final_data_2[NY.GDP.MKTP.CD_lagged != "NA"]
# 
# 
# # value weight portfolio, this might be wrong, let's see
# valueWeight_world_GDP <- final_data_2[,list(GDP_Vw = weighted.mean( NY.GDP.MKTP.KD.ZG, NY.GDP.MKTP.CD_lagged, na.rm = TRUE)),
#                                       by=list(year)]
# 
# # equal weight portfolio, this might be wrong, let's see
# equalWeight_world_GDP <- final_data_2[,list(GDP_Ew = mean( NY.GDP.MKTP.KD.ZG, na.rm = TRUE)),
#                                       by=list(year)]
# 
# # sort
# setorder(valueWeight_world_GDP, year)
# setorder(equalWeight_world_GDP, year)
# valueWeight_world_GDP <- valueWeight_world_GDP[-length(valueWeight_world_GDP$year)]
# equalWeight_world_GDP <- equalWeight_world_GDP[-length(equalWeight_world_GDP$year)]



# # import World GDP data from work
# world_GDP_raw_data <-as.data.table(read.csv("world_GDP.csv", header = T, sep = ","))
# world_GDP_raw_data[, Series.Name :=NULL]
# world_GDP_raw_data[, Series.Code :=NULL]
# world_GDP_raw_data[, Country.Name :=NULL]
# world_GDP_raw_data[, Country.Code :=NULL]
# 
# 
# colnames(world_GDP_raw_data) <- as.character(c(1961:2017))
# world_GDP_data <- t(world_GDP_raw_data)/100
# 
# xyr <- as.numeric(as.character(valueWeight_world_GDP$year))
# plot(x = xyr, y=valueWeight_world_GDP$GDP_Vw, type="l",col="blue", main = "World GDP growth", xlab = "year", ylab="GDP Growth")
# lines(x= xyr, y= world_GDP_data[,1],  type="l",col="green")
# lines(x= xyr, y= equalWeight_world_GDP$GDP_Ew,  type="l",col="red")
# legend("bottomleft",legend=c("Value Weight","Equal Weight","Actual World Bank Data"),fill=c("blue","red","green"), cex = 0.8)
