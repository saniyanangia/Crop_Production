library(dplyr)
library(ggplot2)
library(class)
library(boot)
library(e1071)
library(caret)
library(glmnet)
library(corrplot)
library(olsrr)
library(lmtest)
library(stats)

crop_data_all = read.csv("file_path", header = TRUE)
head(crop_data_all)
tail(crop_data_all)
summary(crop_data_all)
colnames(crop_data_all)
str(crop_data_all)
attach(crop_data_all)

#Find top 5 producer countries of corn in 2021
crop_data_2022 = filter(crop_data_all, Market_Year == 2021, Commodity_Description == 'Corn', Attribute_Description == "Production")
crop_data_2022 = crop_data_2022 %>% arrange(desc(Value)) %>% slice(1:6)
pdtn_figures = data.frame(Country = crop_data_2022$Country_Name, Value = crop_data_2022$Value*1000)
print(pdtn_figures)
pdtn_figures = pdtn_figures[c(1:3, 5:6),]
print(pdtn_figures)
#Top 5 producer countries in 2021 are United States, China, Brazil, Argentina and Ukraine. 
#European Union is excluded since it is not a single country.

#Find region with highest production in each of the top 5 countries
#United States: Iowa
#China: Heilongjiang
#Brazil: Mato Grosso
#Argentina: Cordoba
#Ukraine: Poltava Oblast

#Regional Production Figures
crop_data_top5 = filter(crop_data_all, Country_Name %in% c("United States", "China", "Brazil", "Argentina", "Ukraine"), Commodity_Description == 'Corn', Attribute_Description == "Production")
Year = filter(crop_data_top5, Country_Name == "United States")$Market_Year

iowa_corn_stats = c(0.17*(filter(crop_data_top5, Country_Name == "United States")$Value))
hlj_corn_stats = c(0.15*(filter(crop_data_top5, Country_Name == "China")$Value))
mg_corn_stats = c(0.32*(filter(crop_data_top5, Country_Name == "Brazil")$Value))
cdb_corn_stats = c(0.32*(filter(crop_data_top5, Country_Name == "Argentina")$Value))
pltv_corn_stats = c(0.13*(filter(crop_data_top5, Country_Name == "Brazil")$Value))

regional_pdtn = data.frame(Year, iowa_corn_stats, hlj_corn_stats, mg_corn_stats, cdb_corn_stats, pltv_corn_stats)
regional_pdtn = regional_pdtn %>% rename("Iowa" = 2, "Heilongjiang" = 3, "Mato_Grosso" = 4, "Cordoba"= 5, "Poltava_Oblast" = 6)

#Data Pre-processing

#Iowa NDVI values
#Growing Season = April to November
iowa_ndvi = read.csv("file_path", skip = 14, header = TRUE)
head(iowa_ndvi)
tail(iowa_ndvi)
summary(iowa_ndvi)
iowa_ndvi$SAMPLE.VALUE = as.numeric(iowa_ndvi$SAMPLE.VALUE)
iowa_ndvi$ANOM.VALUE = as.numeric(iowa_ndvi$ANOM.VALUE)
iowa_ndvi$START.DATE = format(as.Date(iowa_ndvi$START.DATE, "%d/%m/%y"), "%d/%m/%Y")
iowa_ndvi_df = data.frame(
  iowa_ndvi.START.DATE = as.Date(iowa_ndvi$START.DATE, format = "%d/%m/%Y"),
  iowa_ndvi.SAMPLE.VALUE = iowa_ndvi$SAMPLE.VALUE,
  iowa_ndvi.ANOM.VALUE = iowa_ndvi$ANOM.VALUE
)
iowa_ndvi_df = na.omit(iowa_ndvi_df)
ggplot(data = iowa_ndvi_df, aes(x = iowa_ndvi.START.DATE, y = iowa_ndvi.ANOM.VALUE)) + 
  geom_point() +
  labs(x = "Date", y = "NDVI", title = "Iowa NDVI Anomalies")
iowa_anom = iowa_ndvi_df$iowa_ndvi.ANOM.VALUE
hist(iowa_anom)
shapiro.test(iowa_anom)

#Iowa precipitation values
iowa_prec = read.csv("file_path", header = TRUE)
head(iowa_prec)
tail(iowa_prec)
summary(iowa_prec)
iowa_prec_values = as.numeric(iowa_prec$precipitation)
iowa_prec$time = format(as.Date(iowa_prec$system.time_start, format = "%b %d, %Y"), "%d/%m/20%y")
iowa_prec_df = data.frame(
  iowa_prec.time = as.Date(iowa_prec$time, format = "%d/%m/%Y"), 
  iowa_prec_values = iowa_prec_values)
iowa_prec_df = na.omit(iowa_prec_df)
ggplot(data = iowa_prec_df, aes(x = iowa_prec.time, y = iowa_prec_values)) + 
  geom_point() +
  labs(x = "Date", y = "Precipitation", title = "Iowa Precipitation")
hist(iowa_prec_values)
shapiro.test(iowa_prec_values)

#Iowa Land Surface Temperature Values
iowa_lst = read.csv("file_path", header = TRUE)
head(iowa_lst)
tail(iowa_lst)
summary(iowa_lst)
iowa_lst_values = as.numeric(iowa_lst$LST_Day_1km)
iowa_lst$time = format(as.Date(iowa_lst$system.time_start, format = "%b %d, %Y"), "%d/%m/20%y")
iowa_lst_df = data.frame(
  iowa_lst.time = as.Date(iowa_lst$time, format = "%d/%m/%Y"), 
  iowa_lst_values = iowa_lst_values)
iowa_lst_df = na.omit(iowa_lst_df)
ggplot(data = iowa_lst_df, aes(x = iowa_lst.time, y = iowa_lst_values)) + 
  geom_point() +
  labs(x = "Date", y = "Land Surface Temperature", title = "Iowa Land Surface Temperature")
hist(iowa_lst_values)
shapiro.test(iowa_lst_values)


#Combining Datasets for Iowa
summary(iowa_ndvi_df)
iowa_ndvi_df$iowa_ndvi.START.DATE = as.Date(iowa_ndvi_df$iowa_ndvi.START.DATE, format="%d/%m/%Y")
iowa_ndvi_df$Month = format(iowa_ndvi_df$iowa_ndvi.START.DATE, format = "%m")
iowa_ndvi_df$Year = format(iowa_ndvi_df$iowa_ndvi.START.DATE, format = "%Y")
iowa_avg_ndvi = aggregate(cbind(iowa_ndvi.SAMPLE.VALUE, iowa_ndvi.ANOM.VALUE)~Month+Year, iowa_ndvi_df, mean)
iowa_avg_ndvi$Month = as.numeric(iowa_avg_ndvi$Month)
iowa_avg_ndvi = filter(iowa_avg_ndvi, Month >= 4, Month <= 11)

iowa_prec_df$iowa_prec.time = as.Date(iowa_prec_df$iowa_prec.time, format="%d/%m/%Y")
iowa_prec_df$Month = format(iowa_prec_df$iowa_prec.time, format = "%m")
iowa_prec_df$Year = format(iowa_prec_df$iowa_prec.time, format = "%Y")
iowa_prec_df$Month = as.numeric(iowa_prec_df$Month)

iowa_lst_df$iowa_lst.time = as.Date(iowa_lst_df$iowa_lst.time, format="%d/%m/%Y")
iowa_lst_df$Month = format(iowa_lst_df$iowa_lst.time, format = "%m")
iowa_lst_df$Year = format(iowa_lst_df$iowa_lst.time, format = "%Y")
iowa_lst_df$Month = as.numeric(iowa_lst_df$Month)
iowa_lst_df = aggregate(iowa_lst_values~Month+Year, iowa_lst_df, mean)

iowa_data = iowa_avg_ndvi %>% left_join(iowa_prec_df, by=c("Month","Year")) %>% left_join(iowa_lst_df, by=c("Month","Year"))
iowa_data = filter(iowa_data, !is.na(iowa_data$iowa_prec_values))


#Heilongjiang NDVI values
#Growing Season = April to September
hlj_ndvi = read.csv("file_path", skip = 14, header = TRUE)
head(hlj_ndvi)
tail(hlj_ndvi)
summary(hlj_ndvi)
hlj_ndvi$SAMPLE.VALUE = as.numeric(hlj_ndvi$SAMPLE.VALUE)
hlj_ndvi$ANOM.VALUE = as.numeric(hlj_ndvi$ANOM.VALUE)
hlj_ndvi$START.DATE = format(as.Date(hlj_ndvi$START.DATE, "20%y-%m-%d"), "%d/%m/20%y")
hlj_ndvi_df = data.frame(
  hlj_ndvi.START.DATE = as.Date(hlj_ndvi$START.DATE, format = "%d/%m/%Y"), 
  hlj_ndvi.SAMPLE.VALUE = hlj_ndvi$SAMPLE.VALUE, 
  hlj_ndvi.ANOM.VALUE = hlj_ndvi$ANOM.VALUE)
hlj_ndvi_df = na.omit(hlj_ndvi_df)
ggplot(data = hlj_ndvi_df, aes(x = hlj_ndvi.START.DATE, y = hlj_ndvi.ANOM.VALUE)) + 
  geom_point() +
  labs(x = "Date", y = "NDVI", title = "Heilongjiang NDVI Anomalies")
hlj_anom = hlj_ndvi_df$hlj_ndvi.ANOM.VALUE
hist(hlj_anom)
shapiro.test(hlj_anom)

#Heilongjiang precipitation values
hlj_prec = read.csv("file_path", header = TRUE)
head(hlj_prec)
tail(hlj_prec)
summary(hlj_prec)
hlj_prec_values = as.numeric(hlj_prec$precipitation)
hlj_prec$time = format(as.Date(hlj_prec$system.time_start, format = "%b %d, %Y"), "%d/%m/20%y")
hlj_prec_df = data.frame(
  hlj_prec.time = as.Date(hlj_prec$time, format = "%d/%m/%Y"), 
  hlj_prec_values = hlj_prec_values)
hlj_prec_df = na.omit(hlj_prec_df)
ggplot(data = hlj_prec_df, aes(x = hlj_prec.time, y = hlj_prec_values)) + 
  geom_point() +
  labs(x = "Date", y = "Precipitation", title = "Heilongjiang Precipitation")
hist(hlj_prec_values)
shapiro.test(hlj_prec_values)

#Heilongjiang Land Surface Temperature Values
hlj_lst = read.csv("file_path", header = TRUE)
head(hlj_lst)
tail(hlj_lst)
summary(hlj_lst)
hlj_lst_values = as.numeric(hlj_lst$LST_Day_1km)
hlj_lst$time = format(as.Date(hlj_lst$system.time_start, format = "%b %d, %Y"), "%d/%m/20%y")
hlj_lst_df = data.frame(
  hlj_lst.time = as.Date(hlj_lst$time, format = "%d/%m/%Y"), 
  hlj_lst_values = hlj_lst_values)
hlj_lst_df = na.omit(hlj_lst_df)
ggplot(data = hlj_lst_df, aes(x = hlj_lst.time, y = hlj_lst_values)) + 
  geom_point() +
  labs(x = "Date", y = "Land Surface Temperature", title = "Heilongjiang Land Surface Temperature")
hist(hlj_lst_values)
shapiro.test(hlj_lst_values)

#Combining Datasets for Heilongjiang
summary(hlj_ndvi_df)
hlj_ndvi_df$hlj_ndvi.START.DATE = as.Date(hlj_ndvi_df$hlj_ndvi.START.DATE, format="%d/%m/%Y")
hlj_ndvi_df$Month = format(hlj_ndvi_df$hlj_ndvi.START.DATE, format = "%m")
hlj_ndvi_df$Year = format(hlj_ndvi_df$hlj_ndvi.START.DATE, format = "%Y")
hlj_avg_ndvi = aggregate(cbind(hlj_ndvi.SAMPLE.VALUE, hlj_ndvi.ANOM.VALUE)~Month+Year, hlj_ndvi_df, mean)
hlj_avg_ndvi$Month = as.numeric(hlj_avg_ndvi$Month)
hlj_avg_ndvi = filter(hlj_avg_ndvi, Month >= 4, Month <= 9)

hlj_prec_df$hlj_prec.time = as.Date(hlj_prec_df$hlj_prec.time, format="%d/%m/%Y")
hlj_prec_df$Month = format(hlj_prec_df$hlj_prec.time, format = "%m")
hlj_prec_df$Year = format(hlj_prec_df$hlj_prec.time, format = "%Y")
hlj_prec_df$Month = as.numeric(hlj_prec_df$Month)

hlj_lst_df$hlj_lst.time = as.Date(hlj_lst_df$hlj_lst.time, format="%d/%m/%Y")
hlj_lst_df$Month = format(hlj_lst_df$hlj_lst.time, format = "%m")
hlj_lst_df$Year = format(hlj_lst_df$hlj_lst.time, format = "%Y")
hlj_lst_df$Month = as.numeric(hlj_lst_df$Month)
hlj_lst_df = aggregate(hlj_lst_values~Month+Year, hlj_lst_df, mean)

hlj_data = hlj_avg_ndvi %>% left_join(hlj_prec_df, by=c("Month","Year")) %>% left_join(hlj_lst_df, by=c("Month","Year"))
hlj_data = filter(hlj_data, !is.na(hlj_data$hlj_prec_values))

#Mato Grosso NDVI values
#Growing Season = October to May
mg_ndvi = read.csv("file_path", skip = 14, header = TRUE)
head(mg_ndvi)
tail(mg_ndvi)
summary(mg_ndvi)
mg_ndvi$SAMPLE.VALUE = as.numeric(mg_ndvi$SAMPLE.VALUE)
mg_ndvi$ANOM.VALUE = as.numeric(mg_ndvi$ANOM.VALUE)
mg_ndvi$START.DATE = format(as.Date(mg_ndvi$START.DATE, "20%y-%m-%d"), "%d/%m/20%y")
mg_ndvi_df = data.frame(
  mg_ndvi.START.DATE = as.Date(mg_ndvi$START.DATE, format = "%d/%m/%Y"),
  mg_ndvi.SAMPLE.VALUE = mg_ndvi$SAMPLE.VALUE,
  mg_ndvi.ANOM.VALUE = mg_ndvi$ANOM.VALUE
)
mg_ndvi_df = na.omit(mg_ndvi_df)
ggplot(data = mg_ndvi_df, aes(x = mg_ndvi.START.DATE, y = mg_ndvi.ANOM.VALUE)) + 
  geom_point() +
  labs(x = "Date", y = "NDVI", title = "Mato Grosso NDVI Anomalies")
mg_anom = mg_ndvi_df$mg_ndvi.ANOM.VALUE
hist(mg_anom)
shapiro.test(mg_anom)

#Mato Grosso precipitation values
mg_prec = read.csv("file_path", header = TRUE)
head(mg_prec)
tail(mg_prec)
summary(mg_prec)
mg_prec_values = as.numeric(mg_prec$precipitation)
mg_prec$time = format(as.Date(mg_prec$system.time_start, format = "%b %d, %Y"), "%d/%m/20%y")
mg_prec_df = data.frame(
  mg_prec.time = as.Date(mg_prec$time, format = "%d/%m/%Y"), 
  mg_prec_values = mg_prec_values)
mg_prec_df = na.omit(mg_prec_df)
ggplot(data = mg_prec_df, aes(x = mg_prec.time, y = mg_prec_values)) + 
  geom_point() +
  labs(x = "Date", y = "Precipitation", title = "Mato Grosso Precipitation")
hist(mg_prec_values)
shapiro.test(mg_prec_values)

#Mato Grosso Land Surface Temperature Values
mg_lst = read.csv("file_path", header = TRUE)
head(mg_lst)
tail(mg_lst)
summary(mg_lst)
mg_lst_values = as.numeric(mg_lst$LST_Day_1km)
mg_lst$time = format(as.Date(mg_lst$system.time_start, format = "%b %d, %Y"), "%d/%m/20%y")
mg_lst_df = data.frame(
  mg_lst.time = as.Date(mg_lst$time, format = "%d/%m/%Y"), 
  mg_lst_values = mg_lst_values)
mg_lst_df = na.omit(mg_lst_df)
ggplot(data = mg_lst_df, aes(x = mg_lst.time, y = mg_lst_values)) + 
  geom_point() +
  labs(x = "Date", y = "Land Surface Temperature", title = "Mato Grosso Land Surface Temperature")
hist(mg_lst_values)
shapiro.test(mg_lst_values)

#Combining Datasets for Mato Grosso
summary(mg_ndvi_df)
mg_ndvi_df$mg_ndvi.START.DATE = as.Date(mg_ndvi_df$mg_ndvi.START.DATE, format="%d/%m/%Y")
mg_ndvi_df$Month = format(mg_ndvi_df$mg_ndvi.START.DATE, format = "%m")
mg_ndvi_df$Year = format(mg_ndvi_df$mg_ndvi.START.DATE, format = "%Y")
mg_avg_ndvi = aggregate(cbind(mg_ndvi.SAMPLE.VALUE, mg_ndvi.ANOM.VALUE)~Month+Year, mg_ndvi_df, mean)
mg_avg_ndvi$Month = as.numeric(mg_avg_ndvi$Month)
mg_avg_ndvi = filter(mg_avg_ndvi, Month <= 5 | Month >= 10)

mg_prec_df$mg_prec.time = as.Date(mg_prec_df$mg_prec.time, format="%d/%m/%Y")
mg_prec_df$Month = format(mg_prec_df$mg_prec.time, format = "%m")
mg_prec_df$Year = format(mg_prec_df$mg_prec.time, format = "%Y")
mg_prec_df$Month = as.numeric(mg_prec_df$Month)

mg_lst_df$mg_lst.time = as.Date(mg_lst_df$mg_lst.time, format="%d/%m/%Y")
mg_lst_df$Month = format(mg_lst_df$mg_lst.time, format = "%m")
mg_lst_df$Year = format(mg_lst_df$mg_lst.time, format = "%Y")
mg_lst_df$Month = as.numeric(mg_lst_df$Month)
mg_lst_df = aggregate(mg_lst_values~Month+Year, mg_lst_df, mean)

mg_data = mg_avg_ndvi %>% left_join(mg_prec_df, by=c("Month","Year")) %>% left_join(mg_lst_df, by=c("Month","Year"))
mg_data = filter(mg_data, !is.na(mg_data$mg_prec_values))


#Cordoba NDVI values
#Growing Season = September to March
cdb_ndvi = read.csv("file_path", skip = 14, header = TRUE)
head(cdb_ndvi)
tail(cdb_ndvi)
summary(cdb_ndvi)
cdb_ndvi$SAMPLE.VALUE = as.numeric(cdb_ndvi$SAMPLE.VALUE)
cdb_ndvi$ANOM.VALUE = as.numeric(cdb_ndvi$ANOM.VALUE)
cdb_ndvi$START.DATE = format(as.Date(cdb_ndvi$START.DATE, "%d/%m/%y"), "%d/%m/20%y")
cdb_ndvi_df = data.frame(
  cdb_ndvi.START.DATE = as.Date(cdb_ndvi$START.DATE, format = "%d/%m/%Y"),
  cdb_ndvi.SAMPLE.VALUE = cdb_ndvi$SAMPLE.VALUE,
  cdb_ndvi.ANOM.VALUE = cdb_ndvi$ANOM.VALUE
)
cdb_ndvi_df = na.omit(cdb_ndvi_df)
ggplot(data = cdb_ndvi_df, aes(x = cdb_ndvi.START.DATE, y = cdb_ndvi.ANOM.VALUE)) + 
  geom_point() +
  labs(x = "Date", y = "NDVI", title = "Cordoba NDVI Anomalies")
cdb_anom = cdb_ndvi_df$cdb_ndvi.ANOM.VALUE
hist(cdb_anom)
shapiro.test(cdb_anom)

#Cordoba precipitation values
cdb_prec = read.csv("file_path", header = TRUE)
head(cdb_prec)
tail(cdb_prec)
summary(cdb_prec)
cdb_prec_values = as.numeric(cdb_prec$precipitation)
cdb_prec$time = format(as.Date(cdb_prec$system.time_start, format = "%b %d, %Y"), "%d/%m/20%y")
cdb_prec_df = data.frame(
  cdb_prec.time = as.Date(cdb_prec$time, format = "%d/%m/%Y"), 
  cdb_prec_values = cdb_prec_values)
cdb_prec_df = na.omit(cdb_prec_df)
ggplot(data = cdb_prec_df, aes(x = cdb_prec.time, y = cdb_prec_values)) + 
  geom_point() +
  labs(x = "Date", y = "Precipitation", title = "Cordoba Precipitation")
hist(cdb_prec_values)
shapiro.test(cdb_prec_values)

#Cordoba Land Surface Temperature Values
cdb_lst = read.csv("file_path", header = TRUE)
head(cdb_lst)
tail(cdb_lst)
summary(cdb_lst)
cdb_lst_values = as.numeric(cdb_lst$LST_Day_1km)
cdb_lst$time = format(as.Date(cdb_lst$system.time_start, format = "%b %d, %Y"), "%d/%m/20%y")
cdb_lst_df = data.frame(
  cdb_lst.time = as.Date(cdb_lst$time, format = "%d/%m/%Y"), 
  cdb_lst_values = cdb_lst_values)
cdb_lst_df = na.omit(cdb_lst_df)
ggplot(data = cdb_lst_df, aes(x = cdb_lst.time, y = cdb_lst_values)) + 
  geom_point() +
  labs(x = "Date", y = "Land Surface Temperature", title = "Cordoba Land Surface Temperature")
hist(cdb_lst_values)
shapiro.test(cdb_lst_values)

#Combining Datasets for Cordoba
summary(cdb_ndvi_df)
cdb_ndvi_df$cdb_ndvi.START.DATE = as.Date(cdb_ndvi_df$cdb_ndvi.START.DATE, format="%d/%m/%Y")
cdb_ndvi_df$Month = format(cdb_ndvi_df$cdb_ndvi.START.DATE, format = "%m")
cdb_ndvi_df$Year = format(cdb_ndvi_df$cdb_ndvi.START.DATE, format = "%Y")
cdb_avg_ndvi = aggregate(cbind(cdb_ndvi.SAMPLE.VALUE, cdb_ndvi.ANOM.VALUE)~Month+Year, cdb_ndvi_df, mean)
cdb_avg_ndvi$Month = as.numeric(cdb_avg_ndvi$Month)
cdb_avg_ndvi = filter(cdb_avg_ndvi, Month <= 3 | Month >= 9)

cdb_prec_df$cdb_prec.time = as.Date(cdb_prec_df$cdb_prec.time, format="%d/%m/%Y")
cdb_prec_df$Month = format(cdb_prec_df$cdb_prec.time, format = "%m")
cdb_prec_df$Year = format(cdb_prec_df$cdb_prec.time, format = "%Y")
cdb_prec_df$Month = as.numeric(cdb_prec_df$Month)

cdb_lst_df$cdb_lst.time = as.Date(cdb_lst_df$cdb_lst.time, format="%d/%m/%Y")
cdb_lst_df$Month = format(cdb_lst_df$cdb_lst.time, format = "%m")
cdb_lst_df$Year = format(cdb_lst_df$cdb_lst.time, format = "%Y")
cdb_lst_df$Month = as.numeric(cdb_lst_df$Month)
cdb_lst_df = aggregate(cdb_lst_values~Month+Year, cdb_lst_df, mean)

cdb_data = cdb_avg_ndvi %>% left_join(cdb_prec_df, by=c("Month","Year")) %>% left_join(cdb_lst_df, by=c("Month","Year"))
cdb_data = filter(cdb_data, !is.na(cdb_data$cdb_prec_values))


#Poltava Oblast NDVI values
#Growing Season = April to November
pltv_ndvi = read.csv("file_path", skip = 14, header = TRUE)
head(pltv_ndvi)
tail(pltv_ndvi)
summary(pltv_ndvi)
pltv_ndvi$SAMPLE.VALUE = as.numeric(pltv_ndvi$SAMPLE.VALUE)
pltv_ndvi$ANOM.VALUE = as.numeric(pltv_ndvi$ANOM.VALUE)
pltv_ndvi$START.DATE = format(as.Date(pltv_ndvi$START.DATE, "20%y-%m-%d"), "%d/%m/20%y")
pltv_ndvi_df = data.frame(
  pltv_ndvi.START.DATE = as.Date(pltv_ndvi$START.DATE, format = "%d/%m/%Y"), 
  pltv_ndvi.SAMPLE.VALUE = pltv_ndvi$SAMPLE.VALUE, 
  pltv_ndvi.ANOM.VALUE = pltv_ndvi$ANOM.VALUE)
pltv_ndvi_df = na.omit(pltv_ndvi_df)
ggplot(data = pltv_ndvi_df, aes(x = pltv_ndvi.START.DATE, y = pltv_ndvi.ANOM.VALUE)) + 
  geom_point() +
  labs(x = "Date", y = "NDVI", title = "Poltava Oblast NDVI Anomalies")
pltv_anom = pltv_ndvi_df$pltv_ndvi.ANOM.VALUE
hist(pltv_anom)
shapiro.test(pltv_anom)

#Poltava Oblast precipitation values
pltv_prec = read.csv("file_path", header = TRUE)
head(pltv_prec)
tail(pltv_prec)
summary(pltv_prec)
pltv_prec_values = as.numeric(pltv_prec$precipitation)
pltv_prec$time = format(as.Date(pltv_prec$system.time_start, format = "%b %d, %Y"), "%d/%m/20%y")
pltv_prec_df = data.frame(
  pltv_prec.time = as.Date(pltv_prec$time, format = "%d/%m/%Y"), 
  pltv_prec_values = pltv_prec_values)
pltv_prec_df = na.omit(pltv_prec_df)
ggplot(data = pltv_prec_df, aes(x = pltv_prec.time, y = pltv_prec_values)) + 
  geom_point() +
  labs(x = "Date", y = "Precipitation", title = "Poltava Oblast Precipitation")
hist(pltv_prec_values)
shapiro.test(pltv_prec_values)

#Poltava Oblast Land Surface Temperature Values
pltv_lst = read.csv("file_path", header = TRUE)
head(pltv_lst)
tail(pltv_lst)
summary(pltv_lst)
pltv_lst_values = as.numeric(pltv_lst$LST_Day_1km)
pltv_lst$time = format(as.Date(pltv_lst$system.time_start, format = "%b %d, %Y"), "%d/%m/20%y")
pltv_lst_df = data.frame(
  pltv_lst.time = as.Date(pltv_lst$time, format = "%d/%m/%Y"), 
  pltv_lst_values = pltv_lst_values)
pltv_lst_df = na.omit(pltv_lst_df)
ggplot(data = pltv_lst_df, aes(x = pltv_lst.time, y = pltv_lst_values)) + 
  geom_point() +
  labs(x = "Date", y = "Land Surface Temperature", title = "Poltava Oblast Land Surface Temperature")
hist(pltv_lst_values)
shapiro.test(pltv_lst_values)

#Combining Datasets for Poltava Oblast
summary(pltv_ndvi_df)
pltv_ndvi_df$pltv_ndvi.START.DATE = as.Date(pltv_ndvi_df$pltv_ndvi.START.DATE, format="%d/%m/%Y")
pltv_ndvi_df$Month = format(pltv_ndvi_df$pltv_ndvi.START.DATE, format = "%m")
pltv_ndvi_df$Year = format(pltv_ndvi_df$pltv_ndvi.START.DATE, format = "%Y")
pltv_avg_ndvi = aggregate(cbind(pltv_ndvi.SAMPLE.VALUE, pltv_ndvi.ANOM.VALUE)~Month+Year, pltv_ndvi_df, mean)
pltv_avg_ndvi$Month = as.numeric(pltv_avg_ndvi$Month)
pltv_avg_ndvi = filter(pltv_avg_ndvi, Month >= 4, Month <= 11)

pltv_prec_df$pltv_prec.time = as.Date(pltv_prec_df$pltv_prec.time, format="%d/%m/%Y")
pltv_prec_df$Month = format(pltv_prec_df$pltv_prec.time, format = "%m")
pltv_prec_df$Year = format(pltv_prec_df$pltv_prec.time, format = "%Y")
pltv_prec_df$Month = as.numeric(pltv_prec_df$Month)

pltv_lst_df$pltv_lst.time = as.Date(pltv_lst_df$pltv_lst.time, format="%d/%m/%Y")
pltv_lst_df$Month = format(pltv_lst_df$pltv_lst.time, format = "%m")
pltv_lst_df$Year = format(pltv_lst_df$pltv_lst.time, format = "%Y")
pltv_lst_df$Month = as.numeric(pltv_lst_df$Month)
pltv_lst_df = aggregate(pltv_lst_values~Month+Year, pltv_lst_df, mean)

pltv_data = pltv_avg_ndvi %>% left_join(pltv_prec_df, by=c("Month","Year")) %>% left_join(pltv_lst_df, by=c("Month","Year"))
pltv_data = filter(pltv_data, !is.na(pltv_data$pltv_prec_values))


#Iowa Time Series Analysis
iowa_monthly_stats = iowa_data %>%
  group_by(Month) %>%
  summarise(Mean_Value = as.numeric(mean(iowa_ndvi.SAMPLE.VALUE, na.rm = TRUE)),
            Min_Value = as.numeric(min(iowa_ndvi.SAMPLE.VALUE, na.rm = TRUE)),
            Max_Value = as.numeric(max(iowa_ndvi.SAMPLE.VALUE, na.rm = TRUE)))

iowa_ts = iowa_data[, c("Month", "Year", "iowa_ndvi.SAMPLE.VALUE")]
iowa_filtered_data = iowa_ts %>%
  filter(Year >= 2018 & Year <= 2020)
iowa_combined_data = iowa_monthly_stats %>%
  left_join(iowa_filtered_data, by = "Month")

ggplot(iowa_combined_data, aes(x = Month)) +
  geom_ribbon(aes(ymin = Min_Value, ymax = Max_Value), fill = "gray90", alpha = 0.3) +
  geom_line(data = iowa_monthly_stats, aes(x = Month, y = Mean_Value), color = "black", linetype = "dashed") +
  geom_line(aes(y = iowa_ndvi.SAMPLE.VALUE, color = factor(Year))) +
  labs(title = "Iowa 2018-2020 NDVI", x = "Month", y = "NDVI") +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  scale_color_manual(name = "Legend", values = c("2018" = "red", "2019" = "purple", "2020" = "green", "Mean_Value" = "black", "Min_Value" = "gray"),
                     labels = c("2018", "2019", "2020", "Mean NDVI (2000 - 2021)", "Min/Max (2000 - 2021)")) +
  theme_minimal()

ggplot(iowa_ndvi_df %>% filter(iowa_ndvi.START.DATE <= as.Date("2021-12-31")), aes(x = iowa_ndvi.START.DATE, y = iowa_ndvi.ANOM.VALUE)) +
  geom_segment(aes(xend = iowa_ndvi.START.DATE, yend = 0, color = ifelse(iowa_ndvi.ANOM.VALUE >= 0, "Positive", "Negative")), size = 0.5) +
  labs(title = "Iowa NDVI Anomalies",
       x = "Date", y = "Value") +
  theme_minimal() +
  scale_color_manual(name = "Legend", values = c("Positive" = "blue", "Negative" = "red")) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y")

#Heilongjiang Time Series Analysis
hlj_monthly_stats = hlj_data %>%
  group_by(Month) %>%
  summarise(Mean_Value = as.numeric(mean(hlj_ndvi.SAMPLE.VALUE, na.rm = TRUE)),
            Min_Value = as.numeric(min(hlj_ndvi.SAMPLE.VALUE, na.rm = TRUE)),
            Max_Value = as.numeric(max(hlj_ndvi.SAMPLE.VALUE, na.rm = TRUE)))

hlj_ts = hlj_data[, c("Month", "Year", "hlj_ndvi.SAMPLE.VALUE")]
hlj_filtered_data = hlj_ts %>%
  filter(Year >= 2018 & Year <= 2020)
hlj_combined_data = hlj_monthly_stats %>%
  left_join(hlj_filtered_data, by = "Month")

ggplot(hlj_combined_data, aes(x = Month)) +
  geom_ribbon(aes(ymin = Min_Value, ymax = Max_Value), fill = "gray90", alpha = 0.3) +
  geom_line(data = hlj_monthly_stats, aes(x = Month, y = Mean_Value), color = "black", linetype = "dashed") +
  geom_line(aes(y = hlj_ndvi.SAMPLE.VALUE, color = factor(Year))) +
  labs(title = "Heilongjiang 2018-2020 NDVI", x = "Month", y = "NDVI") +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  scale_color_manual(name = "Legend", values = c("2018" = "red", "2019" = "purple", "2020" = "green", "Mean_Value" = "black", "Min_Value" = "gray"),
                     labels = c("2018", "2019", "2020", "Mean NDVI (2000 - 2021)", "Min/Max (2000 - 2021)")) +
  theme_minimal()

ggplot(hlj_ndvi_df %>% filter(hlj_ndvi.START.DATE <= as.Date("2021-12-31")), aes(x = hlj_ndvi.START.DATE, y = hlj_ndvi.ANOM.VALUE)) +
  geom_segment(aes(xend = hlj_ndvi.START.DATE, yend = 0, color = ifelse(hlj_ndvi.ANOM.VALUE >= 0, "Positive", "Negative")), size = 0.5) +
  labs(title = "Heilongjiang NDVI Anomalies",
       x = "Date", y = "Value") +
  theme_minimal() +
  scale_color_manual(name = "Legend", values = c("Positive" = "blue", "Negative" = "red")) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y")

#Mato Grosso Time Series Analysis
mg_monthly_stats = mg_data %>%
  group_by(Month) %>%
  summarise(Mean_Value = as.numeric(mean(mg_ndvi.SAMPLE.VALUE, na.rm = TRUE)),
            Min_Value = as.numeric(min(mg_ndvi.SAMPLE.VALUE, na.rm = TRUE)),
            Max_Value = as.numeric(max(mg_ndvi.SAMPLE.VALUE, na.rm = TRUE)))

mg_ts = mg_data[, c("Month", "Year", "mg_ndvi.SAMPLE.VALUE")]
mg_ts$Year = as.numeric(mg_ts$Year)
mg_ts$Year = ifelse(mg_ts$Month >= 10, mg_ts$Year + 1, mg_ts$Year)
mg_filtered_data = mg_ts %>%
  filter(Year >= 2019 & Year <= 2021)
mg_combined_data = mg_monthly_stats %>%
  right_join(mg_filtered_data, by = "Month")
mg_combined_data = mg_combined_data %>%
  arrange(match(Month, mg_filtered_data$Month))
mg_combined_data = mg_combined_data %>%
  mutate(Month = factor(Month,
                        levels = c(10:12, 1:9),
                        labels = c(month.abb[10:12], month.abb[1:9])))

ggplot(mg_combined_data, aes(x = Month)) +
  geom_ribbon(aes(ymin = Min_Value, ymax = Max_Value, group = factor(Year)), fill = "gray90", alpha = 0.3) +
  geom_line(data = mg_combined_data, aes(x = Month, y = Mean_Value, color = "black", group = factor(Year)), linetype = "dashed") +
  geom_line(aes(y = mg_ndvi.SAMPLE.VALUE, color = factor(Year), group = Year)) +
  labs(title = "Mato Grosso 2018-2021 NDVI", x = "Month", y = "NDVI") +
  scale_color_manual(name = "Legend", values = c("2019" = "purple", "2020" = "green", "2021" = "blue", "Mean_Value" = "black", "Min_Value" = "gray"),
                     labels = c("2018-2019", "2019-2020", "2020-2021", "Mean NDVI (2000 - 2021)", "Min/Max (2000 - 2021)")) +
  theme_minimal()

ggplot(mg_ndvi_df %>% filter(mg_ndvi.START.DATE <= as.Date("2021-12-31")), aes(x = mg_ndvi.START.DATE, y = mg_ndvi.ANOM.VALUE)) +
  geom_segment(aes(xend = mg_ndvi.START.DATE, yend = 0, color = ifelse(mg_ndvi.ANOM.VALUE >= 0, "Positive", "Negative")), size = 0.5) +
  labs(title = "Mato Grosso NDVI Anomalies",
       x = "Date", y = "Value") +
  theme_minimal() +
  scale_color_manual(name = "Legend", values = c("Positive" = "blue", "Negative" = "red")) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y")

#Cordoba Time Series Analysis
cdb_monthly_stats = cdb_data %>%
  group_by(Month) %>%
  summarise(Mean_Value = as.numeric(mean(cdb_ndvi.SAMPLE.VALUE, na.rm = TRUE)),
            Min_Value = as.numeric(min(cdb_ndvi.SAMPLE.VALUE, na.rm = TRUE)),
            Max_Value = as.numeric(max(cdb_ndvi.SAMPLE.VALUE, na.rm = TRUE)))

cdb_ts = cdb_data[, c("Month", "Year", "cdb_ndvi.SAMPLE.VALUE")]
cdb_ts$Year = as.numeric(cdb_ts$Year)
cdb_ts$Year = ifelse(cdb_ts$Month >= 9, cdb_ts$Year + 1, cdb_ts$Year)
cdb_filtered_data = cdb_ts %>%
  filter(Year >= 2019 & Year <= 2021)
cdb_combined_data = cdb_monthly_stats %>%
  right_join(cdb_filtered_data, by = "Month")
cdb_combined_data = cdb_combined_data %>%
  arrange(match(Month, cdb_filtered_data$Month))
cdb_combined_data = cdb_combined_data %>%
  mutate(Month = factor(Month,
                        levels = c(9:12, 1:8),
                        labels = c(month.abb[9:12], month.abb[1:8])))

ggplot(cdb_combined_data, aes(x = Month)) +
  geom_ribbon(aes(ymin = Min_Value, ymax = Max_Value, group = factor(Year)), fill = "gray90", alpha = 0.3) +
  geom_line(data = cdb_combined_data, aes(x = Month, y = Mean_Value, color = "black", group = factor(Year)), linetype = "dashed") +
  geom_line(aes(y = cdb_ndvi.SAMPLE.VALUE, color = factor(Year), group = Year)) +
  labs(title = "Cordoba 2018-2021 NDVI", x = "Month", y = "NDVI") +
  scale_color_manual(name = "Legend", values = c("2019" = "purple", "2020" = "green", "2021" = "blue", "Mean_Value" = "black", "Min_Value" = "gray"),
                     labels = c("2018-2019", "2019-2020", "2020-2021", "Mean NDVI (2000 - 2021)", "Min/Max (2000 - 2021)")) +
  theme_minimal()

ggplot(cdb_ndvi_df %>% filter(cdb_ndvi.START.DATE <= as.Date("2021-12-31")), aes(x = cdb_ndvi.START.DATE, y = cdb_ndvi.ANOM.VALUE)) +
  geom_segment(aes(xend = cdb_ndvi.START.DATE, yend = 0, color = ifelse(cdb_ndvi.ANOM.VALUE >= 0, "Positive", "Negative")), size = 0.5) +
  labs(title = "Cordoba NDVI Anomalies",
       x = "Date", y = "Value") +
  theme_minimal() +
  scale_color_manual(name = "Legend", values = c("Positive" = "blue", "Negative" = "red")) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y")

#Poltava Oblast Time Series Analysis
pltv_monthly_stats = pltv_data %>%
  group_by(Month) %>%
  summarise(Mean_Value = as.numeric(mean(pltv_ndvi.SAMPLE.VALUE, na.rm = TRUE)),
            Min_Value = as.numeric(min(pltv_ndvi.SAMPLE.VALUE, na.rm = TRUE)),
            Max_Value = as.numeric(max(pltv_ndvi.SAMPLE.VALUE, na.rm = TRUE)))

pltv_ts = pltv_data[, c("Month", "Year", "pltv_ndvi.SAMPLE.VALUE")]
pltv_filtered_data = pltv_ts %>%
  filter(Year >= 2018 & Year <= 2020)
pltv_combined_data = pltv_monthly_stats %>%
  left_join(pltv_filtered_data, by = "Month")

ggplot(pltv_combined_data, aes(x = Month)) +
  geom_ribbon(aes(ymin = Min_Value, ymax = Max_Value), fill = "gray", alpha = 0.3) +
  geom_line(data = pltv_monthly_stats, aes(x = Month, y = Mean_Value), color = "black", linetype = "dashed") +
  geom_line(aes(y = pltv_ndvi.SAMPLE.VALUE, color = factor(Year))) +
  labs(title = "Poltava Oblast 2018-2020 NDVI", x = "Month", y = "NDVI") +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  scale_color_manual(name = "Legend", values = c("2018" = "red", "2019" = "purple", "2020" = "green", "Mean_Value" = "black", "Min_Value" = "gray"),
                     labels = c("2018", "2019", "2020", "Mean NDVI (2000 - 2021)", "Min/Max (2000 - 2021)")) +
  theme_minimal()

ggplot(pltv_ndvi_df %>% filter(pltv_ndvi.START.DATE <= as.Date("2021-12-31")), aes(x = pltv_ndvi.START.DATE, y = pltv_ndvi.ANOM.VALUE)) +
  geom_segment(aes(xend = pltv_ndvi.START.DATE, yend = 0, color = ifelse(pltv_ndvi.ANOM.VALUE >= 0, "Positive", "Negative")), size = 0.5) +
  labs(title = "Poltava Oblast NDVI Anomalies",
       x = "Date", y = "Value") +
  theme_minimal() +
  scale_color_manual(name = "Legend", values = c("Positive" = "blue", "Negative" = "red")) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y")


#Exploratory data analysis on all 5 regions
bp_data_1 = list("Iowa" = iowa_data$iowa_ndvi.SAMPLE.VALUE, 
                 "Heilongjiang" = hlj_data$hlj_ndvi.SAMPLE.VALUE, 
                 "Mato Grosso" = mg_data$mg_ndvi.SAMPLE.VALUE, 
                 "Cordoba" = cdb_data$cdb_ndvi.SAMPLE.VALUE, 
                 "Poltava Oblast" = pltv_data$pltv_ndvi.SAMPLE.VALUE)
colors = c("light blue", "coral", "light green", "pink", "orange")
boxplot(bp_data_1, main = "NDVI of Regions", col = colors, xaxt = "n") 
axis(side = 1, labels = FALSE)
text(x = 1:length(bp_data_1),
     y = par("usr")[3] - 0.02, 
     labels = names(bp_data_1), 
     xpd = NA,
     adj = 1,
     srt = 45)


bp_data_2 = list("Iowa" = iowa_data$iowa_prec_values, 
                 "Heilongjiang" = hlj_data$hlj_prec_values, 
                 "Mato Grosso" = mg_data$mg_prec_values, 
                 "Cordoba" = cdb_data$cdb_prec_values, 
                 "Poltava Oblast" = pltv_data$pltv_prec_values)
colors = c("light blue", "coral", "light green", "pink", "orange")
boxplot(bp_data_2, main = "Precipitation Values of Regions", col = colors, xaxt = "n") 
axis(side = 1, labels = FALSE)
text(x = 1:length(bp_data_2),
     y = par("usr")[3] - 0.02, 
     labels = names(bp_data_2), 
     xpd = NA,
     adj = 1,
     srt = 45)


bp_data_3 = list("Iowa" = iowa_data$iowa_lst_values, 
                 "Heilongjiang" = hlj_data$hlj_lst_values, 
                 "Mato Grosso" = mg_data$mg_lst_values, 
                 "Cordoba" = cdb_data$cdb_lst_values, 
                 "Poltava Oblast" = pltv_data$pltv_lst_values)
colors = c("light blue", "coral", "light green", "pink", "orange")
boxplot(bp_data_3, main = "Land Surface Temperature of Regions", col = colors, xaxt = "n") 
axis(side = 1, labels = FALSE)
text(x = 1:length(bp_data_3),
     y = par("usr")[3] - 0.9, 
     labels = names(bp_data_3), 
     xpd = NA,
     adj = 1,
     srt = 45)


bp_data_4 = list("Iowa" = iowa_data$iowa_ndvi.ANOM.VALUE, 
                 "Heilongjiang" = hlj_data$hlj_ndvi.ANOM.VALUE, 
                 "Mato Grosso" = mg_data$mg_ndvi.ANOM.VALUE, 
                 "Cordoba" = cdb_data$cdb_ndvi.ANOM.VALUE, 
                 "Poltava Oblast" = pltv_data$pltv_ndvi.ANOM.VALUE)
colors = c("light blue", "coral", "light green", "pink", "orange")
boxplot(bp_data_4, main = "NDVI Anomalies of Regions", col = colors, xaxt = "n") 
axis(side = 1, labels = FALSE)
text(x = 1:length(bp_data_4),
     y = par("usr")[3] - 0.01, 
     labels = names(bp_data_1), 
     xpd = NA,
     adj = 1,
     srt = 45)


#Creating Annual Production Datasets
annual_pdtn = filter(regional_pdtn, Year >= 2000, Year <= 2021)

#Iowa
iowa_annual_prec_lst = aggregate(cbind(iowa_data$iowa_prec_values, iowa_data$iowa_lst_values)~Year, iowa_data, mean)
iowa_max_ndvi = aggregate(cbind(iowa_data$iowa_ndvi.SAMPLE.VALUE, iowa_data$iowa_ndvi.ANOM.VALUE)~Year, iowa_data, max)
iowa_min_ndvi_anom = aggregate(iowa_data$iowa_ndvi.ANOM.VALUE~Year, iowa_data, min)
iowa_annual_data = iowa_annual_prec_lst %>% right_join(iowa_max_ndvi, by="Year") %>% right_join(iowa_min_ndvi_anom, by="Year")
iowa_annual_df = cbind(iowa_annual_data, annual_pdtn$Iowa)
iowa_annual_df = iowa_annual_df %>% rename("Precipitation" = 2, "Land_Surface_Temp" = 3, "NDVI" = 4, "Max_NDVI_Anom" = 5, "Min_NDVI_Anom" = 6, "Production" = 7)
iowa_annual_df$Year = as.numeric(iowa_annual_df$Year)
iowa_annual_df$NDVI = as.numeric(iowa_annual_df$NDVI)
iowa_annual_df$Max_NDVI_Anom = as.numeric(iowa_annual_df$Max_NDVI_Anom)
iowa_annual_df$Min_NDVI_Anom = as.numeric(iowa_annual_df$Min_NDVI_Anom)
iowa_annual_df$Precipitation = as.numeric(iowa_annual_df$Precipitation)
iowa_annual_df$Land_Surface_Temp = as.numeric(iowa_annual_df$Land_Surface_Temp)
iowa_annual_df$Production = as.numeric(iowa_annual_df$Production)

#Heilongjiang
hlj_annual_prec_lst = aggregate(cbind(hlj_data$hlj_prec_values, hlj_data$hlj_lst_values)~Year, hlj_data, mean)
hlj_max_ndvi = aggregate(cbind(hlj_data$hlj_ndvi.SAMPLE.VALUE, hlj_data$hlj_ndvi.ANOM.VALUE)~Year, hlj_data, max)
hlj_min_ndvi_anom = aggregate(hlj_data$hlj_ndvi.ANOM.VALUE~Year, hlj_data, min)
hlj_annual_data = hlj_annual_prec_lst %>% right_join(hlj_max_ndvi, by="Year") %>% right_join(hlj_min_ndvi_anom, by="Year")
hlj_annual_df = cbind(hlj_annual_data, annual_pdtn$Heilongjiang)
hlj_annual_df = hlj_annual_df %>% rename("Precipitation" = 2, "Land_Surface_Temp" = 3, "NDVI" = 4, "Max_NDVI_Anom" = 5, "Min_NDVI_Anom" = 6, "Production" = 7)
hlj_annual_df$Year = as.numeric(hlj_annual_df$Year)
hlj_annual_df$NDVI = as.numeric(hlj_annual_df$NDVI)
hlj_annual_df$Max_NDVI_Anom = as.numeric(hlj_annual_df$Max_NDVI_Anom)
hlj_annual_df$Min_NDVI_Anom = as.numeric(hlj_annual_df$Min_NDVI_Anom)
hlj_annual_df$Precipitation = as.numeric(hlj_annual_df$Precipitation)
hlj_annual_df$Land_Surface_Temp = as.numeric(hlj_annual_df$Land_Surface_Temp)
hlj_annual_df$Production = as.numeric(hlj_annual_df$Production)

#Mato Grosso
mg_data$Year = as.integer(mg_data$Year)
mg_data = mg_data %>% mutate(pdtn_cycle = ifelse(Month >= 10, Year+1, Year))
mg_annual_prec_lst = aggregate(cbind(mg_data$mg_prec_values, mg_data$mg_lst_values)~pdtn_cycle, mg_data, mean)
mg_max_ndvi = aggregate(cbind(mg_data$mg_ndvi.SAMPLE.VALUE, mg_data$mg_ndvi.ANOM.VALUE)~pdtn_cycle, mg_data, max)
mg_min_ndvi_anom = aggregate(mg_data$mg_ndvi.ANOM.VALUE~pdtn_cycle, mg_data, min)
mg_annual_data = mg_annual_prec_lst %>% right_join(mg_max_ndvi, by="pdtn_cycle") %>% right_join(mg_min_ndvi_anom, by="pdtn_cycle")
mg_annual_df = cbind(mg_annual_data, annual_pdtn[-1,]$Mato_Grosso)
mg_annual_df = mg_annual_df %>% rename("Year" = 1, "Precipitation" = 2, "Land_Surface_Temp" = 3, "NDVI" = 4, "Max_NDVI_Anom" = 5, "Min_NDVI_Anom" = 6, "Production" = 7)
mg_annual_df$Year = as.numeric(mg_annual_df$Year)
mg_annual_df$NDVI = as.numeric(mg_annual_df$NDVI)
mg_annual_df$Max_NDVI_Anom = as.numeric(mg_annual_df$Max_NDVI_Anom)
mg_annual_df$Min_NDVI_Anom = as.numeric(mg_annual_df$Min_NDVI_Anom)
mg_annual_df$Precipitation = as.numeric(mg_annual_df$Precipitation)
mg_annual_df$Land_Surface_Temp = as.numeric(mg_annual_df$Land_Surface_Temp)
mg_annual_df$Production = as.numeric(mg_annual_df$Production)

#Cordoba
cdb_data$Year = as.integer(cdb_data$Year)
cdb_data = cdb_data %>% mutate(pdtn_cycle = ifelse(Month >= 9, Year+1, Year))
cdb_data = cdb_data[-nrow(cdb_data), ]
cdb_annual_prec_lst = aggregate(cbind(cdb_data$cdb_prec_values, cdb_data$cdb_lst_values)~pdtn_cycle, cdb_data, mean)
cdb_max_ndvi = aggregate(cbind(cdb_data$cdb_ndvi.SAMPLE.VALUE, cdb_data$cdb_ndvi.ANOM.VALUE)~pdtn_cycle, cdb_data, max)
cdb_min_ndvi_anom = aggregate(cdb_data$cdb_ndvi.ANOM.VALUE~pdtn_cycle, cdb_data, min)
cdb_annual_data = cdb_annual_prec_lst %>% right_join(cdb_max_ndvi, by="pdtn_cycle") %>% right_join(cdb_min_ndvi_anom, by="pdtn_cycle")
cdb_annual_df = cbind(cdb_annual_data, annual_pdtn[-1,]$Cordoba)
cdb_annual_df = cdb_annual_df %>% rename("Year" = 1, "Precipitation" = 2, "Land_Surface_Temp" = 3, "NDVI" = 4, "Max_NDVI_Anom" = 5, "Min_NDVI_Anom" = 6, "Production" = 7)
cdb_annual_df$Year = as.numeric(cdb_annual_df$Year)
cdb_annual_df$NDVI = as.numeric(cdb_annual_df$NDVI)
cdb_annual_df$Max_NDVI_Anom = as.numeric(cdb_annual_df$Max_NDVI_Anom)
cdb_annual_df$Min_NDVI_Anom = as.numeric(cdb_annual_df$Min_NDVI_Anom)
cdb_annual_df$Precipitation = as.numeric(cdb_annual_df$Precipitation)
cdb_annual_df$Land_Surface_Temp = as.numeric(cdb_annual_df$Land_Surface_Temp)
cdb_annual_df$Production = as.numeric(cdb_annual_df$Production)

#Poltava Oblast
pltv_annual_prec_lst = aggregate(cbind(pltv_data$pltv_prec_values, pltv_data$pltv_lst_values)~Year, pltv_data, mean)
pltv_max_ndvi = aggregate(cbind(pltv_data$pltv_ndvi.SAMPLE.VALUE, pltv_data$pltv_ndvi.ANOM.VALUE)~Year, pltv_data, max)
pltv_min_ndvi_anom = aggregate(pltv_data$pltv_ndvi.ANOM.VALUE~Year, pltv_data, min)
pltv_annual_data = pltv_annual_prec_lst %>% right_join(pltv_max_ndvi, by="Year") %>% right_join(pltv_min_ndvi_anom, by="Year")
pltv_annual_df = cbind(pltv_annual_data, annual_pdtn$Poltava_Oblast)
pltv_annual_df = pltv_annual_df %>% rename("Precipitation" = 2, "Land_Surface_Temp" = 3, "NDVI" = 4, "Max_NDVI_Anom" = 5, "Min_NDVI_Anom" = 6, "Production" = 7)
pltv_annual_df$Year = as.numeric(pltv_annual_df$Year)
pltv_annual_df$NDVI = as.numeric(pltv_annual_df$NDVI)
pltv_annual_df$Max_NDVI_Anom = as.numeric(pltv_annual_df$Max_NDVI_Anom)
pltv_annual_df$Min_NDVI_Anom = as.numeric(pltv_annual_df$Min_NDVI_Anom)
pltv_annual_df$Precipitation = as.numeric(pltv_annual_df$Precipitation)
pltv_annual_df$Land_Surface_Temp = as.numeric(pltv_annual_df$Land_Surface_Temp)
pltv_annual_df$Production = as.numeric(pltv_annual_df$Production)


#Iowa Linear Regression
set.seed(1)
train = sample(22, 11)

#Linear regression line
lm.fit = lm(Production~Year+NDVI+Max_NDVI_Anom+Min_NDVI_Anom+Precipitation+Land_Surface_Temp, data = iowa_annual_df, subset = train)
summary(lm.fit)

set.seed(4)
train = sample(22, 11)

#Linear regression line
lm.fit = lm(Production~Year+NDVI+Max_NDVI_Anom+Min_NDVI_Anom+Precipitation+Land_Surface_Temp, data = iowa_annual_df, subset = train)
summary(lm.fit)


#Iowa Ridge Regression
y = iowa_annual_df$Production
x = data.matrix(iowa_annual_df[, c("Year", "NDVI", "Min_NDVI_Anom", "Max_NDVI_Anom", "Precipitation", "Land_Surface_Temp")])
ridge_model = cv.glmnet(x, y, alpha = 0, nfolds = 3)
best_lambda = ridge_model$lambda.min
best_lambda

best_model = glmnet(x, y, alpha = 0, lambda = best_lambda)
best_model
coef(best_model)

y_predicted = predict(best_model, s = best_lambda, newx = x)
sst = sum((y - mean(y))^2)
sse = sum((y_predicted - y)^2)
rsq = 1 - sse/sst
rsq

n = nrow(x)
p = ncol(x) - 1  #Subtract 1 for the intercept
adj_rsq = 1 - (1-rsq) * (n-1) / (n-p-1)
adj_rsq


#Iowa Lasso Regression
y = iowa_annual_df$Production
x = data.matrix(iowa_annual_df[, c("Year", "NDVI", "Min_NDVI_Anom", "Max_NDVI_Anom", "Precipitation", "Land_Surface_Temp")])
lasso_model = cv.glmnet(x, y, alpha = 1, nfolds = 5)
best_lambda = lasso_model$lambda.min
best_lambda

best_model = glmnet(x, y, alpha = 1, lambda = best_lambda)
best_model
coef(best_model)

y_predicted = predict(best_model, s = best_lambda, newx = x)
sst = sum((y - mean(y))^2)
sse = sum((y_predicted - y)^2)
rsq = 1 - sse/sst
rsq

n = nrow(x)
p = ncol(x) - 1 # Subtract 1 for the intercept
adj_rsq = 1 - (1-rsq) * (n-1) / (n-p-1)
adj_rsq


#Heilongjiang Linear Regression
set.seed(1)
train = sample(22, 11)

#Linear regression line
lm.fit = lm(Production~Year+NDVI+Max_NDVI_Anom+Min_NDVI_Anom+Precipitation+Land_Surface_Temp, data = hlj_annual_df, subset = train)
summary(lm.fit)

set.seed(4)
train = sample(22, 11)

#Linear regression line
lm.fit = lm(Production~Year+NDVI+Max_NDVI_Anom+Min_NDVI_Anom+Precipitation+Land_Surface_Temp, data = hlj_annual_df, subset = train)
summary(lm.fit)


#Heilongjiang Ridge Regression
y = hlj_annual_df$Production
x = data.matrix(hlj_annual_df[, c("Year", "NDVI", "Min_NDVI_Anom", "Max_NDVI_Anom", "Precipitation", "Land_Surface_Temp")])
ridge_model = cv.glmnet(x, y, alpha = 0, nfolds = 5)
best_lambda = ridge_model$lambda.min
best_lambda

best_model = glmnet(x, y, alpha = 0, lambda = best_lambda)
best_model
coef(best_model)

y_predicted = predict(best_model, s = best_lambda, newx = x)
sst = sum((y - mean(y))^2)
sse = sum((y_predicted - y)^2)
rsq = 1 - sse/sst
rsq

n = nrow(x)
p = ncol(x) - 1  #Subtract 1 for the intercept
adj_rsq = 1 - (1-rsq) * (n-1) / (n-p-1)
adj_rsq


#Heilongjiang Lasso Regression
y = hlj_annual_df$Production
x = data.matrix(hlj_annual_df[, c("Year", "NDVI", "Min_NDVI_Anom", "Max_NDVI_Anom", "Precipitation", "Land_Surface_Temp")])
lasso_model = cv.glmnet(x, y, alpha = 1, nfolds = 5)
best_lambda = lasso_model$lambda.min
best_lambda

best_model = glmnet(x, y, alpha = 1, lambda = best_lambda)
best_model
coef(best_model)

y_predicted = predict(best_model, s = best_lambda, newx = x)
sst = sum((y - mean(y))^2)
sse = sum((y_predicted - y)^2)
rsq = 1 - sse/sst
rsq

n = nrow(x)
p = ncol(x) - 1 # Subtract 1 for the intercept
adj_rsq = 1 - (1-rsq) * (n-1) / (n-p-1)
adj_rsq


#Mato Grosso Linear Regression
set.seed(1)
train = sample(21, 11)

#Linear regression line
lm.fit = lm(Production~Year+NDVI+Max_NDVI_Anom+Min_NDVI_Anom+Precipitation+Land_Surface_Temp, data = mg_annual_df, subset = train)
summary(lm.fit)

set.seed(4)
train = sample(21, 11)

#Linear regression line
lm.fit = lm(Production~Year+NDVI+Max_NDVI_Anom+Min_NDVI_Anom+Precipitation+Land_Surface_Temp, data = mg_annual_df, subset = train)
summary(lm.fit)


#Mato Grosso Ridge Regression
y = mg_annual_df$Production
x = data.matrix(mg_annual_df[, c("Year", "NDVI", "Min_NDVI_Anom", "Max_NDVI_Anom", "Precipitation", "Land_Surface_Temp")])
ridge_model = cv.glmnet(x, y, alpha = 0, nfolds = 5)
best_lambda = ridge_model$lambda.min
best_lambda

best_model = glmnet(x, y, alpha = 0, lambda = best_lambda)
best_model
coef(best_model)

y_predicted = predict(best_model, s = best_lambda, newx = x)
sst = sum((y - mean(y))^2)
sse = sum((y_predicted - y)^2)
rsq = 1 - sse/sst
rsq

n = nrow(x)
p = ncol(x) - 1  #Subtract 1 for the intercept
adj_rsq = 1 - (1-rsq) * (n-1) / (n-p-1)
adj_rsq


#Mato Grosso Lasso Regression
y = mg_annual_df$Production
x = data.matrix(mg_annual_df[, c("Year", "NDVI", "Min_NDVI_Anom", "Max_NDVI_Anom", "Precipitation", "Land_Surface_Temp")])
lasso_model = cv.glmnet(x, y, alpha = 1, nfolds = 5)
best_lambda = lasso_model$lambda.min
best_lambda

best_model = glmnet(x, y, alpha = 1, lambda = best_lambda)
best_model
coef(best_model)

y_predicted = predict(best_model, s = best_lambda, newx = x)
sst = sum((y - mean(y))^2)
sse = sum((y_predicted - y)^2)
rsq = 1 - sse/sst
rsq

n = nrow(x)
p = ncol(x) - 1 # Subtract 1 for the intercept
adj_rsq = 1 - (1-rsq) * (n-1) / (n-p-1)
adj_rsq


#Cordoba Linear Regression
set.seed(1)
train = sample(21, 11)

#Linear regression line
lm.fit = lm(Production~Year+NDVI+Max_NDVI_Anom+Min_NDVI_Anom+Precipitation+Land_Surface_Temp, data = cdb_annual_df, subset = train)
summary(lm.fit)

set.seed(4)
train = sample(21, 11)

#Linear regression line
lm.fit = lm(Production~Year+NDVI+Max_NDVI_Anom+Min_NDVI_Anom+Precipitation+Land_Surface_Temp, data = cdb_annual_df, subset = train)
summary(lm.fit)


#Cordoba Ridge Regression
y = cdb_annual_df$Production
x = data.matrix(cdb_annual_df[, c("Year", "NDVI", "Min_NDVI_Anom", "Max_NDVI_Anom", "Precipitation", "Land_Surface_Temp")])
ridge_model = cv.glmnet(x, y, alpha = 0, nfolds = 5)
best_lambda = ridge_model$lambda.min
best_lambda

best_model = glmnet(x, y, alpha = 0, lambda = best_lambda)
best_model
coef(best_model)

y_predicted = predict(best_model, s = best_lambda, newx = x)
sst = sum((y - mean(y))^2)
sse = sum((y_predicted - y)^2)
rsq = 1 - sse/sst
rsq

n = nrow(x)
p = ncol(x) - 1  #Subtract 1 for the intercept
adj_rsq = 1 - (1-rsq) * (n-1) / (n-p-1)
adj_rsq


#Cordoba Lasso Regression
y = cdb_annual_df$Production
x = data.matrix(cdb_annual_df[, c("Year", "NDVI", "Min_NDVI_Anom", "Max_NDVI_Anom", "Precipitation", "Land_Surface_Temp")])
lasso_model = cv.glmnet(x, y, alpha = 1, nfolds = 5)
best_lambda = lasso_model$lambda.min
best_lambda

best_model = glmnet(x, y, alpha = 1, lambda = best_lambda)
best_model
coef(best_model)

y_predicted = predict(best_model, s = best_lambda, newx = x)
sst = sum((y - mean(y))^2)
sse = sum((y_predicted - y)^2)
rsq = 1 - sse/sst
rsq

n = nrow(x)
p = ncol(x) - 1 # Subtract 1 for the intercept
adj_rsq = 1 - (1-rsq) * (n-1) / (n-p-1)
adj_rsq


#Poltava Oblast Linear Regression
set.seed(1)
train = sample(22, 11)

#Linear regression line
lm.fit = lm(Production~Year+NDVI+Max_NDVI_Anom+Min_NDVI_Anom+Precipitation+Land_Surface_Temp, data = pltv_annual_df, subset = train)
summary(lm.fit)

set.seed(4)
train = sample(22, 11)

#Linear regression line
lm.fit = lm(Production~Year+NDVI+Max_NDVI_Anom+Min_NDVI_Anom+Precipitation+Land_Surface_Temp, data = pltv_annual_df, subset = train)
summary(lm.fit)

#Poltava Oblast Ridge Regression
y = pltv_annual_df$Production
x = data.matrix(pltv_annual_df[, c("Year", "NDVI", "Min_NDVI_Anom", "Max_NDVI_Anom", "Precipitation", "Land_Surface_Temp")])
ridge_model = cv.glmnet(x, y, alpha = 0, nfolds = 5)
best_lambda = ridge_model$lambda.min
best_lambda

best_model = glmnet(x, y, alpha = 0, lambda = best_lambda)
best_model
coef(best_model)

y_predicted = predict(best_model, s = best_lambda, newx = x)
sst = sum((y - mean(y))^2)
sse = sum((y_predicted - y)^2)
rsq = 1 - sse/sst
rsq

n = nrow(x)
p = ncol(x) - 1  #Subtract 1 for the intercept
adj_rsq = 1 - (1-rsq) * (n-1) / (n-p-1)
adj_rsq


#Poltava Oblast Lasso Regression
y = pltv_annual_df$Production
x = data.matrix(pltv_annual_df[, c("Year", "NDVI", "Min_NDVI_Anom", "Max_NDVI_Anom", "Precipitation", "Land_Surface_Temp")])
lasso_model = cv.glmnet(x, y, alpha = 1, nfolds = 5)
best_lambda = lasso_model$lambda.min
best_lambda

best_model = glmnet(x, y, alpha = 1, lambda = best_lambda)
best_model
coef(best_model)

y_predicted = predict(best_model, s = best_lambda, newx = x)
sst = sum((y - mean(y))^2)
sse = sum((y_predicted - y)^2)
rsq = 1 - sse/sst
rsq

n = nrow(x)
p = ncol(x) - 1 # Subtract 1 for the intercept
adj_rsq = 1 - (1-rsq) * (n-1) / (n-p-1)
adj_rsq

