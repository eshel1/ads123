library(sf)
library(dplyr)
library(tidyverse)
library(corrgram)
getwd()
#spatil---------
# Set your working directory

#סידור המידע--------
setwd("C:/אשל לימודים/אשל לימודים תשפד' סמסטר ב/למדעי הנתונים למתקדמים/פרויקט/אוטותל/ADS_finalproject")
tel_auto_tel=read.csv("at_origin.csv")
tel <- read.csv("tel_table.csv")#טבלת אוטו תל לפי איזור סטטסטי
lamas <- read.csv("lams_2022.csv")#טבלת למס של פרטים על השכונה
eshkol<-read.csv("eshkol.csv")
dim(tel_auto_tel)
dim(tel)
dim(lamas)
dim(eshkol)


#כל הjoin מבוסס על קוד איזו סטטסיטי
tel=tel|>mutate(age_y_pec=(g20to24+g25to29)/ sumpop  )|>
  mutate(age_a_pec=(g30to34+g35to39+ g40to44+ g45to49+ g50to54+ g55to59+ g60to64)/sumpop )|>
  mutate(carto_pop=start/sumpop)|>
  select(msezor,age_y_pec,age_a_pec,carto_pop,)




#אם אתה ק:ורצים הסבר על עומודת למס תבדקו בchat
lamas_filtered <- lamas |> filter(LocNameHeb == "תל אביב -יפו")|>
  select(StatArea,Vehicle2up_pcnt,employeesAnnual_medWage,Parking_pcnt,AcadmCert_pcnt,SelfEmployedAnnual_medWage )

merged_data <- left_join(tel, lamas_filtered, by = c("msezor" = "StatArea"))#איחו למס  ותל איבב





eshkol_filtered=eshkol|>filter(yeshoove_h=="תל אביב -יפו")|>select(CODE.OF.STATISTICAL.AREA,INDEX.VALUE.2019.2.)

final_data <- left_join(merged_data, eshkol_filtered, by = c("msezor" = "CODE.OF.STATISTICAL.AREA"))#חיבור דירוג אשקולים
#צריך להורי הרבה עמודות כמובן
final_data

final_data <- final_data |>
  mutate(across(everything(), ~ replace_na(., 0)))

final_data <- final_data |>
  mutate_all(~replace(., is.infinite(.), 0))

final_data <- final_data |> rename(Economics_Index = INDEX.VALUE.2019.2.)

summary(final_data)
View(final_data)

#cheking linarity---------
ggplot(final_data, aes(y = sqrt(carto_pop),x=sqrt(age_y_pec))) +
  geom_point(color = "blue")+geom_smooth(method = "lm") +labs(
    title = " carto_pop vs age_y_pec",
    x = "age g20-to29 in poulation", # X-axis label
    y = "sharing cra using to poplation" # Y-axis label
  )
ggplot(final_data, aes(y = carto_pop,x=age_a_pec)) +
  geom_point(color = "blue")+geom_smooth(method = "lm") +labs(
    title = " carto_pop vs Vehicle2up_pcnt",
    x = "age_a_pec", # X-axis label
    y = "carto_pop" # Y-axis label
  )

ggplot(final_data, aes(y = carto_pop,x=Vehicle2up_pcnt)) +
  geom_point(color = "blue")+geom_smooth(method = "lm") +labs(
    title = " carto_pop vs Vehicle2up_pcnt",
    x = "Vehicle2up_pcnt", # X-axis label
    y = "carto_pop" # Y-axis label
  )



ggplot(final_data, aes(y = carto_pop,x=employeesAnnual_medWage)) +
  geom_point(color = "blue")+geom_smooth(method = "lm") +labs(
    title = " carto_pop vs employeesAnnual_medWage",
    x = "employeesAnnual_medWage", # X-axis label
    y = "carto_pop" # Y-axis label
  )



ggplot(final_data, aes(y = carto_pop,x=Parking_pcnt)) +
  geom_point(color = "blue")+geom_smooth(method = "lm") +labs(
    title = " carto_pop vs Parking_pcnt",
    x = "Parking_pcnt", # X-axis label
    y = "carto_pop" # Y-axis label
  )




ggplot(final_data, aes(y = carto_pop,x=AcadmCert_pcnt)) +
  geom_point(color = "blue")+geom_smooth(method = "lm") +labs(
    title = " carto_pop vs AcadmCert_pcnt",
    x = "AcadmCert_pcnt", # X-axis label
    y = "carto_pop" # Y-axis label
  )




ggplot(final_data, aes(y = carto_pop,x=AcadmCert_pcnt)) +
  geom_point(color = "blue")+geom_smooth(method = "lm") +labs(
    title = " carto_pop vs AcadmCert_pcnt",
    x = "AcadmCert_pcnt", # X-axis label
    y = "carto_pop" # Y-axis label
  )



ggplot(final_data, aes(y = carto_pop,x=SelfEmployedAnnual_medWage)) +
  geom_point(color = "blue")+geom_smooth(method = "lm") +labs(
    title = " carto_pop vs SelfEmployedAnnual_medWage",
    x = "SelfEmployedAnnual_medWage", # X-axis label
    y = "carto_pop" # Y-axis label
  )



ggplot(final_data, aes(y = carto_pop,x=Economics_Index)) +
  geom_point(color = "blue")+geom_smooth(method = "lm") +labs(
    title = " carto_pop vs Economics_Index",
    x = "SelfEmployedAnnual_medWage", # X-axis label
    y = "carto_pop" # Y-axis label
  )


#normality---------

shapiro_results <- sapply(final_data, function(x) if(is.numeric(x)) shapiro.test(x)$p.value else NA)
shapiro_results
## msezor: 1.883383e-07 - Not normally distributed (p-value < 0.05)
# age_y_pec: 2.289723e-16 - Not normally distributed (p-value < 0.05)
# carto_pop: 6.673668e-27 - Not normally distributed (p-value < 0.05) did not mange to nkae the data normal
# start: 3.537074e-09 - Not normally distributed (p-value < 0.05)
# sumpop: 2.467217e-07 - Not normally distributed (p-value < 0.05)
# Vehicle2up_pcnt: 1.128837e-15 - Not normally distributed (p-value < 0.05)
# employeesAnnual_medWage: 1.065011e-13 - Not normally distributed (p-value < 0.05)
# Parking_pcnt: 2.010579e-15 - Not normally distributed (p-value < 0.05)
# AcadmCert_pcnt: 6.869806e-15 - Not normally distributed (p-value < 0.05)
# SelfEmployedAnnual_medWage: 2.029233e-13 - Not normally distributed (p-value < 0.05)
# Economics_Index: 2.159792e-07 - Not normally distributed (p-value < 0.05)

#------

#טרסנפורמציה---------------
ggplot(data = final_data)+geom_histogram(aes(x=carto_pop),fill = "skyblue", color = "black")


ggplot(data = final_data)+geom_histogram(aes(x=1/carto_pop),fill = "skyblue", color = "black")


par(mfrow=c(2,2))
a=1/final_data$carto_pop
a[is.infinite(a)] <- NA
shapiro.test(a)






cor1 = cor.test( final_data$age_y_pec,final_data$carto_pop, method = "kendall")
print(cor1)

cor2 = cor.test(final_data$carto_pop, final_data$age_a_pec, method = "kendall")
print(cor2)


cor3 = cor.test(final_data$carto_pop, final_data$Vehicle2up_pcnt, method = "kendall")
print(cor3)

cor4 = cor.test(final_data$carto_pop, final_data$employeesAnnual_medWage, method = "kendall")
print(cor4)

cor5 = cor.test(final_data$carto_pop, final_data$Parking_pcnt, method = "kendall")
print(cor5)
cor6 = cor.test(final_data$carto_pop, final_data$AcadmCert_pcnt,method = "kendall")
print(cor6)
cor7 = cor.test(final_data$carto_pop, final_data$SelfEmployedAnnual_medWage,method = "kendall")
print(cor7)
cor8 = cor.test(final_data$carto_pop, final_data$Economics_Index,method = "kendall")
print(cor8)



corrgram(final_data,upper.panel=panel.conf)



lm1=lm(carto_pop  ~  +age_y_pec +age_a_pec+Vehicle2up_pcnt+employeesAnnual_medWage+Parking_pcnt+AcadmCert_pcnt+Economics_Index , data = final_data)
summary(lm1)


variables <- list(final_data$age_y_pec, final_data$age_a_pec, final_data$Vehicle2up_pcnt, final_data$employeesAnnual_medWage, final_data$Parking_pcnt, final_data$AcadmCert_pcnt, final_data$Economics_Index)
cor_tests <- lapply(variables, function(var) cor.test(final_data$carto_pop, var, method = "kendall"))
print(cor_tests)


  hist(exp(final_data$carto_pop^3))

  
  
  #לעשות ויזואליציה לכל הגרפים בפאנל אחד
  # Load required packages
  library(ggplot2)
  library(gridExtra)
  
  # Define all the ggplot objects
  plot1 <- ggplot(final_data, aes(y = sqrt(carto_pop), x = sqrt(age_y_pec))) +
    geom_point(color = "blue") + 
    geom_smooth(method = "lm") +
    labs(
      title = "carto_pop vs age_y_pec",
      x = "age g20-to29 in population",
      y = "sharing cra using to population"
    )
  
  plot2 <- ggplot(final_data, aes(y = carto_pop, x = age_a_pec)) +
    geom_point(color = "blue") + 
    geom_smooth(method = "lm") +
    labs(
      title = "carto_pop vs age_a_pec",
      x = "age_a_pec",
      y = "carto_pop"
    )
  
  plot3 <- ggplot(final_data, aes(y = carto_pop, x = Vehicle2up_pcnt)) +
    geom_point(color = "blue") + 
    geom_smooth(method = "lm") +
    labs(
      title = "carto_pop vs Vehicle2up_pcnt",
      x = "Vehicle2up_pcnt",
      y = "carto_pop"
    )
  
  plot4 <- ggplot(final_data, aes(y = carto_pop, x = employeesAnnual_medWage)) +
    geom_point(color = "blue") + 
    geom_smooth(method = "lm") +
    labs(
      title = "carto_pop vs employeesAnnual_medWage",
      x = "employeesAnnual_medWage",
      y = "carto_pop"
    )
  
  plot5 <- ggplot(final_data, aes(y = carto_pop, x = Parking_pcnt)) +
    geom_point(color = "blue") + 
    geom_smooth(method = "lm") +
    labs(
      title = "carto_pop vs Parking_pcnt",
      x = "Parking_pcnt",
      y = "carto_pop"
    )
  
  plot6 <- ggplot(final_data, aes(y = carto_pop, x = AcadmCert_pcnt)) +
    geom_point(color = "blue") + 
    geom_smooth(method = "lm") +
    labs(
      title = "carto_pop vs AcadmCert_pcnt",
      x = "AcadmCert_pcnt",
      y = "carto_pop"
    )
  
  plot7 <- ggplot(final_data, aes(y = carto_pop, x = SelfEmployedAnnual_medWage)) +
    geom_point(color = "blue") + 
    geom_smooth(method = "lm") +
    labs(
      title = "carto_pop vs SelfEmployedAnnual_medWage",
      x = "SelfEmployedAnnual_medWage",
      y = "carto_pop"
    )
  
  plot8 <- ggplot(final_data, aes(y = carto_pop, x = Economics_Index)) +
    geom_point(color = "blue") + 
    geom_smooth(method = "lm") +
    labs(
      title = "carto_pop vs Economics_Index",
      x = "Economics_Index",
      y = "carto_pop"
    )
  
  # Arrange all the plots in a grid
  grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8, ncol = 2)
  
  
  polygon <- st_read("layer22/pol1.shp")
  polygon=polygon|>select(msezor)
  spatil_data=left_join(polygon,final_data)
  colors <- rainbow(10)
    plot(spatil_data[, -1],axes = TRUE)
  
  
  map_list <- lapply(names(final_data)[-1], function(col) {
    mapview(spatil_data, zcol = col, layer.name = col)
  })
  
  # Combine all mapview objects into one interactive map
  combined_map <- reduce(map_list, `+`)
  
  # Plot the combined mapview
  combined_map
  
