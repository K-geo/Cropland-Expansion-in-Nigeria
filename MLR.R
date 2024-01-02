# Clear workspace
rm(list = ls())     
# Garbage collection
gc(reset = TRUE)

#installing packages
library(lme4)
library(lmerTest)
library(MuMIn)
library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(ggplot2)
library(expss)
library(car)

setwd("E:\\Dietary\\ArcGIS\\all")
#Read the files in that folder
list.files(path=".", pattern=NULL, all.files=FALSE,
           full.names=FALSE)


df1 = read.csv("all08_18_catenotSD_wind_mod4_fc.csv", header = TRUE, sep = ",")


#running  model
model2 = lm(diet_div_std ~ fl + Popdenmean_std + urban_dis_std + road_dis_std + lvstk_std + malhead_std + Water_im_std + Toilet_imp_std + edu_std + age_std + time_to_wtr_std + weatlh_std + FL15SD:FCSD + (1 | year) , df1)

options(width = 200)

# summary will display the regression coefficients 
summary(model1)

# this wil give you the R square
r.squaredGLMM(model1)
write.csv(summary(model1)$coefficients,"E:\\Dietary\\RStudio\\Summary_all1_rural_lm.csv")

# summary will display the regression coefficients 
summary(model2)

# this wil give you the R square
r.squaredGLMM(model2)
write.csv(summary(model2)$coefficients,"E:\\Dietary\\RStudio\\Summary_all2.csv")


#creatin plot
plot_model(model1, show.values = TRUE, dot.size = 0.5, value.size = 2, value.offset = 0.3, colors = "firebrick", title = "15YR_INT_ALL", xlim = c(-0.5, 0.5))

plot_model(model1, show.values = TRUE, dot.size = 0.5, value.size = 2, value.offset = 0.3, title = "15YR_INT_ALL", xlim = c(-0.5, 0.5))

save_plot(
  "E:\\Dietary\\RStudio\\15yr_inter_ALL.png",
  fig = last_plot(),
  width = 12,
  height = 9,
  dpi = 300,
  label.color = "black",
  label.size = 2.4,
  axis.textsize = 0.75,
  axis.titlesize = 0.5,)


# Compute VIF
vif_values <- vif(model1)
print(vif_values)

co

?plot_model