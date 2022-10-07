# Homework 2 - Xuan Huang

library(dplyr)
library(data.table)
library(tidyverse)
library(data.table)
library(gtools)
library(ggpmisc)
library(graphics)
library(leaflet)

# download data
if (!file.exists("chs_individual.csv")) {
  download.file(
    url = "https://raw.githubusercontent.com/USCbiostats/data-science-data/master/01_chs/chs_individual.csv", 
    destfile = "chs_individual.csv", 
    method="libcurl", 
    timeout = 60
  )
}
if (!file.exists("chs_regional.csv")) {
  download.file(
    url = "https://raw.githubusercontent.com/USCbiostats/data-science-data/master/01_chs/chs_regional.csv", 
    destfile = "chs_regional.csv", 
    method="libcurl", 
    timeout = 60
  )
}


# read data
chsInd <- fread("chs_individual.csv")
chsReg <- fread("chs_regional.csv")
head(chsInd)
head(chsReg)


dim(chsInd)
dim(chsReg)

chsMerge <- merge(
  x = chsInd,
  y = chsReg,
  by.x = "townname",
  by.y = "townname",
  all.x = T,
  all.y = F
)
dim(chsMerge)

# After merging the data, the rows of merged dataset is equal to the rows of 
# individual CHS dataset

# To make sure there is no duplicates
chsMerge <- unique(chsMerge)

# Impute data using the average within the variables “male” and “hispanic” for
# missing value
#summary(chsMerge)
chsMale_His <- chsMerge %>% 
  filter(male == 1 & hispanic == 1)

chsMerge[is.na(agepft), agepft := round(mean(chsMale_His$agepft, na.rm = T), digits = 3)]
chsMerge[is.na(height), height := round(mean(chsMale_His$height, na.rm = T), digits = 0)]
chsMerge[is.na(weight), weight := round(mean(chsMale_His$weight, na.rm = T), digits = 0)]
chsMerge[is.na(bmi), bmi := round(mean(chsMale_His$bmi, na.rm = T), digits = 3)]
chsMerge[is.na(asthma), asthma := round(mean(chsMale_His$asthma, na.rm = T), 1)]
chsMerge[is.na(father_asthma), father_asthma := round(mean(chsMale_His$father_asthma, na.rm = T), 1)]
chsMerge[is.na(mother_asthma), mother_asthma := round(mean(chsMale_His$mother_asthma, na.rm = T), 1)]
chsMerge[is.na(wheeze), wheeze := round(mean(chsMale_His$wheeze, na.rm = T), 1)]
chsMerge[is.na(hayfever), hayfever := round(mean(chsMale_His$hayfever, na.rm = T), 1)]
chsMerge[is.na(allergy), allergy := round(mean(chsMale_His$allergy, na.rm = T), 1)]
chsMerge[is.na(educ_parent), educ_parent := round(mean(chsMale_His$educ_parent, na.rm = T), 1)]
chsMerge[is.na(smoke), smoke := round(mean(chsMale_His$smoke, na.rm = T), 1)]
chsMerge[is.na(gasstove), gasstove := round(mean(chsMale_His$gasstove, na.rm = T), 1)]
chsMerge[is.na(fev), fev := round(mean(chsMale_His$fev, na.rm = T), digits = 1)]
chsMerge[is.na(fvc), fvc := round(mean(chsMale_His$fvc, na.rm = T), 1)]
chsMerge[is.na(mmef), mmef := round(mean(chsMale_His$mmef, na.rm = T), digits = 1)]

#summary(chsMerge)

# create categorical variable
chsMerge$obesity_level <- cut(chsMerge$bmi,
                              breaks = c(0, 14, 22, 24, Inf),
                              labels = c("underweight", "normal", "overweight", "obese")
)

# create a summary table for bmi level
chsMerge %>%
  group_by(obesity_level) %>%
  summarise(
    min = min(bmi),
    max = max(bmi),
    count = n()
  )

### 3. Create another categorical variable named “smoke_gas_exposure”
#“smoke_gas_exposure”  summarizes “Second Hand Smoke” and “Gas Stove.”
summary(chsMerge$smoke)
summary(chsMerge$gasstove)

# there are 4 possible combination of second hand smoke status and gas stove status
chsMerge <- 
  chsMerge %>%
  mutate(
    smoke_gas_exposure = case_when(
      smoke == 0 & gasstove == 0 ~ "neiher",
      smoke == 1 & gasstove == 0 ~ "only smoke",
      smoke == 0 & gasstove == 1 ~ "only gas stove",
      smoke == 1 & gasstove == 1 ~ "both"
    )
  )

table(chsMerge$smoke_gas_exposure)

# summary table by town
chsMerge %>% 
  group_by(townname) %>%
  summarise(
    count = n(),
    average_fev = mean(fev),
    sd_fev = sd(fev),
    prop_asthma = mean(asthma),
    sd_asthma = sd(asthma)
  )


# summary table by sex
chsMerge %>% 
  group_by(male) %>%
  summarise(
    count = n(),
    average_fev = mean(fev),
    sd_fev = sd(fev),
    prop_asthma = mean(asthma),
    sd_asthma = sd(asthma)
  )


# by obesity level
chsMerge %>% 
  group_by(obesity_level) %>%
  summarise(
    count = n(),
    average_fev = mean(fev),
    sd_fev = sd(fev),
    prop_asthma = mean(asthma),
    sd_asthma = sd(asthma)
  )


# by smoke_gas_exposure
chsMerge %>% 
  group_by(smoke_gas_exposure) %>%
  summarise(
    count = n(),
    average_fev = mean(fev),
    sd_fev = sd(fev),
    prop_asthma = mean(asthma),
    sd_asthma = sd(asthma)
  )


### EDA checklist
# check the dimensions and headers and footers
dim(chsMerge)
head(chsMerge)
tail(chsMerge)

#check the variable types
str(chsMerge)

# Take a closer look at some/all of the variables
# here, our key variables are bmi, fev, smoke_gas_exposure, PM2.5
summary(chsMerge$bmi)
summary(chsMerge$fev)
table(chsMerge$smoke_gas_exposure)
summary(chsMerge$pm25_mass)

### 1. Facet plot showing scatterplots with regression lines of BMI vs FEV by “townname”.
chsMerge %>%
  ggplot(mapping = aes(x = bmi, y = fev, color = townname)) +
  geom_point() +
  geom_smooth(method = "lm", color = "Black") +
  labs(title = "BMI vs FEV by Twon",x = "BMI", y = "FEV") +
  facet_wrap(~ townname)

### 2. Stacked histograms of FEV by BMI category and FEV by smoke/gas exposure. Use different color schemes than the ggplot default.
# Stacked histograms of FEV by BMI category
chsMerge %>%
  ggplot(aes(x = fev, fill = obesity_level)) +
  geom_histogram() +
  scale_fill_manual(values = c("red", "blue", "darkgreen", "orange")) +
  labs(title = "FEV by Obesity level",x = "FEV", y = "Count", fill = "Obesity level")

# FEV by smoke/gas exposure
chsMerge %>%
  ggplot(aes(x = fev, fill = smoke_gas_exposure)) +
  geom_histogram() +
  scale_fill_manual(values = c("red", "blue", "darkgreen", "orange")) +
  labs(title = "FEV by smoke/gas exposure", x = "FEV", y = "Count", fill = "smoke/gas exposure")

chsMerge %>%
  ggplot(aes(x = obesity_level, fill = smoke_gas_exposure)) +
  geom_bar() + 
  scale_fill_manual(values = c("red", "blue", "darkgreen", "orange")) +
  labs(title = "Barchart of BMI by smoke/gas exposure", x = "Obesity level", y = "Count", fill = "Smoke/Gas exposure")


### 4. Statistical summary graphs of FEV by BMI and FEV by smoke/gas exposure category.
chsMerge %>%
  ggplot(mapping = aes(y = fev, x = obesity_level),
         fun = median,
         fun.min = min,
         fun.max = max
  ) +
  stat_summary() +
  labs(title = "Statistical Summary of FEV by BMI", y = "FEV", x = "Obesity_level")


chsMerge %>%
  ggplot(mapping = aes(y = fev, x = smoke_gas_exposure),
         fun = median,
         fun.min = min,
         fun.max = max
  ) +
  stat_summary() +
  labs(title = "Statistical Summary of FEV by Smoke/Gas exposure", y = "FEV", x = "Smoke/Gas exposure")

### 5. A leaflet map showing the concentrations of PM2.5 mass in each of the CHS communities.

temp.pal <- colorFactor(palette ='viridis', domain = chsMerge$pm25_mass)

leaflet(chsMerge[!is.na(pm25_mass)]) %>%
  addProviderTiles('CartoDB.Positron') %>%
  addCircles(
    lat = ~lat, lng=~lon,
    label = chsMerge$townname, color = ~ temp.pal(pm25_mass),
    opacity = 1, fillOpacity = 1, radius = 500
  ) %>%
  # And a pretty legend
  addLegend('bottomleft', pal=temp.pal, values = chsMerge$pm25_mass,
            title='Concentrations of PM2.5 mass in each of the CHS communities', opacity=1)


### 6. Choose a visualization to examine whether PM2.5 mass is associated with FEV.


# Both PM2.5 and FEV are numeric variable, so we can create a scatter plot with a linear regression model
chsMerge %>%
  ggplot(mapping = aes(x = pm25_mass, y = fev)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Relationship between PM2.5 and FEV")



# Based on the previous scatter plot, we cannot find a obviously association.
# Because PM2.5 mass is a town_level variable, we should calculate the typical 
# FEV for each town and research the relationship between the town level FEV and
# twon level PM2.5 mass

chsMerge_town <- chsMerge %>%
  group_by(townname) %>%
  mutate(fev_town = mean(fev))

chsMerge_town %>%
  ggplot(mapping = aes(x = pm25_mass, y = fev_town, color = factor(townname))) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Relationship between PM2.5 and FEV", x = "PM2.5 Concentration for each Town", y = "Average FEV for each Town")



