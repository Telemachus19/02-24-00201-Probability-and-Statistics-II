# Loading packages
library(tidyverse)
library(ggplot2)
library(ggcorrplot)
library(RColorBrewer)
dis
# Loading Data
train <- read_csv("data/train.csv")


# Summary of Data
summary(train)

# Viewing the first six rows of the data
head(train)

## Correlation Matrix
train %>%
  filter(!is.na(Age)) %>%
  select(Survived, Pclass,Age,SibSp,Parch,Fare) %>%
  cor() %>%
  ggcorrplot(lab = T,
             ggtheme = ggplot2::theme_void(),
             title="Correlation Graph",hc.order=T,
             colors =rev(viridis(3,alpha=0.7)))
train %>% drop