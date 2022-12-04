# Loading packages
library(tidyverse)
library(viridis)
library(ggplot2)
library(ggcorrplot)
library(ggthemes)
library(hrbrthemes)
library(e1071)
library(mice)

# Loading Data
train <- read_csv("data/train.csv")
test <- read_csv("data/test.csv")

df <- bind_rows(train,test)


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
             ggtheme =theme_ipsum_rc(grid = F),
             title="Correlation Graph",hc.order=T,
             colors =rev(viridis(3,alpha=0.7)),
             digits = 3)
colnames(train)
## SibSp Against Survived
train %>%
  select(Pclass,Survived) %>%
  ggplot(aes(as_factor(Pclass),fill=as_factor(Survived))) + 
  geom_bar(position = "fill") +
  scale_y_continuous(labels=percent) +
  theme_ipsum_rc() + 
  xlab("Classes") + 
  ylab("Survival Rate")

train %>%
  select(SibSp,Survived) %>%
  ggplot(aes(as_factor(SibSp),fill=as_factor(Survived))) +
  geom_bar(position = "fill") + 
  scale_y_continuous(labels = scales::percent) +
  xlab("Siblings and Spouses") + 
  ylab("Survivial Rate") + 
  theme_ipsum()

train %>%
  select(Parch,Survived) %>%
  ggplot(aes(as_factor(Parch),fill=as_factor(Survived))) + 
  geom_bar(position = "fill") +
  scale_y_continuous(label = scales::percent)+
  xlab("Parch") +
  ylab("Survival Rate") + 
  theme_ipsum_rc()

train %>%
  select(Sex,Survived) %>%
  ggplot(aes(as_factor(Sex),fill = as_factor(Survived))) + 
  geom_bar(position = "fill") +
  scale_y_continuous(label = scales::percent) + 
  xlab("Sex") + 
  ylab("Survival Rate") + 
  theme_ipsum_rc()

train %>%
  select(Age) %>%
  ggplot(aes(Age, y = ..density..)) +
  geom_histogram(bins = 20,binwidth = 1,color=inferno(1,alpha=1)) + 
  geom_density(fill=inferno(1,begin = 0.5,alpha = 0.5),color = inferno(1,begin=0)) + 
  annotate(
    "text",
    x = 70,
    y = 0.04,
    label = paste(skewness(train$Age,na.rm = T)),
    colour = viridis(1,begin = 0.5),
    size = 4
  ) + 
  theme_ipsum_rc()

FareDensity <- train %>%
  select(Fare) %>%
  ggplot(aes(Fare, y = ..density..)) +
  geom_histogram(bins = 20,binwidth = 1,color=viridis(1,alpha=1)) + 
  geom_density(fill=inferno(1,begin = 0.5,alpha = 0.5),color = viridis(1,begin=0)) + 
  scale_y_continuous(limits = c(0,0.05))+
  theme_ipsum_rc() + 
  annotate(
    "text",
    x = 500,
    y = 0.05,
    label = paste("Skewness",skewness(train$Fare)),
    colour = "red",
    size = 4
  )
FareDensity 

FareDensity <- train %>%
  select(Fare) %>%
  mutate(Fare = log(Fare)) %>%
  ggplot(aes(Fare, y = ..density..)) +
  geom_histogram(binwidth = 0.1,color=inferno(1,alpha=1)) + 
  geom_density(fill=inferno(1,begin = 0.5,alpha = 0.5),color = inferno(1,begin=0)) + 
  theme_ipsum_rc()

skewness()

#---------------MICE--------------
set.seed(129)
mice_mod <- mice(df[,!names(df) %in% c('PassengerId','Name','Ticket','Cabin','Survived')],method = 'rf')
mice_output <- complete(mice_mod)
g1 <- df2 %>%
  select(Age) %>%
  ggplot(aes(Age, y = ..density..)) +
  geom_histogram(bins = 25,binwidth = 1,color=inferno(1,alpha=1)) + 
  geom_density(fill=inferno(1,begin = 0.5,alpha = 0.5),color = inferno(1,begin=0)) +
  ggtitle("Distribution of original data") +
  theme_ipsum_rc()
g2 <- mice_output %>%
  select(Age) %>%
  ggplot(aes(Age, y = ..density..)) +
  geom_histogram(bins = 25,binwidth = 1,color=inferno(1,alpha=1)) + 
  geom_density(fill=inferno(1,begin = 0.5,alpha = 0.5),color = inferno(1,begin=0)) + 
  ggtitle("Distribution of mice output") +
  theme_ipsum_rc()

gridExtra::grid.arrange(g1,g2,nrow = 1)

colSums(is.na(mice_output))
colSums(is.na(df))
