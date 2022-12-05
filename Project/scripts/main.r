# ----- Loading packages ---------
library(tidyverse)
library(viridis)
library(ggplot2)
library(ggcorrplot)
library(ggthemes)
library(hrbrthemes)
library(e1071)
library(mice)
library(statsr)
library(gganimate)


# Loading Data
train <- read_csv("data/train.csv")
test <- read_csv("data/test.csv")

df <- bind_rows(train,test)

train %>%
  dplyr::filter(Survived == 1)

colSums(is.na(train))

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
             title="Correlation Matrix",hc.order=T,
             colors =rev(viridis(3,alpha=0.7)),
             digits = 3)
colnames(train)
## Pclass Against Survived
g1 <- train %>%
  select(Pclass,Survived) %>%
  ggplot(aes(as_factor(Pclass),fill=as_factor(Survived))) + 
  geom_bar(position = "fill") +
  scale_y_continuous(labels=scales::percent) +
  theme_ipsum_rc() + 
  labs(x = "Classes",y = "Survival Rate")+
  scale_fill_discrete(name = "Survived", labels = c("Didn't Survive","Survived"))

## SibSp Against Survived
g2 <- train %>%
  select(SibSp,Survived) %>%
  ggplot(aes(as_factor(SibSp),fill=as_factor(Survived))) +
  geom_bar(position = "fill") + 
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Siblings and Spouses",y = "Survival Rate")+
  scale_fill_discrete(name = "Survived", labels = c("Didn't Survive","Survived")) +
  theme_ipsum()

## Parch against Survived
g3 <- train %>%
  select(Parch,Survived) %>%
  ggplot(aes(as_factor(Parch),fill=as_factor(Survived))) + 
  geom_bar(position = "fill") +
  scale_y_continuous(label = scales::percent)+
  labs(x = "Number of parents/children",y = "Survival Rate")+
  scale_fill_discrete(name = "Survived", labels = c("Didn't Survive","Survived")) +
  theme_ipsum_rc()

## Sex against Survived
g4 <- train %>%
  select(Sex,Survived) %>%
  ggplot(aes(as_factor(Sex),fill = as_factor(Survived))) + 
  geom_bar(position = "fill") +
  scale_y_continuous(label = scales::percent) + 
  labs(x = "Sex",y = "Survival Rate")+
  scale_fill_discrete(name = "Survived", labels = c("Didn't Survive","Survived")) +
  theme_ipsum_rc()

gridExtra::grid.arrange(g1,g2,g3,g4,nrow=2)
train %>%
  select(Age) %>%
  ggplot(aes(Age, y = ..density..)) +
  geom_histogram(bins = 20,binwidth = 1,color=inferno(1,alpha=1)) + 
  geom_density(fill=inferno(1,begin = 0.5,alpha = 0.5),color = inferno(1,begin=0)) + 
  annotate(
    "text",
    x = 70,
    y = 0.04,
    label = paste("Skewness:",skewness(train$Age,na.rm = T)),
    colour = inferno(1,begin = 0.1),
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
    x = 200,
    y = 0.05,
    label = paste("Skewness",skewness(train$Fare)),
    colour = "black",
    size = 4
  )
FareDensity 

train_log <- train %>%
  select(Fare) %>%
  mutate(Fare = log(Fare))
colSums(is.na(train_log))

FareDensity <- train_log %>%
  ggplot(aes(Fare, y = ..density..)) +
  geom_histogram(binwidth = 0.1,color=inferno(1,alpha=1)) + 
  geom_density(fill=inferno(1,begin = 0.5,alpha = 0.5),color = inferno(1,begin=0)) + 
  theme_ipsum_rc() +
  annotate(
    "text",
    x = 0,
    y = 0.05,
    label = paste("Skewness",skewness(train_log$Fare)),
    colour = "black",
    size = 4
  )

FareDensity 
#---------------MICE--------------
set.seed(129)
mice_mod <- mice(df[,!names(df) %in% c('PassengerId','Name','Ticket','Cabin','Survived')],method = 'rf')
mice_output <- complete(mice_mod)
g1 <- df %>%
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

# ---- GGanimate ----



