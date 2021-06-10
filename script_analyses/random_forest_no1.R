library(don)
library(ranger)
library(randomForest)
library(tidyverse)
library(tidymodels)
library(caret)
#### 

don <- read_csv("data/magic_train_feature_eng.csv")

ca <- don %>%
  filter(state=="06")

df <- ca %>%
  select(-c(state,date_semaine,nb,area_burned,semaine,annee,
            nb_feux_main_cause,nb_feux_voisins,X0_0))%>%
  mutate(XX_Developed = X21_Developed_open_space+X22_Developed_low_intensity+
           X23_Developed_medium_intensity+X24_Developed_high_intensity)%>%
  select(-c(X21_Developed_open_space:X24_Developed_high_intensity,county))


df_split <- initial_split(df,prob = .8)

train <- training(df_split)
test <- testing(df_split)

### VALEURS MANQUANTES: SURTOUT HUMIDITY
x <- train %>% summarise_all(~sum(is.na(.)))

## POUR ALLER VITE: NA_OMIT 
train <- train %>% na.omit()

rf1 <- randomForest(as_factor(FIRE)~.,ntree =50,
                    data = train)

pred_test <- predict(rf1,test)

results <- tibble(Y = test$FIRE, pred = pred_test) %>%
  mutate(bonne_prediction = Y==pred) %>%
  filter(!is.na(Y),!is.na(pred))

sum(results$bonne_prediction)/nrow(results)

#### 
caret::confusionMatrix(pred_test,as_factor(test$FIRE))

#### Meilleures variables
varImpPlot(rf1)

randomForest::importance(rf1)

