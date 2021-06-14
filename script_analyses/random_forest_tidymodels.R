library(tidymodels)
library(tidyverse)
library(workflows)
library(tune)
library(ranger)

### utilisation de tidy models
# Test sur la Californie 
don <- read_csv("data/magic_train_feature_eng.csv") %>%
  mutate(FIRE = ifelse(FIRE,"FEU","NON_FEU"),
         FIRE = as_factor(FIRE))

# Start with california 
ca <- don %>%
  filter(state=="06")

# Selection des variables d'intérêt
df <- ca %>%
  select(-c(state,date_semaine,nb,area_burned,semaine,annee,
            nb_feux_main_cause,nb_feux_voisins,X0_0))%>%
  mutate(XX_Developed = X21_Developed_open_space+X22_Developed_low_intensity+
           X23_Developed_medium_intensity+X24_Developed_high_intensity)%>%
  select(-c(X21_Developed_open_space:X24_Developed_high_intensity,county))

# Création jeu de training et jeu de test 
# 400 lignes qui partent sur un na.omit
df <- df %>% na.omit()
df_split <- initial_split(df,prob = .8)

train <- training(df_split) 
#train <- training(df_split) %>% na.omit()
test <- testing(df_split)

# Création d'une version de CV du jeu de train 
#train_cv <- vfold_cv(train, v=10)

# Recette 1: Enlever les missing et Normalisation 
# NB: Recette ne prend que les noms et les rôles du data
# On peut donc donner un head(data) comme argument pour économiser de la mémoire
df_recipe <- recipe(FIRE ~. , data = head(train)) %>%
  step_normalize(all_numeric()) 
  #step_impute_knn(all_numeric())

# Pas obligatoire mais on peut regarder
# On peut regarder la matrice utilisée par le modèle 
df_pre_processed <- df_recipe %>% prep(train) %>% juice()


# Test avec ranger que modèle fonctionne 
# rf <- ranger(FIRE~.,data=na.omit(train),num.trees =50,mtry=5)
# 1-rf$prediction.error

## Tuning à la main de mtry
# quelles valeurs va-t-on essayer? 
# sqrt(length(train)) # 5, donc essayons 4,5,6
# res <- tibble(model = "Rf_50tree",
#               mtry  = 0,
#               accuracy = 0) 
# 
# for (i in c(4,5,6)){
#   print("------------------------------")
#   print(paste0("Test avec mtry = ",i))
#   rf_pre_pro <- ranger(FIRE~.,data=df_pre_processed,
#                        num.trees = 50,
#                        mtry = i)
#   temp <- tibble(model = "Rf_50tree",
#                  mtry  = i,
#                  accuracy = 1-rf_pre_pro$prediction.error) 
#   res <- bind_rows(res,temp) 
#   
# }
# res %>% slice_max(accuracy)

## MODELE 1: 
# RANDOM FOREST : On va tuner mtry
#1. Creation de l'objet model
rf_model <- rand_forest() %>%
  set_args(mtry = 4,
           num.tree =50) %>% 
  # select the engine/package that underlies the model
  set_engine("ranger", importance = "impurity") %>%
  set_mode("classification") 

# 2. Creation d'un workflow 
rf_workflow <- workflow() %>%
  add_recipe(df_recipe)%>%
  add_model(rf_model) 


# 3. Tuning 
# quelles valeurs va-t-on essayer? 
sqrt(length(train)) # 5, donc essayons 4,5,6
train_cv <- vfold_cv(train, v=10)
rf_grid <- expand.grid(mtry = c(4, 5, 6))
# extract results
rf_tune_results <- rf_workflow %>%
  tune_grid(resamples = train_cv, #CV object
            grid = rf_grid, # grid of values to try
            metrics = metric_set(accuracy, roc_auc) # metrics we care about
  )
# # Tuning de Mtry: regardons le meilleur modèle:
rf_tune_results %>%
  collect_metrics()
# # On prend le meilleur résultat de la metric "accuracy"
param_final <- rf_tune_results %>%
  select_best(metric = "accuracy")

# On l'intègre pour finaliser le workflow
rf_workflow <- rf_workflow %>%
  finalize_workflow(param_final)

rf_fit <- rf_workflow %>%
  # fit on the training set and evaluate on test set
  last_fit(df_split)

# check performance
test_performance <- rf_fit %>% collect_metrics()

test_predictions <- rf_fit %>% collect_predictions()
