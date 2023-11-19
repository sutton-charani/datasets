library(RCurl); library(rpart); library(rpart.plot); library(randomForest)

# Data import
x <- getURL("https://raw.githubusercontent.com/sutton-charani/datasets/main/dark_triade.csv", 
            .encoding="UTF-8’")
dataset <- read.csv2(text=x, header=F)

# Reshaping
questions <- dataset[1, c(14:19, 24:25, 32, 34:37)]; names(questions) <- NULL
dataset <- dataset[4:234, c(14:19, 24:25, 32, 34:42)]
names(dataset) <- c("gender", "school_year", "dept", "wanted_dept", "wanted_dble_diploma_aborad",
               "ever_or_intention_abroad", "living_location", "PEP_choice", "responsability_position",
               "party", "asso_activities", "school_sociability", "won_shifumi", "mac", "narc", "psy", 
               "start_date", "subimit_date")
feats <- names(dataset)[1:13]

# Preprocessing
num_vars <- c("responsability_position", "party", "asso_activities", "school_sociability", "mac", "narc", "psy")
for (var_name in num_vars){
  dataset[, var_name] <- as.numeric(dataset[, var_name])
}
v <- dataset$ever_or_intention_abroad
v[v == "DD"] <- "yes_dd"
v[v == "Non, je n'ai pas obtenu mon choix"] <- "no_not_selected"
v[v == "Non, je n'en ai pas demandé"] <- "no_not_asked"
v[v == "Semestre"] <- "yes_semester"
dataset$ever_or_intention_abroad <- v

# Data mining
### Is psychotay predictable or explanable?
lab_name <- "psy"
n_run <- 100
df <- dataset[, c(feats, lab_name)]

acc_dummy <- c()
acc_tree <- c()
acc_forest <- c()
for (irun in 1:n_run){
  df <- df[sample(nrow(df)), ]
  train <- df[1:200, ]
  test <- df[201:231, ]
  reg_formula <- formula(paste(lab_name, "~ ."))
  tree <- rpart(reg_formula, train)
  forest <- randomForest(reg_formula, train)
  preds_dummy <- rep(mean(train[, lab_name]), nrow(test))
  preds_tree <- predict(tree, test)
  preds_forest <- predict(forest, test)
  acc_dummy <- c(acc_dummy, sqrt(mean((test[, lab_name] - preds_dummy)^2)))
  acc_tree <- c(acc_tree, sqrt(mean((test[, lab_name] - preds_tree)^2)))
  acc_forest <- c(acc_forest, sqrt(mean((test[, lab_name] - preds_forest)^2)))
}
res <- data.frame(dummy=acc_dummy, tree=acc_tree, forest=acc_forest)
colMeans(res)
boxplot(res)
###### apprently not

#tree <- rpart(formula(paste(lab_name, "~ .")), df)
#prp(tree, extra=1, varlen=0, faclen=0)

### Is gender a significant psychopathic factor?
t.test(df[df$gender=="Homme", "psy"], df[df$gender=="Femme", "psy"])
###### apprently yes