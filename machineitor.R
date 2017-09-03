# Header ------------------------------------------------------------------

#   Programa para el calculo de múltiples modelos de Regression
#   Antonio Ferreras
#   17 de Agosto de 2017
#

# Begin -------------------------------------------------------------------

long <- T
allModels <- list()

run <- list()
run[["preprocess"]] <- T

F_SALIDA <-
  paste("~/Chapter10/",
        format(Sys.time(), "%y%m%d") ,
        "ws.RData",
        sep =   "")

salvar <- function() {
  save(allModels, file = F_SALIDA)
}

# Servidor Multicore
multitarea <- function(num_cores) {
  # Servidor Multicore
  args = (commandArgs())
  multitarea <- (length(args) == 0)
  if (!multitarea) {
    multitarea <- (args[[1]] != "RStudio")
  }
  if (multitarea) {
    library(doMC)
    registerDoMC(cores = num_cores)
    print("MULTITAREA!!")
  }
}

multitarea(4)

library(caret)
source("grid.R")
source("preproc.R")

modelos <- c(
  "lm",
  "rlm",
  "pls",
  "enet",
  "ridge",
  "lasso",
  "earth",
  "svmLinear",
  "svmRadial",
  "svmPoly",
  "gaussprLinear",
  "gaussprRadial",
  "gaussprPoly",
  "knn",
  "nnet",
  "brnn",
  "pcaNNet",
  "avNNet",
  "rpart",
  "rpart2",
  "ctree",
  "M5",
  "treebag",
  "rf",
  "gbm",
  "blackboost",
  "gamboost",
  "cubist"
)

info <-
  lapply(modelos, function(x) {
    getModelInfo(x, regex = F)[[1]]
  })
names(info) <- modelos

TIPO = "Regression"
for (modelo in modelos) {
  # print(info[[modelo]])
  if (TIPO %in% info[[modelo]]$type) {
    print(info[[modelo]]$label)
    run[[modelo]] <- T
  }
}

# run["lm"] <- F
# run["rlm"] <- F
# run["pls"] <- F
# run["enet"] <- F
# run["ridge"] <- F
# run["lasso"] <- F
# run["earth"] <- F
# run["svmLinear"] <- F
# run["svmRadial"] <- F
# run["svmPoly"] <- F
# run["gaussprLinear"] <- F
# run["gaussprRadial"] <- F
# run["gaussprPoly"] <- F
# run["knn"] <- F
# run["nnet"] <- F
# run["brnn"] <- F
# run["pcaNNet"] <- F
# run["avNNet"] <- F
# run["rpart"] <- F
# run["rpart2"] <- F
# run["ctree"] <- F
# run["M5"] <- F
# run["treebag"] <- F
# run["rf"] <- F
# run["gbm"] <- F
# run["blackboost"] <- F
# run["gamboost"] <- F
# run["cubist"] <- F

# for (modelo in modelos) {
#   libreria <- getModelInfo(modelo, regex = F)[[1]]$library
#   if (!is.null(libreria)) {
#     do.call("requireNamespace", list(package = libreria))
#   }
# }
# rm(libreria)

SEED <- 1966

# pre process -----------------------------------------------------------------

if (run[["preprocess"]]) {
  print("0. Preprocessing")
  
  library(AppliedPredictiveModeling)
  data(concrete)
  detach("package:AppliedPredictiveModeling", unload = TRUE)
  
  # featurePlot(concrete[,-9], concrete[,9], type = c("g", "p", "smooth"))
  library("plyr")
  # Para eliminar los duplicados
  averaged <-
    ddply(mixtures, .(
      Cement,
      BlastFurnaceSlag,
      FlyAsh,
      Water,
      Superplasticizer,
      CoarseAggregate,
      FineAggregate,
      Age
    ),
    function(x)
      c(CompressiveStrength =
          mean(x$CompressiveStrength)))
  
  set.seed(SEED)
  
  forTraining <- createDataPartition(averaged$CompressiveStrength,
                                     p = 0.75)[[1]]
  trainingSet <- averaged[forTraining,]
  testSet <- averaged[-forTraining,]
  
  controlObject <- trainControl(
    method = "repeatedcv",
    repeats = 5,
    number = 10,
    #returnResamp = "none",
    savePredictions = "final",
    verboseIter = T
  )
  
  controlobject <- function(tipo_modelo) {
    controlObject
  }
  
  training <- function(tipo_modelo) {
    trainingSet
  }
  
  Formula1 <-
    as.formula(
      paste(
        "CompressiveStrength ~ (.)^2 + I(Cement^2) +",
        "I(BlastFurnaceSlag^2) + I(FlyAsh^2) + I(Water^2) + ",
        "I(Superplasticizer^2) + I(CoarseAggregate^2) + ",
        "I(FineAggregate^2) + I(Age^2)"
      )
    )
  Formula2 <- as.formula("CompressiveStrength ~ .")
  
  formula <- function(tipo_modelo) {
    if (tipo_modelo %in% c("lm",
                           "rlm",
                           "pls",
                           "ridge",
                           "enet",
                           "lasso",
                           "gaussprLinear")) {
      out <- Formula1
    }
    
    if (tipo_modelo %in% c(
      "earth",
      "svmLinear",
      "svmRadial",
      "svmPoly",
      "gaussprRadial",
      "gaussprPoly",
      "knn",
      "nnet",
      "brnn",
      "pcaNNet",
      "avNNet",
      "rpart",
      "rpart2",
      "ctree",
      "M5",
      "treebag",
      "rf",
      "gbm",
      "blackboost",
      "gamboost",
      "cubist"
    )) {
      out <- Formula2
    }
    out
  }
  
  run[["preprocess"]] = F
  salvar()  # Para comprobar que puede grabar
}


# lm ----------------------------------------------------------------------

modelo <- "lm"
if (!modelo %in% modelos)
  print("Error")
if (run[[modelo]]) {
  cat(info[[modelo]]$label, "\n")
  set.seed(SEED)
  
  allModels[[modelo]] <-
    do.call("train", 
            c(
              list(
                form = formula(modelo),
                data = training(modelo),
                method = modelo,
                preProcess = preproc(modelo),
                tuneGrid = grid(modelo),
                trControl = controlobject(modelo)
              ),
              list()
            )
    )
  
  
  # allModels[[modelo]] <- train(
  #   form = formula(modelo),
  #   data = training(modelo),
  #   method = modelo,
  #   preProcess = preproc(modelo),
  #   tuneGrid = grid(modelo),
  #   trControl = controlobject(modelo)
  # )
  run[[modelo]] = F
  salvar()
}

# rlm ---------------------------------------------------------------------

modelo <- "rlm"
if (!modelo %in% modelos)
  print("Error")
if (run[[modelo]]) {
  cat(info[[modelo]]$label, "\n")
  set.seed(SEED)
  
  allModels[[modelo]] <- train(
    form = formula(modelo),
    data = training(modelo),
    method = modelo,
    tuneGrid = grid(modelo),
    preProcess = preproc(modelo),
    trControl = controlobject(modelo)
  )
  run[[modelo]] = F
  salvar()
}


# pls ---------------------------------------------------------------------

modelo <- "pls"
if (!modelo %in% modelos)
  print("Error")
if (run[[modelo]]) {
  cat(info[[modelo]]$label, "\n")
  set.seed(SEED)
  
  allModels[[modelo]] <- train(
    form = formula(modelo),
    data = training(modelo),
    method = modelo,
    preProc = preproc(modelo),
    tuneGrid = grid(modelo),
    trControl = controlobject(modelo)
  )
  run[[modelo]] = F
  salvar()
}

# ridge -------------------------------------------------------------------

modelo <- "ridge"
if (!modelo %in% modelos)
  print("Error")
if (run[[modelo]]) {
  cat(info[[modelo]]$label, "\n")
  set.seed(SEED)
  
  allModels[[modelo]] <- train(
    form = formula(modelo),
    data = training(modelo),
    method = modelo,
    preProc = preproc(modelo),
    tuneGrid = grid(modelo),
    trControl = controlobject(modelo)
  )
  run[[modelo]] = F
}

# enet --------------------------------------------------------------------

modelo <- "enet"
if (!modelo %in% modelos)
  print("Error")
if (run[[modelo]]) {
  cat(info[[modelo]]$label, "\n")
  set.seed(SEED)
  
  allModels[[modelo]] <- train(
    form = formula(modelo),
    data = training(modelo),
    method = modelo,
    preProc = preproc(modelo),
    tuneGrid = grid(modelo),
    trControl = controlobject(modelo)
  )
  run[[modelo]] = F
  salvar()
}

# lasso -------------------------------------------------------------------

modelo <- "lasso"
if (!modelo %in% modelos)
  print("Error")
if (run[[modelo]]) {
  cat(info[[modelo]]$label, "\n")
  set.seed(SEED)
  allModels[[modelo]] <- train(
    form = formula(modelo),
    data = training(modelo),
    method = modelo,
    preProc = preproc(modelo),
    tuneGrid = grid(modelo),
    trControl = controlobject(modelo)
  )
  run[[modelo]] = F
}

# earth -------------------------------------------------------------------

modelo <- "earth"
if (!modelo %in% modelos)
  print("Error")
if (run[[modelo]]) {
  cat(info[[modelo]]$label, "\n")
  set.seed(SEED)
  
  allModels[[modelo]] <- train(
    form = formula(modelo),
    data = training(modelo),
    method = modelo,
    preProc = preproc(modelo),
    tuneGrid = grid(modelo),
    trControl = controlobject(modelo)
  )
  run[[modelo]] = F
}

# svmLinear ---------------------------------------------------------------

modelo <- "svmLinear"
if (!modelo %in% modelos)
  print("Error")
if (run[[modelo]]) {
  cat(info[[modelo]]$label, "\n")
  set.seed(SEED)
  
  allModels[[modelo]] <- train(
    form = formula(modelo),
    data = training(modelo),
    method = modelo,
    preProc = preproc(modelo),
    tuneGrid = grid(modelo),
    trControl = controlobject(modelo)
  )
  run[[modelo]] = F
  salvar()
}


# svmRadial ---------------------------------------------------------------

modelo <- "svmRadial"
if (!modelo %in% modelos)
  print("Error")
if (run[[modelo]]) {
  cat(info[[modelo]]$label, "\n")
  set.seed(SEED)
  
  allModels[[modelo]] <- train(
    form = formula(modelo),
    data = training(modelo),
    method = modelo,
    preProc = preproc(modelo),
    tuneGrid = grid(modelo),
    trControl = controlobject(modelo)
  )
  run[[modelo]] = F
}

# svmPoly -----------------------------------------------------------------

modelo <- "svmPoly"
if (!modelo %in% modelos)
  print("Error")
if (run[[modelo]]) {
  cat(info[[modelo]]$label, "\n")
  set.seed(SEED)
  
  allModels[[modelo]] <- train(
    form = formula(modelo),
    data = training(modelo),
    method = modelo,
    preProc = preproc(modelo),
    tuneGrid = grid(modelo),
    trControl = controlobject(modelo)
  )
  run[[modelo]] = F
}

# gaussprLinear -----------------------------------------------------------

modelo <- "gaussprLinear"
if (!modelo %in% modelos)
  print("Error")

if (run[[modelo]]) {
  cat(info[[modelo]]$label, "\n")
  set.seed(SEED)
  allModels[[modelo]] <- train(
    form = formula(modelo),
    data = training(modelo),
    method = modelo,
    preProc = preproc(modelo),
    tuneGrid = NULL,
    trControl = controlobject(modelo)
  )
  run[[modelo]] = F
  salvar()
}

# gaussprRadial -----------------------------------------------------------

modelo <- "gaussprRadial"
if (!modelo %in% modelos)
  print("Error")
if (run[[modelo]]) {
  cat(info[[modelo]]$label, "\n")
  set.seed(SEED)
  
  allModels[[modelo]] <- train(
    form = formula(modelo),
    data = training(modelo),
    method = modelo,
    preProc = preproc(modelo),
    tuneGrid = grid(modelo),
    trControl = controlobject(modelo)
  )
  run[[modelo]] = F
  salvar()
}

# gaussprPoly -------------------------------------------------------------

modelo <- "gaussprPoly"
if (!modelo %in% modelos)
  print("Error")
if (run[[modelo]]) {
  cat(info[[modelo]]$label, "\n")
  set.seed(SEED)
  
  allModels[[modelo]] <- train(
    form = formula(modelo),
    data = training(modelo),
    method = modelo,
    preProc = preproc(modelo),
    tuneGrid = grid(modelo),
    trControl = controlobject(modelo)
  )
  run[[modelo]] = F
}

# knn ---------------------------------------------------------------------

modelo <- "knn"
if (!modelo %in% modelos)
  print("Error")
if (run[[modelo]]) {
  cat(info[[modelo]]$label, "\n")
  set.seed(SEED)
  
  allModels[[modelo]] <- train(
    form = formula(modelo),
    data = training(modelo),
    method = modelo,
    preProc = preproc(modelo),
    tuneGrid = grid(modelo),
    trControl = controlobject(modelo)
  )
  run[[modelo]] = F
  salvar()
}

# nnet --------------------------------------------------------------------

modelo <- "nnet"
if (!modelo %in% modelos)
  print("Error")
if (run[[modelo]]) {
  cat(info[[modelo]]$label, "\n")
  
  set.seed(SEED)
  allModels[[modelo]] <- train(
    form = formula(modelo),
    data = training(modelo),
    method = modelo,
    preProc = preproc(modelo),
    tuneGrid = grid(modelo),
    trControl = controlobject(modelo),
    linout = 1,
    trace = F,
    maxit = 1000
    #                   MaxNWts = 10*(ncol(trainingSet)+1)+10+1,
  )
  run[[modelo]] = F
  salvar()
}

# brnn --------------------------------------------------------------------

modelo <- "brnn"
if (!modelo %in% modelos)
  print("Error")
if (run[[modelo]]) {
  cat(info[[modelo]]$label, "\n")
  set.seed(SEED)
  
  allModels[[modelo]] <- train(
    form = formula(modelo),
    data = training(modelo),
    method = modelo,
    preProc = preproc(modelo),
    tuneGrid = grid(modelo),
    trControl = controlobject(modelo),
    linout = 1,
    trace = F,
    maxit = 1000
    #                   MaxNWts = 10*(ncol(trainingSet)+1)+10+1,
  )
  run[[modelo]] = F
  salvar()
}

# pcaNNet -----------------------------------------------------------------

modelo <- "pcaNNet"
if (!modelo %in% modelos)
  print("Error")
if (run[[modelo]]) {
  cat(info[[modelo]]$label, "\n")
  set.seed(SEED)
  
  allModels[[modelo]] <- train(
    form = formula(modelo),
    data = training(modelo),
    method = modelo,
    preProc = preproc(modelo),
    tuneGrid = grid(modelo),
    trControl = controlobject(modelo),
    linout = 1,
    trace = F,
    maxit = 1000
  )
  run[[modelo]] = F
  salvar()
}

# avNNet ------------------------------------------------------------------

modelo <- "avNNet"
if (!modelo %in% modelos)
  print("Error")
if (run[[modelo]]) {
  cat(info[[modelo]]$label, "\n")
  
  set.seed(SEED)
  allModels[[modelo]]  <- train(
    form = formula(modelo),
    data = training(modelo),
    method = modelo,
    preProc = preproc(modelo),
    tuneGrid = grid(modelo),
    trControl = controlobject(modelo),
    linout = 1,
    trace = F,
    maxit = 1000
  )
  run[[modelo]] = F
  salvar()
}

# rpart -------------------------------------------------------------------

modelo <- "rpart"
if (!modelo %in% modelos)
  print("Error")
if (run[[modelo]]) {
  cat(info[[modelo]]$label, "\n")
  
  allModels[[modelo]] <- train(
    form = formula(modelo),
    data = training(modelo),
    method = modelo,
    preProc = preproc(modelo),
    tuneGrid = grid(modelo),
    trControl = controlobject(modelo)
  )
  run[[modelo]] = F
  salvar()
}

# rpart2 -------------------------------------------------------------------

modelo <- "rpart2"
if (!modelo %in% modelos)
  print("Error")
if (run[[modelo]]) {
  cat(info[[modelo]]$label, "\n")
  set.seed(SEED)
  
  allModels[[modelo]] <- train(
    form = formula(modelo),
    data = training(modelo),
    method = modelo,
    preProc = preproc(modelo),
    tuneGrid = grid(modelo),
    trControl = controlobject(modelo)
  )
  run[[modelo]] = F
  salvar()
}

# ctree -------------------------------------------------------------------

modelo <- "ctree"
if (!modelo %in% modelos)
  print("Error")
if (run[[modelo]]) {
  cat(info[[modelo]]$label, "\n")
  set.seed(SEED)
  
  allModels[[modelo]] <- train(
    form = formula(modelo),
    data = training(modelo),
    method = modelo,
    preProc = preproc(modelo),
    tuneGrid = grid(modelo),
    trControl = controlobject(modelo)
  )
  run[[modelo]] = F
  salvar()
}

# M5 ----------------------------------------------------------------------

modelo <- "M5"
if (!modelo %in% modelos)
  print("Error")
if (run[[modelo]]) {
  cat(info[[modelo]]$label, "\n")
  set.seed(SEED)
  
  multitarea(1)
  allModels[[modelo]] <- train(
    form = formula(modelo),
    data = training(modelo),
    method = modelo,
    preProc = preproc(modelo),
    tuneGrid = grid(modelo),
    trControl = controlobject(modelo)
  )
  multitarea(4)
  run[[modelo]] = F
  salvar()
}

# treebag -----------------------------------------------------------------

modelo <- "treebag"
if (!modelo %in% modelos)
  print("Error")
if (run[[modelo]]) {
  cat(info[[modelo]]$label, "\n")
  set.seed(SEED)
  allModels[[modelo]] <- train(
    form = formula(modelo),
    data = training(modelo),
    method = modelo,
    preProc = preproc(modelo),
    tuneGrid = grid(modelo),
    trControl = controlobject(modelo)
  )
  run[[modelo]] = F
  salvar()
}

# rf -----------------------------------------------------------------

modelo <- "rf"
if (!modelo %in% modelos)
  print("Error")
if (run[[modelo]]) {
  cat(info[[modelo]]$label, "\n")
  set.seed(SEED)
  
  allModels[[modelo]] <- train(
    form = formula(modelo),
    data = training(modelo),
    method = modelo,
    preProc = preproc(modelo),
    tuneGrid = grid(modelo),
    trControl = controlobject(modelo),
    ntrees = 1000,
    importance = T
  )
  run[[modelo]] = F
  salvar()
}

# gbm -----------------------------------------------------------------

modelo <- "gbm"
if (!modelo %in% modelos)
  print("Error")
if (run[[modelo]]) {
  cat(info[[modelo]]$label, "\n")
  set.seed(SEED)
  
  allModels[[modelo]] <- train(
    form = formula(modelo),
    data = training(modelo),
    method = modelo,
    preProc = preproc(modelo),
    tuneGrid = grid(modelo),
    trControl = controlobject(modelo),
    verbose = F
  )
  multitarea(1)
  run[[modelo]] = F
  salvar()
}

# blackboost -----------------------------------------------------------------

modelo <- "blackboost"
if (!modelo %in% modelos)
  print("Error")
if (run[[modelo]]) {
  cat(info[[modelo]]$label, "\n")
  set.seed(SEED)
  
  allModels[[modelo]] <- train(
    form = formula(modelo),
    data = training(modelo),
    method = modelo,
    preProc = preproc(modelo),
    tuneGrid = grid(modelo),
    trControl = controlobject(modelo)
  )
  run[[modelo]] = F
  salvar()
}

# gamboost -----------------------------------------------------------------

modelo <- "gamboost"            # 3 minutos
if (!modelo %in% modelos)
  print("Error")
if (run[[modelo]]) {
  cat(info[[modelo]]$label, "\n")
  
  allModels[[modelo]] <- train(
    form = formula(modelo),
    data = training(modelo),
    method = modelo,
    preProc = preproc(modelo),
    tuneGrid = grid(modelo),
    trControl = controlobject(modelo)
  )
  run[[modelo]] = F
  salvar()
}

# cubist -----------------------------------------------------------------

modelo <- "cubist"   # 3 minutos
if (!modelo %in% modelos)
  print("Error")
if (run[[modelo]]) {
  cat(info[[modelo]]$label, "\n")
  set.seed(SEED)
  
  allModels[[modelo]] <- train(
    form = formula(modelo),
    data = training(modelo),
    preProc = preproc(modelo),
    method = modelo,
    tuneGrid = grid(modelo),
    trControl = controlobject(modelo)
  )
  run[[modelo]] = F
  salvar()
}


# post process -----------------------------------------------------------------

allResamples <- resamples(allModels)

parallelplot(allResamples)

parallelplot(allResamples, metric = "Rsquared")

bwplot(allResamples, metric = "RMSE")
bwplot(allResamples, metric = "Rsquared")

#splom(allResamples)

difValues <- diff(allResamples)
difValues
summary(difValues)
##?xy.plot - For other images
