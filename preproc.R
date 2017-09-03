preproc <- function(model)
{
  
  out <- as.character()
  
  if(model %in% c("enet", "gaussprLinear", "gaussprPoly",
                  "gaussprRadial", "glmnet", "gpls", "knn",
                  "lars", "lars2", "lasso", "lssvmLinear",
                  "lssvmPoly", "lssvmRadial", "multinom",
                  "neuralnet", "nnet", "penalized", "pls",
                  "relaxo", "rocc", "rvmLinear", "rvmPoly",
                  "rvmRadial", "smda", "sparseLDA", "spls",
                  "superpc", "svmLinear", "svmPoly", "svmRadial", 
                  "cubist", "gamboost", "brnn", "avNNet")) 
    out <- c(out, "center")
  
  if(model %in% c("gaussprLinear", "gaussprPoly",
                  "gaussprRadial", "glmnet", "gpls", "knn",
                  "lars", "lars2", "lasso", "lssvmLinear",
                  "lssvmPoly", "lssvmRadial", "multinom",
                  "neuralnet", "nnet", "penalized", "pls",
                  "relaxo", "rocc", "rvmLinear", "rvmPoly",
                  "rvmRadial", "smda", "sparseLDA", "spls",
                  "superpc", "svmLinear", "svmPoly", "svmRadial", 
                  "cubist", "gamboost", "brnn", "avNNet"))
    out <- c(out, "scale")
  
  if(model %in% c("enet", "gaussprLinear", "gaussprPoly",
                  "gaussprRadial", "gpls", "hda", "hdda", "icr",
                  "knn", "lars", "lars2", "lasso", "lda", "knn",
                  "lm", "lmStepAIC", "glm", "glmStepAIC",
                  "lssvmLinear", "lssvmPoly", "lssvmRadial",
                  "lvq", "mda", "multinom", "nb",
                  "neuralnet", "nnet", "pam", "pcaNNet",  "pcr",
                  "pda", "pda2", "penalized", "pls", "qda",
                  "QdaCov", "rda", "relaxo", "rocc",
                  "rvmLinear", "rvmPoly", "rvmRadial",
                  "scrda", "sda", "sddaLDA", "sddaQDA",      
                  "slda", "smda", "sparseLDA", "spls",         
                  "stepLDA", "stepQDA", "superpc", "svmLinear",  
                  "svmPoly", "svmRadial", "vbmpRadial", "brnn", "avNNet")) 
  out <- c(out, "nzv")
  
  # if(model %in% c("enet", "lars", "lars2", "lasso", "lda",
  #                 "lm", "lmStepAIC", "glm", "glmStepAIC",
  #                 "multinom", "nb", "neuralnet", "nnet",
  #                 "pda", "pda2", "penalized", "relaxo","brnn", "avNNet")) 
  #   out <- c(out, "corr")
  
  if(model %in% c("rlm" ))
    out <- c(out, "pca")
  if (length(out) == 0)
    out <- NULL
  out
}
