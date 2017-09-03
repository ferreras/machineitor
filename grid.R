grid <- function(tipo_modelo) {
  if (long) {
    switch(
      tipo_modelo,
      lm = {
        g <- NULL
      },
      rlm = {
        g <- expand.grid(
          .intercept = T,
          .psi = c("psi.huber", "psi.hampel", "psi.bisquare")
        )
      },
      pls = {
        g <- expand.grid(.ncomp = c(1:25))
      },
      enet = {
        g <- expand.grid(
          .lambda = seq(0.001, 0.01, length = 10),
          .fraction = seq(0.5, 1, length = 6)
        )
      },
      ridge = {
        g <- expand.grid(.lambda = seq(.001, 0.01, length = 10))
      },
      lasso = {
        g <- expand.grid(.fraction = seq(0.05, 1, length = 20))
      },
      earth = {
        g <- expand.grid(.degree = 1:2,
                         .nprune = c(12:22))
      },
      svmLinear = {
        g <- expand.grid(.C = seq(0.007, 0.015, length = 9))
      },
      svmRadial = {
        g <- expand.grid(.sigma = seq(0.1, 0.3, by = 0.05),
                         .C = 2 ^ (2:6))
      },
      svmPoly = {
        g <- expand.grid(
          .degree = c(3:4),
          .scale = seq(0.05, 0.2, length = 7),
          .C = c(2, 3, 4)
        )
      },
      gaussprLinear = {
        g <- NULL
      },
      gaussprRadial = {
        g <- expand.grid(.sigma = seq(0.1, 0.25, length = 7))
      },
      gaussprPoly = {
        g <- expand.grid(.scale = seq(0.1, 0.7, by = 0.1),
                         .degree = c(3:6))
      },
      knn = {
        g <- expand.grid(.k = 1:9)
      },
      nnet = {
        g <- expand.grid(.decay = c(0.01, 0.1, 1),
                         .size = seq(5, 30, by = 2))
      },
      brnn = {
        g <- expand.grid(.neurons = c(4:25))
      },
      pcaNNet = {
        g <- expand.grid(.decay = c(0.01, 0.1, 1),
                         .size = seq(5, 31, by = 2))
      },
      avNNet = {
        g <- expand.grid(
          .decay = c(.01, .1),
          .size = seq(15, 35, by = 2),
          .bag = F
        )
      },
      rpart = {
        g <- expand.grid(.cp = 2 ^ c(-10:-3))
      },
      rpart2 = {
        g <- expand.grid(.maxdepth = c(7:20))
      },
      ctree = {
        g <- expand.grid(.mincriterion = seq(0.01, 0.1, length = 10))
      },
      M5 = {
        g <- expand.grid(
          .pruned = c("Yes", "No"),
          .smoothed = c("Yes", "No"),
          .rules = c("Yes", "No")
        )
      },
      treebag = {
        g <- NULL
      },
      rf = {
        g <- expand.grid(.mtry = c(1:7))
      },
      gbm = {
        g <- expand.grid(
          .interaction.depth = c(7:11),
          .n.trees = seq(500, 1500, by = 500),
          .shrinkage = c(0.01, 0.1),
          .n.minobsinnode = 10
        )
      },
      blackboost = {
        g <- expand.grid(.maxdepth = c(5, 6),
                         .mstop = seq(20, 55, by = 5))
      },
      gamboost = {
        g <- expand.grid(
          .mstop = c(1500, 2000, 2500, 3000, 3500, 4000),
          .prune = c("no")
        )
      },
      cubist = {
        g <- expand.grid(.committees = c(10, 50, 100),
                         .neighbors = c(2, 3))
      }
    )
  } else {
    switch(
      tipo_modelo,
      lm = {
        g <- NULL
      },
      rlm = {
        g <- data.frame(.intercept = T, .psi = "psi.hampel")
      },
      pls = {
        g <- data.frame(.ncomp = 15)
      },
      enet = {
        g <- data.frame(.lambda = 0.002,
                        .fraction = 0.9)
      },
      ridge = {
        g <- data.frame(.lambda = 0.0024)
      },
      lasso = {
        g <- data.frame(.fraction = 0.5)
      },
      earth = {
        g <- data.frame(.degree = 2,
                        .nprune = 19)
      },
      svmLinear = {
        g <- data.frame(.C = 0.011)
      },
      svmRadial = {
        g <- data.frame(.sigma = 0.08,
                        .C = 64)
      },
      svmPoly = {
        g <- data.frame(.degree = 4,
                        .scale = 0.06,
                        .C = 3)
      },
      gaussprLinear = {
        g <- NULL
      },
      gaussprRadial = {
        g <- data.frame(.sigma = 0.17)
      },
      gaussprPoly = {
        g <- data.frame(.degree = 3, .scale = 0.1)
      },
      knn = {
        g <- data.frame(.k = 3)
      },
      nnet = {
        g <- data.frame(.decay = 1,
                        .size = 30)
      },
      brnn = {
        g <- data.frame(.neurons = 12)
      },
      pcaNNet = {
        g <- data.frame(.decay = 1, .size = 31)
      },
      avNNet = {
        g <- data.frame(.decay = .1,
                        .size = 35,
                        .bag = F)
      },
      rpart = {
        g <- data.frame(.cp = 0.0001)
      },
      rpart2 = {
        g <- data.frame(.maxdepth = 18)
      },
      ctree = {
        g <- data.frame(.mincriterion = 0.03)
      },
      M5 = {
        g <- data.frame(.pruned = "No",
                        .smoothed = "Yes",
                        .rules = "No")
      },
      treebag = {
        g <- NULL
      },
      rf = {
        g <- data.frame(.mtry = 4)
      },
      gbm = {
        g <- data.frame(.interaction.depth = 10,
                        .n.trees = 1000,
                        .shrinkage = 0.1,
                        .n.minobsinnode = 10)
      },
      blackboost = {
        g <- data.frame(.maxdepth = 8,
                        .mstop = 38)
      },
      gamboost = {
        g <- data.frame(.prune = "no", .mstop = 3000)
      },
      cubist = {
        g <- data.frame(.committees = 100, .neighbors = 2
        )
      }
    )
  }
  g
}