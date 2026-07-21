test_that("CART tree interprets with importance", {
  skip_if_not_installed("rpart")
  s <- interpret(rpart::rpart(Species ~ ., iris))
  expect_match(txt(s), "CART|classification and regression tree")
  expect_match(txt(s), "terminal node")
  expect_match(txt(s), "important predictors")
})

test_that("random forest interprets OOB and importance (class and regression)", {
  skip_if_not_installed("randomForest")
  set.seed(1)
  sc <- interpret(randomForest::randomForest(Species ~ ., iris, importance = TRUE))
  expect_match(txt(sc), "out-of-bag")
  expect_match(txt(sc), "accuracy")
  sr <- interpret(randomForest::randomForest(mpg ~ ., mtcars))
  expect_match(txt(sr), "variance explained")
})

test_that("SVM interprets kernel/cost/support vectors", {
  skip_if_not_installed("e1071")
  s <- interpret(e1071::svm(Species ~ ., iris))
  expect_match(txt(s), "support vector machine")
  expect_match(txt(s), "support vectors")
})

test_that("nnet ANN interprets architecture", {
  skip_if_not_installed("nnet")
  s <- interpret(nnet::nnet(Species ~ ., iris, size = 4, trace = FALSE), outcome = "species")
  expect_match(txt(s), "neural network")
  expect_match(txt(s), "hidden")
})

test_that("caret train interprets resampled performance", {
  skip_if_not_installed("caret")
  set.seed(1)
  tr <- caret::train(Species ~ ., iris, method = "rpart",
                     trControl = caret::trainControl(method = "cv", number = 3))
  s <- interpret(tr)
  expect_match(txt(s), "tuned")
  expect_match(txt(s), "Accuracy")
})
