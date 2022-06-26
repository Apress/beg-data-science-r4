## ---- echo=FALSE--------------------------------------------------------------
red <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv", header=TRUE, sep=';')
white <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv", header=TRUE, sep=';')


## -----------------------------------------------------------------------------
wines <- bind_rows(tibble(type = "red", red),
                   tibble(type = "white", white))


## ----wine-qualities, warning=FALSE, message=FALSE, fig.cap="Distribution of wine qualities."----
ggplot(wines) + 
  geom_bar(aes(x = factor(quality), fill = type), 
           position = 'dodge') +
  xlab('Quality') + ylab('Frequency')


## ---- echo=FALSE, warning=FALSE-----------------------------------------------
suppressPackageStartupMessages(library(e1071, quietly = TRUE))


## -----------------------------------------------------------------------------
random_group <- function(n, probs) {
  probs <- probs / sum(probs)
  g <- findInterval(seq(0, 1, length = n), c(0, cumsum(probs)),
                    rightmost.closed = TRUE)
  names(probs)[sample(g)]
}

partition <- function(df, n, probs) {
  replicate(n, split(df, random_group(nrow(df), probs)), FALSE)
}


## -----------------------------------------------------------------------------
accuracy <- function(confusion_matrix)
  sum(diag(confusion_matrix)) / sum(confusion_matrix)

prediction_accuracy_wines <- function(test_and_training) {
  test_and_training |>
    map_dbl(
      \(tt) {
       # Fit the model using training data
       model <- naiveBayes(type ~ ., data = tt$training)
       # Then make predictions on the test data
       predictions <- predict(model, newdata = tt$test)
       # Get accurracy of predictions
       accuracy(table(tt$test$type, predictions))
      }
    )
}


## -----------------------------------------------------------------------------
random_wines <- wines |>
    partition(4, c(training = 0.5, test = 0.5))
random_wines |> prediction_accuracy_wines()


## ---- echo=FALSE, warning=FALSE-----------------------------------------------
suppressPackageStartupMessages(library(party, quietly = TRUE))


## ---- message=FALSE, fig.cap="Decision tree for determining the type of wine."----
tree <- ctree(type ~ ., data = wines |> mutate(type = as.factor(type)), 
              control = ctree_control(minsplit = 4420))


## -----------------------------------------------------------------------------
wines |>
  group_by(type) |>
  summarise(total.mean = mean(total.sulfur.dioxide),
            total.sd = sd(total.sulfur.dioxide),
            free.mean = mean(free.sulfur.dioxide),
            free.sd = sd(free.sulfur.dioxide),
            .groups = "drop")


## ---- fig.cap="Sulfur dioxide versis volatile acidity."-----------------------
qplot(total.sulfur.dioxide, volatile.acidity, data = wines,
      color = type,
      xlab = 'Total sulfur dioxide', 
      ylab = 'Volatile acidity (VA)')


## -----------------------------------------------------------------------------
wines |>
  group_by(type) |>
  summarise(mean = mean(volatile.acidity),
            sd = sd(volatile.acidity),
            .groups = "drop")


## -----------------------------------------------------------------------------
rmse <- function(x,t) sqrt(mean(sum((t - x)^2)))

wines |> 
  # predict the mean for all the wines, regardless of
  # parameters
  mutate(null_prediction = mean(quality)) |>
  # Summerise the predictions with a root mean square error
  summarise(rmse = rmse(null_prediction, quality)) |>
  # We have a data frame with a single number now, just
  # get that number
  as.numeric()


## -----------------------------------------------------------------------------
prediction_accuracy_wines <- function(test_and_training,
                                      model_function) {
  test_and_training |>
    map_dbl(
      \(tt) {
       # Fit the model using training data
       model <- model_function(quality ~ ., data = tt$training)
       # Then make predictions on the test data
       predictions <- predict(model, newdata = tt$test)
       # Get accuracy of predictions as a root mean square error
       rmse(predictions, tt$test$quality)
      }
    )
}


## -----------------------------------------------------------------------------
null_model <- function(formula, data) {
  # Here we just remember the mean of the input by putting it in a list
  # and by wrapping it in a `structure` with class "null_model" we can 
  # define we want this model to make predictions
  structure(list(mean = mean(data$quality)),
            class = "null_model")
}

# The name predict.null_model says that if you call predict()
# on something with class "null_model", it is this function
# that R will call. Since "model" is the list we made above
# we can get the prediction by looking up "mean" in the object.
predict.null_model <- function(model, newdata) {
  rep(model$mean, each = nrow(newdata))
}


## -----------------------------------------------------------------------------
test_and_training <- wines |> 
    partition(4, c(training = 0.5, test = 0.5))
test_and_training |> prediction_accuracy_wines(null_model)


## -----------------------------------------------------------------------------
test_and_training |> prediction_accuracy_wines(lm)

