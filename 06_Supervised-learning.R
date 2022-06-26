## -----------------------------------------------------------------------------
cars |> head()


## ----dist-vs-speed, fig.cap="Plot of breaking distance versus speed for cars."----
cars |> ggplot(aes(x = speed, y = dist)) +
  geom_point() +
  geom_smooth(formula = y ~ x, method = "lm")


## -----------------------------------------------------------------------------
cars %>% lm(dist ~ speed, data = .) %>% summary()


## -----------------------------------------------------------------------------
cars %>% lm(dist ~ speed, data = .) %>% coefficients()
cars %>% lm(dist ~ speed, data = .) %>% confint()


## ----lin-regress-choices-of-thetas, fig.cap="Prediction lines for different choices of parameters."----
predict_dist <- function(speed, theta_1) 
  data.frame(speed = speed, 
             dist = theta_1 * speed, 
             theta = as.factor(theta_1))

cars %>% ggplot(aes(x = speed, y = dist, colour = theta)) +
  geom_point(colour = "black") +
  geom_line(data = predict_dist(cars$speed, 2)) +
  geom_line(data = predict_dist(cars$speed, 3)) +
  geom_line(data = predict_dist(cars$speed, 4)) +
  scale_color_discrete(name=expression(theta[1]))


## ----lin-regress-errors-for-thetas, fig.cap="Error values for different choices of parameters."----

# Get the error value for the specific theta
fitting_error <- Vectorize(function(theta) 
  sum((theta * cars$speed - cars$dist)**2)
)

# Plot the errors for a range of thetas
tibble(theta = seq(0, 5, length.out = 50)) |> # set the theta values
  mutate(errors = fitting_error(theta))    |> # add the errors
  ggplot(aes(x = theta, y = errors)) +
  geom_line() +
  xlab(expression(theta[1])) + ylab(expression(E(theta[1])))


## -----------------------------------------------------------------------------
cars %>% lm(dist ~ speed - 1, data = .) %>% coefficients()


## ----lin-regress-best-theta, fig.cap="Best regression line going through (0,0)."----
cars |> ggplot(aes(x = speed, y = dist)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x - 1)


## -----------------------------------------------------------------------------
library(mlbench)
data("BreastCancer")
BreastCancer |> head()


## ----breast_cancer_data, fig.cap="Breast cancer class versus clump thickness"----
BreastCancer |> 
  ggplot(aes(x = Cl.thickness, y = Class)) +
  geom_jitter(height = 0.05, width = 0.3, alpha = 0.4)


## ----breast_cancer_logistic_regression, fig.cap="Logistic regression fit to breast cancer data."----
BreastCancer |>
  mutate(Thickness = 
              as.numeric(as.character(Cl.thickness))) |>
  mutate(Malignant = ifelse(Class != "benign", 1, 0)) |>
  ggplot(aes(x = Thickness, y = Malignant)) +
  geom_jitter(height = 0.05, width = 0.3, alpha = 0.4) +
  geom_smooth(method = "glm", formula = y ~ x,
              method.args = list(family = "binomial"))


## ---- warnings=FALSE----------------------------------------------------------
BreastCancer %>% 
  mutate(Thickness = 
              as.numeric(as.character(Cl.thickness))) %>%
  mutate(Malignant = ifelse(Class != "benign", 1, 0)) %>%
  glm(Malignant ~ Thickness, 
      family = "binomial",
      data = .) 


## -----------------------------------------------------------------------------
cars %>%
  model.matrix(dist ~ speed, data = .) %>%
  head(5)


## -----------------------------------------------------------------------------
cars %>%
  model.matrix(dist ~ speed - 1, data = .) %>%
  head(5)


## -----------------------------------------------------------------------------
BreastCancer %>%
  mutate(Thickness =
                 as.numeric(as.character(Cl.thickness)),
         CellSize =
                 as.numeric(as.character(Cell.size))) %>%
  mutate(Malignant = ifelse(Class != "benign", 1, 0)) %>%
  model.matrix(Malignant ~ Thickness + CellSize,
               data = .) %>%
  head(5)


## -----------------------------------------------------------------------------
BreastCancer %>%
  mutate(Thickness =
                 as.numeric(as.character(Cl.thickness)),
         CellSize =
                 as.numeric(as.character(Cell.size))) %>%
  mutate(Malignant = ifelse(Class != "benign", 1, 0)) %>%
  glm(Malignant ~ Thickness + CellSize,
      family = "binomial", 
      data = .)


## -----------------------------------------------------------------------------
BreastCancer %>%
  mutate(Malignant = ifelse(Class != "benign", 1, 0)) %>%
  model.matrix(Malignant ~ Bare.nuclei, data = .) %>%
  head(5)


## -----------------------------------------------------------------------------
BreastCancer %>%
  mutate(Malignant = ifelse(Class != "benign", 1, 0)) %>%
  model.matrix(Malignant ~ Cl.thickness, data = .)    %>%
  head(5)


## -----------------------------------------------------------------------------
BreastCancer %>%
  mutate(Thickness =
           as.numeric(as.character(Cl.thickness)),
         CellSize =
           as.numeric(as.character(Cell.size))) %>%
  mutate(Malignant = ifelse(Class != "benign", 1, 0)) %>%
  model.matrix(Malignant ~ Thickness * CellSize,
               data = .) %>%
  head(5)


## -----------------------------------------------------------------------------
BreastCancer %>%
  mutate(Thickness =
           as.numeric(as.character(Cl.thickness))) %>%
  mutate(Malignant = ifelse(Class != "benign", 1, 0)) %>%
  model.matrix(Malignant ~ Thickness * Bare.nuclei, data = .) %>%
  head(3)


## -----------------------------------------------------------------------------
BreastCancer %>%
  mutate(Thickness =
           as.numeric(as.character(Cl.thickness))) %>%
  mutate(Malignant = ifelse(Class != "benign", 1, 0)) %>%
  model.matrix(Malignant ~ Thickness : Bare.nuclei, data = .) %>%
  head(3)


## -----------------------------------------------------------------------------
cars %>%
  model.matrix(dist ~ speed + speed^2, data = .) %>%
  head()


## -----------------------------------------------------------------------------
cars %>%
  model.matrix(dist ~ speed + I(speed^2), data = .) %>%
  head()


## -----------------------------------------------------------------------------
cars %>% lm(dist ~ speed + I(speed^2), data = .) %>%
  summary()


## ----polynomial-cars, fig.cap="The cars data fitted to a second degree polynomial."----
cars %>% ggplot(aes(x = speed, y = dist)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2))


## -----------------------------------------------------------------------------
line <- cars %>% lm(dist ~ speed, data = .)
poly <- cars %>% lm(dist ~ speed + I(speed^2), data = .)

predict(line, cars) |> head()
predict(poly, cars) |> head()


## -----------------------------------------------------------------------------
rmse <- function(x,t) sqrt(mean(sum((t - x)^2)))

rmse(predict(line, cars), cars$dist)
rmse(predict(poly, cars), cars$dist)


## -----------------------------------------------------------------------------
training_data <- cars[1:25,]
test_data <- cars[26:50,]

line <- training_data %>% lm(dist ~ speed, data = .)
poly <- training_data %>% lm(dist ~ speed + I(speed^2), data = .)

rmse(predict(line, test_data), test_data$dist)
rmse(predict(poly, test_data), test_data$dist)


## -----------------------------------------------------------------------------
sampled_cars <- cars |>
  mutate(training = sample(0:1, nrow(cars), replace = TRUE))

sampled_cars |> head()


## -----------------------------------------------------------------------------
training_data <- sampled_cars |> filter(training == 1)
test_data <- sampled_cars |> filter(training == 0)

training_data |> head()
test_data |> head()


## -----------------------------------------------------------------------------
line <- training_data %>% lm(dist ~ speed, data = .)
poly <- training_data %>% lm(dist ~ speed + I(speed^2), data = .)

rmse(predict(line, test_data), test_data$dist)
rmse(predict(poly, test_data), test_data$dist)


## -----------------------------------------------------------------------------
formatted_data <- BreastCancer |>
  mutate(Thickness =
           as.numeric(as.character(Cl.thickness)),
         CellSize =
           as.numeric(as.character(Cell.size))) %>%
  mutate(Malignant = ifelse(Class != "benign", 1, 0)) 

fitted_model <- formatted_data %>%
  glm(Malignant ~ Thickness + CellSize, 
      family = "binomial",
      data = .)


## -----------------------------------------------------------------------------
predict(fitted_model, formatted_data, type = "response") |> head()


## -----------------------------------------------------------------------------
classify <- function(probability) ifelse(probability < 0.5, 0, 1)
classified_malignant <- classify(predict(fitted_model, formatted_data))


## -----------------------------------------------------------------------------
table(formatted_data$Malignant, classified_malignant)


## -----------------------------------------------------------------------------
table(formatted_data$Malignant, classified_malignant,
      dnn = c("Data", "Predictions"))


## -----------------------------------------------------------------------------
classify <- function(probability)
  ifelse(probability < 0.5, "benign", "malignant")
classified <- classify(predict(fitted_model, formatted_data))

table(formatted_data$Class, classified,
      dnn=c("Data", "Predictions"))


## -----------------------------------------------------------------------------
confusion_matrix <- table(formatted_data$Class, classified,
                          dnn=c("Data", "Predictions"))
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
accuracy


## -----------------------------------------------------------------------------
table(BreastCancer$Class)


## -----------------------------------------------------------------------------
tbl <- table(BreastCancer$Class)
tbl["benign"] / sum(tbl)


## -----------------------------------------------------------------------------
table(BreastCancer$Class, sample(BreastCancer$Class))


## -----------------------------------------------------------------------------
accuracy <- function(confusion_matrix)
  sum(diag(confusion_matrix))/sum(confusion_matrix)
sample_table <- function()
  table(BreastCancer$Class, sample(BreastCancer$Class))

replicate(8, sample_table() |> accuracy())


## -----------------------------------------------------------------------------
(specificity <- confusion_matrix[1,1] /
  (confusion_matrix[1,1] + confusion_matrix[1,2]))


## -----------------------------------------------------------------------------
(sensitivity <- confusion_matrix[2,2]/
  (confusion_matrix[2,1] + confusion_matrix[2,2]))


## -----------------------------------------------------------------------------
specificity <- function(confusion_matrix)
  confusion_matrix[1,1] /
  (confusion_matrix[1,1]+confusion_matrix[1,2])

sensitivity <- function(confusion_matrix) 
  confusion_matrix[2,2] / 
  (confusion_matrix[2,1]+confusion_matrix[2,2])

prediction_summary <- function(confusion_matrix) 
  c("accuracy" = accuracy(confusion_matrix),
    "specificity" = specificity(confusion_matrix),
    "sensitivity" = sensitivity(confusion_matrix))

random_prediction_summary <- function()
  prediction_summary(
    table(BreastCancer$Class, sample(BreastCancer$Class))
  )

replicate(3, random_prediction_summary())


## -----------------------------------------------------------------------------
confusion_matrix[2,1] / sum(confusion_matrix[,1])


## -----------------------------------------------------------------------------
confusion_matrix[1,1] / sum(confusion_matrix[,1])


## -----------------------------------------------------------------------------
confusion_matrix[2,2] / sum(confusion_matrix[,2])
confusion_matrix[1,2] / sum(confusion_matrix[,2])


## -----------------------------------------------------------------------------
permuted_cars <- cars[sample(1:nrow(cars)),]
permuted_cars |> head(3)


## -----------------------------------------------------------------------------
permute_rows <- function(df) df[sample(1:nrow(df)),]


## -----------------------------------------------------------------------------
permuted_cars <- cars |> permute_rows()


## -----------------------------------------------------------------------------
group_data <- function(df, n) {
  groups <- rep(1:n, each = nrow(df)/n)
  split(df, groups)
}


## -----------------------------------------------------------------------------
grouped_cars <- cars |> permute_rows() |> group_data(5)
grouped_cars |> str()
grouped_cars[[1]] # First sample


## -----------------------------------------------------------------------------
grouped_cars[1]


## -----------------------------------------------------------------------------
lm(dist ~ speed, data = grouped_cars[[1]])$coefficients


## -----------------------------------------------------------------------------
get_coef <- function(df) 
  lm(dist ~ speed, data = df)$coefficients

# Get estimates from first group
estimates <- get_coef(grouped_cars[[1]])
for (i in 2:length(grouped_cars)) {
  # Append the next group
  estimates <- rbind(estimates, get_coef(grouped_cars[[i]]))
}

estimates


## ---- echo=FALSE, warning=FALSE-----------------------------------------------
suppressPackageStartupMessages(library(purrr, quietly = TRUE))


## -----------------------------------------------------------------------------
estimates <- grouped_cars |> map_df(get_coef)


## -----------------------------------------------------------------------------
cross_validation_groups <- function(grouped_df) {
  remove_group <- function(group) 
    # remove group "group" from the list
    grouped_df[-group] |>
    # merge the remaining groups into one data frame
    bind_rows()           
  
  # Iterate over indices from 1 to number of groups
  seq_along(grouped_df) |> 
    # get the data frame with this group removed
    map(remove_group)      
}


## -----------------------------------------------------------------------------
cars |> 
  permute_rows() |> # randomize for safety...
  group_data(5) |> # get us five groups 
  cross_validation_groups() |> # then make five cross-validation groups
  # For each cross-validation group, estimate the cofficients and put
  # the results in a data frame
  map_df(
    # We need a lambda expression here because lm doesn't take
    # the data frame as its first argument
    \(df) lm(dist ~ speed, data = df)$coefficients
  )


## -----------------------------------------------------------------------------
cross_validation_split <- function(grouped_df) {
  seq_along(grouped_df) |> map(
    \(group) list(
      # Test is the current group
      test = grouped_df[[group]],
      # Training is all the others
      training = grouped_df[-group] |> bind_rows()
    ))
}


## -----------------------------------------------------------------------------
prediction_accuracy <- function(test_and_training) {
  test_and_training |>
    map_dbl(
      \(tt) {
       # Fit the model using training data
       fit <- lm(dist ~ speed, data = tt$training)
       # Then make predictions on the test data
       predictions <- predict(fit, newdata = tt$test)
       # Get root mean square error of result
       rmse(predictions, tt$test$dist)
      }
    )
}


## -----------------------------------------------------------------------------
cars |>
  permute_rows() |>
  group_data(5) |>
  cross_validation_split() |>
  prediction_accuracy()


## -----------------------------------------------------------------------------
random_group <- function(n, probs) {
  probs <- probs / sum(probs)
  g <- findInterval(seq(0, 1, length = n), c(0, cumsum(probs)),
                    rightmost.closed = TRUE)
  names(probs)[sample(g)]
}


## -----------------------------------------------------------------------------
random_group(8, c(training = 0.5, test = 0.5))
random_group(8, c(training = 0.5, test = 0.5))


## -----------------------------------------------------------------------------
random_group(8, c(training = 0.8, test = 0.2))


## -----------------------------------------------------------------------------
partition <- function(df, n, probs) {
  replicate(n, split(df, random_group(nrow(df), probs)), FALSE)
}


## -----------------------------------------------------------------------------
random_cars <- cars |> partition(4, c(training = 0.5, test = 0.5))


## -----------------------------------------------------------------------------
random_cars |> prediction_accuracy()


## -----------------------------------------------------------------------------
library(rpart)

model <- cars %>% rpart(dist ~ speed, data = .) 
rmse(predict(model, cars), cars$dist)


## -----------------------------------------------------------------------------
model <- BreastCancer %>%
  rpart(Class ~ Cl.thickness, data = .)


## -----------------------------------------------------------------------------
predict(model, BreastCancer) |> head()


## -----------------------------------------------------------------------------
predicted_class <- 
  predict(model, BreastCancer) %>% 
  as.data.frame() %$%
  ifelse(benign > 0.5, "benign", "malignant")

table(BreastCancer$Class, predicted_class)


## ---- echo=FALSE, warning=FALSE-----------------------------------------------
suppressPackageStartupMessages(library(party, quietly = TRUE))


## -----------------------------------------------------------------------------
model <- cars %>% ctree(dist ~ speed, data = .)
rmse(predict(model, cars), cars$dist)


## -----------------------------------------------------------------------------
model <- BreastCancer %>%
  ctree(Class ~ Cl.thickness, data = .)

predict(model, BreastCancer) %>% head()

table(BreastCancer$Class, predict(model, BreastCancer))


## ----cars_ctree_plot, fig.cap="Plot of the cars decision tree."---------------
cars %>% ctree(dist ~ speed, data = .) %>% plot()


## ---- echo=FALSE, warning=FALSE-----------------------------------------------
suppressPackageStartupMessages(library(randomForest, quietly = TRUE))


## -----------------------------------------------------------------------------
model <- cars %>% randomForest(dist ~ speed, data = .)
rmse(predict(model, cars), cars$dist)


## -----------------------------------------------------------------------------
model <- BreastCancer %>%
  randomForest(Class ~ Cl.thickness, data = .)

predict(model, BreastCancer) %>% head()

table(BreastCancer$Class, predict(model, BreastCancer))


## ---- echo=FALSE, warning=FALSE-----------------------------------------------
suppressPackageStartupMessages(library(nnet, quietly = TRUE))


## -----------------------------------------------------------------------------
model <- cars %>% nnet(dist ~ speed, data = ., size = 5) 
rmse(predict(model, cars), cars$dist)


## -----------------------------------------------------------------------------
model <- BreastCancer %>%
  nnet(Class ~ Cl.thickness, data = ., size = 5)


## -----------------------------------------------------------------------------
predict(model, BreastCancer) %>% head()


## -----------------------------------------------------------------------------
predicted_class <- predict(model, BreastCancer) %>%
  { ifelse(. < 0.5, "benign", "malignant") }

table(BreastCancer$Class, predicted_class)


## ---- echo=FALSE, warning=FALSE-----------------------------------------------
suppressPackageStartupMessages(library(kernlab, quietly = TRUE))


## -----------------------------------------------------------------------------
model <- cars %>% ksvm(dist ~ speed, data = .)
rmse(predict(model, cars), cars$dist)


## -----------------------------------------------------------------------------
model <- BreastCancer %>%
  ksvm(Class ~ Cl.thickness, data = .)

predict(model, BreastCancer) %>% head()

table(BreastCancer$Class, predict(model, BreastCancer))


## ---- echo=FALSE, warning=FALSE-----------------------------------------------
suppressPackageStartupMessages(library(e1071, quietly = TRUE))


## -----------------------------------------------------------------------------
model <- BreastCancer %>%
  naiveBayes(Class ~ Cl.thickness, data = .)

predict(model, BreastCancer) %>% head

table(BreastCancer$Class, predict(model, BreastCancer))

