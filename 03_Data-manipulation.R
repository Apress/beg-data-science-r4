## ----cars_plot, fig.cap="Plot of the cars dataset."---------------------------
data(cars)
head(cars)
cars %>% plot(dist ~ speed, data = .)


## -----------------------------------------------------------------------------
cars |> head(3)


## -----------------------------------------------------------------------------
cars %>% tail(3)


## -----------------------------------------------------------------------------
cars %>% summary


## -----------------------------------------------------------------------------
data(iris)
iris |> summary()


## -----------------------------------------------------------------------------
data(iris)
iris |> str()


## -----------------------------------------------------------------------------
library(mlbench)
data(BreastCancer)
BreastCancer %>% head(3)


## ---- echo=FALSE--------------------------------------------------------------
data_url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/breast-cancer-wisconsin.data"


## -----------------------------------------------------------------------------
lines <- readLines(data_url)
lines[1:5]


## -----------------------------------------------------------------------------
writeLines(lines, con = "data/raw-breast-cancer.csv")


## -----------------------------------------------------------------------------
raw_breast_cancer <- read.csv("data/raw-breast-cancer.csv")
raw_breast_cancer |> head(3)


## -----------------------------------------------------------------------------
raw_breast_cancer <- read.csv(data_url)
raw_breast_cancer |> head(3)


## ---- echo=FALSE--------------------------------------------------------------
data_url <- "data/raw-breast-cancer.csv"


## -----------------------------------------------------------------------------
raw_breast_cancer <- read.csv(data_url, header = FALSE)
raw_breast_cancer |> head(3)


## -----------------------------------------------------------------------------
names(raw_breast_cancer) <- names(BreastCancer)
raw_breast_cancer |> head(3)


## -----------------------------------------------------------------------------
raw_breast_cancer <- read.csv(data_url, header = FALSE,
                              col.names = names(BreastCancer))
raw_breast_cancer |> head(3)


## -----------------------------------------------------------------------------
formatted_breast_cancer <- raw_breast_cancer


## -----------------------------------------------------------------------------
map_class <- function(x) {
    ifelse(x == 2, "bening",
    ifelse(x == 4, "malignant",
           NA))
}
mapped <- formatted_breast_cancer$Class %>% map_class
mapped |> table()


## -----------------------------------------------------------------------------
map_class <- function(x) {
    ifelse(x == 2, "bening", "malignant")
}
mapped <- formatted_breast_cancer$Class %>% map_class
mapped |> table()


## -----------------------------------------------------------------------------
formatted_breast_cancer$Class |> unique()


## -----------------------------------------------------------------------------
dict <- c("2" = "benign", "4" = "malignant")
map_class <- function(x) dict[as.character(x)]

mapped <- formatted_breast_cancer$Class |> map_class()
mapped |> table()


## -----------------------------------------------------------------------------
mapped[1:5]


## -----------------------------------------------------------------------------
library(magrittr)
mapped %<>% unname
mapped[1:5]


## -----------------------------------------------------------------------------
# Download data and put it in a variable
raw_breast_cancer <- read.csv(
  data_url, header = FALSE,
  col.names = names(BreastCancer))

# Get a copy of the raw data that we can transform  
formatted_breast_cancer <- raw_breast_cancer

# Reformat the Class variable
formatted_breast_cancer$Class <- 
  formatted_breast_cancer$Class %>% {
    c("2" = "benign", "4" = "malignant")[as.character(.)]
  } |> factor(levels = c("benign", "malignant"))


## -----------------------------------------------------------------------------
formatted_breast_cancer %>%
    save(file = "data/formatted-breast-cancer.rda")


## -----------------------------------------------------------------------------
load("data/formatted-breast-cancer.rda")


## -----------------------------------------------------------------------------
library(mlbench)
data(BostonHousing)
str(BostonHousing)


## ---- echo=FALSE--------------------------------------------------------------
data_url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data"
tiny_url <- "http://tinyurl.com/zq2u8vx"


## -----------------------------------------------------------------------------
boston_housing <- read.table(data_url)
str(boston_housing)


## -----------------------------------------------------------------------------
str(BostonHousing)


## -----------------------------------------------------------------------------
col_classes <- rep("numeric", length(BostonHousing))
col_classes[which("chas" == names(BostonHousing))] <- "factor"


## -----------------------------------------------------------------------------
boston_housing <- read.table(data_url, 
                             col.names = names(BostonHousing),
                             colClasses = col_classes)
str(boston_housing)


## -----------------------------------------------------------------------------
library(readr)


## -----------------------------------------------------------------------------
raw_breast_cancer <- read_csv("data/raw-breast-cancer.csv",
                              show_col_types = FALSE)
raw_breast_cancer %>% head(3)


## -----------------------------------------------------------------------------
raw_breast_cancer <- read_csv("data/raw-breast-cancer.csv",
                              col_names = names(BreastCancer),
                              show_col_types = FALSE)
raw_breast_cancer %>% head(3)


## -----------------------------------------------------------------------------
# Download data and put it in a variable
raw_breast_cancer <- read.csv("data/raw-breast-cancer.csv", 
                              header = FALSE,
                              col.names = names(BreastCancer))

# Reformat the Class column as a benign/malignant factor
formatted_breast_cancer <- raw_breast_cancer |>
  mutate(
    Class = 
      case_when(Class == 2 ~ "benign", Class == 4 ~ "malignant") |>
      factor(levels = c("benign", "malignant"))
  )


## -----------------------------------------------------------------------------
iris %>% as_tibble()


## -----------------------------------------------------------------------------
iris %>% as_tibble() %>% select(Petal.Width) %>% head(3)


## -----------------------------------------------------------------------------
iris %>% as_tibble() %>% 
  select(Sepal.Width, Petal.Length) %>% head(3)


## -----------------------------------------------------------------------------
iris %>% as_tibble() %>%
  select(Sepal.Length:Petal.Length) %>% head(3)


## -----------------------------------------------------------------------------
iris |> as_tibble() |> select(starts_with("Petal")) |> head(3)
iris |> as_tibble() |> select(ends_with("Width")) |> head(3)
iris |> as_tibble() |> select(contains("etal")) |> head(3)
iris |> as_tibble() |> select(matches(".t.")) |> head(3)


## -----------------------------------------------------------------------------
iris %>% as_tibble() %>% 
  select(-starts_with("Petal")) %>% head(3)


## -----------------------------------------------------------------------------
iris %>% as_tibble() %>%
  mutate(Petal.Width.plus.Length = Petal.Width + Petal.Length) %>%
  select(Species, Petal.Width.plus.Length) %>%
  head(3)


## -----------------------------------------------------------------------------
iris %>% as_tibble() %>% 
  mutate(Petal.Width.plus.Length = Petal.Width + Petal.Length,
         Sepal.Width.plus.Length = Sepal.Width + Sepal.Length) %>%
  select(Petal.Width.plus.Length, Sepal.Width.plus.Length) %>%
  head(3)


## -----------------------------------------------------------------------------
iris %>% as_tibble() %>% 
  transmute(Petal.Width.plus.Length = Petal.Width + Petal.Length) %>%
  head(3)


## -----------------------------------------------------------------------------
iris %>% as_tibble() %>% 
  arrange(Sepal.Length) %>%
  head(3)


## -----------------------------------------------------------------------------
iris %>% as_tibble() %>% 
  arrange(desc(Sepal.Length)) %>%
  head(3)


## -----------------------------------------------------------------------------
iris %>% as_tibble() %>% 
  filter(Sepal.Length > 5) %>%
  head(3)


## -----------------------------------------------------------------------------
iris %>% as_tibble() %>% 
  filter(Sepal.Length > 5 & Species == "virginica") %>%
  select(Species, Sepal.Length) %>%
  head(3)


## -----------------------------------------------------------------------------
iris %>% as_tibble() %>% group_by(Species) %>% head(3)


## -----------------------------------------------------------------------------
iris %>% 
  summarise(Mean.Petal.Length = mean(Petal.Length),
            Mean.Sepal.Length = mean(Sepal.Length))


## -----------------------------------------------------------------------------
iris %>% 
  group_by(Species) %>% 
  summarise(Mean.Petal.Length = mean(Petal.Length))


## -----------------------------------------------------------------------------
grouped_iris <- iris %>% as_tibble() %>%
  group_by(Species, Petal.Length)
grouped_iris %>% group_vars()


## -----------------------------------------------------------------------------
grouped_iris %>%
  summarise(Mean.Petal.Length = mean(Petal.Length),
            .groups = "drop_last") %>%
  group_vars()


## -----------------------------------------------------------------------------
grouped_iris %>%
  summarise(Mean.Petal.Length = mean(Petal.Length),
            .groups = "drop") %>%
  group_vars()


## -----------------------------------------------------------------------------
grouped_iris %>%
  summarise(Mean.Petal.Length = mean(Petal.Length),
            .groups = "keep") %>%
  group_vars()


## -----------------------------------------------------------------------------
grouped_iris %>%
  summarise(Mean.Petal.Length = mean(Petal.Length),
            .groups = "rowwise") %>%
  group_vars()


## -----------------------------------------------------------------------------
grouped_iris %>%
  summarise(Mean.Petal.Length = mean(Petal.Length),
            .groups = "rowwise") %>% 
  class()


## -----------------------------------------------------------------------------
iris %>%
  summarise(Observations = n())


## -----------------------------------------------------------------------------
iris %>%
  group_by(Species) %>% 
  summarise(Number.Of.Species = n(), .groups = "drop")


## -----------------------------------------------------------------------------
iris %>% 
  group_by(Species) %>%
  summarise(Number.Of.Samples = n(),
            Mean.Petal.Length = mean(Petal.Length),
            .groups = "drop")


## -----------------------------------------------------------------------------
formatted_breast_cancer <- raw_breast_cancer |>
  as_tibble() |>
  mutate(
    Class = 
      case_when(Class == 2 ~ "benign", Class == 4 ~ "malignant") |>
      factor(levels = c("benign", "malignant"))
  )


## -----------------------------------------------------------------------------
formatted_breast_cancer |> select(Normal.nucleoli:Class) |> head(5)


## -----------------------------------------------------------------------------
formatted_breast_cancer %>%
  group_by(Class) %>%
  summarise(mean.thickness = mean(Cl.thickness), .groups = "drop")


## -----------------------------------------------------------------------------
formatted_breast_cancer %>%
  group_by(Class) %>%
  summarise(mean.size = mean(Cell.size), .groups = "drop")


## -----------------------------------------------------------------------------
formatted_breast_cancer %>%
  arrange(Cell.size) %>%
  group_by(Cell.size, Class) %>%
  summarise(ClassCount = n(), .groups = "drop")


## -----------------------------------------------------------------------------
formatted_breast_cancer %>%
  group_by(Class, as.factor(Cell.size)) %>%
  summarise(mean.thickness = mean(Cl.thickness), 
            .groups = "drop")


## -----------------------------------------------------------------------------
formatted_breast_cancer %>%
  group_by(as.factor(Cell.size), Class) %>%
  summarise(mean.thickness = mean(Cl.thickness), 
            .groups = "drop")


## -----------------------------------------------------------------------------
iris |>
  as_tibble() |>
  select(Species, Petal.Length) |>
  head(3)


## ----iris_species_vs_petal_length, fig.cap="Plot species versus petal length."----
iris %>% 
  select(Species, Petal.Length) %>%
  qplot(Species, Petal.Length, geom = "boxplot", data = .)


## -----------------------------------------------------------------------------
iris |>
  pivot_longer(
    c(Sepal.Length, Sepal.Width),
    names_to = "Attribute", 
    values_to = "Measurement"
  ) |>
  head()


## -----------------------------------------------------------------------------
iris |>
  pivot_longer(
    c(Sepal.Length, Sepal.Width),
    names_to = "Attribute", 
    values_to = "Measurement"
  ) |>
  select(Species, Attribute, Measurement) |>
  head(3)


## ----iris_attributes_vs_mesurements, fig.cap="Plot measurements versus values."----
iris |>
  pivot_longer(
    c(Sepal.Length, Sepal.Width),
    names_to = "Attribute", 
    values_to = "Measurement"
  ) |>
  select(Species, Attribute, Measurement) %>%
  qplot(Attribute, Measurement, 
          geom = "boxplot", 
          facets = . ~ Species, data = .)


## -----------------------------------------------------------------------------
iris |> as_tibble() |>
  pivot_longer(
    c(Sepal.Length, Sepal.Width),
    names_to = "Attribute", 
    values_to = "Measurement"
  ) |>
  pivot_wider(
    names_from = Attribute,
    values_from = Measurement
  )


## -----------------------------------------------------------------------------
iris |> as_tibble() |>
  pivot_longer(
    c(Sepal.Length, Sepal.Width),
    names_to = "Attribute", 
    values_to = "Measurement"
  ) |>
  pivot_wider(
    names_from = Attribute,
    values_from = Measurement,
    values_fn = mean
  ) |>
  # Let's just look at the columns we summarised...
  select(Sepal.Length, Sepal.Width) 

