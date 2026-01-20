# install.packages("tidyverse")

library(shiny)
library(vroom)
library(tidyverse)

# dir.create("data")

# download <- function(name) {
#   url <- "https://github.com/hadley/mastering-shiny/raw/master/neiss/"
#   download.file(paste0(url, name), paste0("data/", name), quiet = TRUE)
# }
# download("injuries.tsv.gz")
# download("population.tsv")
# download("products.tsv")

injuries <- vroom::vroom("data/injuries.tsv.gz")
injuries
products <- vroom::vroom('data/products.tsv')
products
population <- vroom::vroom('data/population.tsv')
population

selected <- injuries |>
  filter(prod_code == 649)
nrow(selected)

products |>
  filter(prod_code == 649)

selected |>
  count(location, wt = weight, sort = T)

selected |>
  count(body_part, wt = weight, sort = T)

selected |>
  count(diag, wt = weight, sort = T)

summary <- selected |>
  count(age, sex, wt = weight)
summary

summary |>
  ggplot(aes(age, n, color = sex)) +
  geom_line() +
  labs(y = "Estimated number of injuries")

summary <- selected |>
  count(age, sex, wt = weight) |>
  left_join(population, by = c("age", "sex")) |>
  mutate(rate = n / population * 1e4)
summary

summary |>
  ggplot(aes(age, rate, color = sex)) +
  geom_line(na.rm = T) +
  labs(y = "Injuries per 10,000 people")
