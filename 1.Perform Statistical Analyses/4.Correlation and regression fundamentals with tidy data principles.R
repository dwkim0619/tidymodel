# Correlation test
# - 두 변수 간에 통계적으로 유의미한 연관성(association)이 있는지를 검정하는 방법, 인과관계를 의미하지는 않음
# - 귀무가설 : 두 변수 사이의 상관관계는 없다

library(tidymodels)

data(Orange)
Orange <- as_tibble(Orange)
Orange

cor(Orange$age, Orange$circumference)

library(ggplot2)

ggplot(Orange, aes(age, circumference, color = Tree)) +
  geom_line()

Orange |>
  group_by(Tree) |>
  summarise(correlation = cor(age, circumference))

ct <- cor.test(Orange$age, Orange$circumference)
ct

str(ct)
tidy(ct)

nested <- 
  Orange |>
  nest(data = c(age, circumference))

nested |>
  mutate(
    test = map(data, ~ cor.test(.x$age, .x$circumference)),
    tidied = map(test, tidy)
) |>
  unnest(cols = tidied) |>
  select(-data, -test)

lm_fit <- lm(age ~ circumference, data = Orange)
summary(lm_fit)

tidy(lm_fit)

Orange |>
  nest(data = c(-Tree)) |>
  mutate(
    fit = map(data, ~lm(age ~ circumference, data = .x)),
    tidied = map(fit, tidy)
  ) |>
  unnest(tidied) |>
  select(-data, -fit)

mtcars <- as_tibble(mtcars)

mtcars |>
  nest(data = c(-am)) |>
  mutate(
    fit = map(data, ~lm(wt ~ mpg+qsec+gear, data = .x)),
    tidied = map(fit, tidy)
  ) |>
  unnest(tidied) |>
  select(-data, -fit)
  
regressions <- mtcars |>
  nest(data = c(-am)) |>
  mutate(
    fit = map(data, ~lm(wt ~ mpg+qsec+gear, data = .x)),
    tidied = map(fit, tidy),
    glanced = map(fit, glance),
    augmented = map(fit, augment)
  ) 
  
regressions |>
  select(tidied) |>
  unnest(tidied)

regressions |>
  select(glanced) |>
  unnest(glanced)

regressions |>
  select(augmented) |>
  unnest(augmented)


# broom
# - tidy : 

lmfit <- lm(mpg ~ wt, mtcars)
summary(lmfit)

library(broom)
tidy(lmfit)
augment(lmfit)
glance(lmfit)

glmfit <- glm(am ~ wt, mtcars, family = "binomial")
tidy(glmfit)
augment(glmfit)
glance(glmfit)

nlsfit <- nls(mpg ~ k / wt + b, mtcars, start = list(k = 1, b = 0))
tidy(nlsfit)
augment(nlsfit)
glance(nlsfit)

tt <- t.test(wt ~ am, mtcars)
tidy(tt)
glance(tt)

wt <- wilcox.test(wt ~ am, mtcars)
tidy(wt)
glance(wt)

chit <- chisq.test(xtabs(Freq ~ Sex + Class, data = as.data.frame(Titanic)))
tidy(chit)
augment(chit)
