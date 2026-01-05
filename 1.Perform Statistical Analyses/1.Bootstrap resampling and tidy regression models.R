# [목표]
# 부트스트랩으로 모수와 예측의 불확실성(분산, 신뢰구간)을 추정
# -  “값 하나”처럼 보이는 추정 결과 뒤에 숨은 신뢰도와 변동성을 수치·그림으로 드러내기 위함
# -> 특정 구간에서 부트스트랩 곡선이 넓게 퍼져 있거나 신뢰구간이 매우 넓다면, 
#    그 구간의 예측·해석은 ‘참고용’으로만 쓰고 중요한 의사결정에는 쓰지 않거나 보수적인 기준을 적용
# -> 데이터가 부족한 구간(예: 극단적인 wt 값)에서 불확실성이 크다면, 
#    그 구간의 데이터를 추가로 수집하거나, 해당 구간을 모델의 적용 범위 밖으로 명시하는 등 모델 범위 제한 조치
# ==> 여러 모델을 비교할 때는, 
#     단순히 평균 성능만 보지 않고 부트스트랩 분포·신뢰구간을 함께 보고, 
#     불확실성이 더 작은 모델을 우선 선택하는 식으로 모델 선택 기준에 반영

# 부트스트랩은 
#  - 전체 데이터를 복원추출로 여러 번 다시 뽑아(부트스트랩 샘플), 
#  - 각 샘플마다 동일한 모델을 적합하고 추정치의 분산이나 신뢰구간을 근사하는 방법

library(tidymodels)

ggplot(mtcars, aes(wt, mpg)) +
  geom_point()

# 비선형 최소 제곱(Nonlinear Least Squares)회귀를 수행하는 함수
# ==> 잔차 제곱합이 최소가 되도록 모수(계수)를 수치적으로 찾음
# - 잔차 제곱합을 줄이기 위해 가우스–뉴턴(Gauss–Newton)이나 관련 알고리즘을 반복적으로 적용하며, 
# - 선형 회귀의 lm()과 달리 해를 한 번에 공식으로 구하지 못해 초기값 설정이 매우 중요

nlsfit <- nls(mpg ~ k / wt + b, mtcars, start = list(k = 1, b = 0))
summary(nlsfit)

ggplot(mtcars, aes(wt, mpg)) +
  geom_point() +
  # predict : mpg 예측값(적합값) 벡터를 뽑아오는 함수
  geom_line(aes(y = predict(nlsfit)))

# x축의 최소 무게 ~ 최대 무게 사이를 같은 간격으로 100개 찍어서 만든 숫자 벡터
newdata = data.frame(wt = seq(min(mtcars$wt), max(mtcars$wt), length.out = 100))
newdata
newdata$pred <- predict(nlsfit, newdata = newdata)
newdata

ggplot(mtcars, aes(wt, mpg)) +
  geom_point() +
  geom_line(data = newdata, aes(wt, pred), color = 'blue')

set.seed(27)
boots <- bootstraps(mtcars, times = 2000, apparent = TRUE)
boots

fit_nls_bootstrap <- function(split) {
  nls(mpg ~ k/wt+b, analysis(split), start = list(k = 1, b = 0))
}

boot_models <- 
  boots |>
  mutate(
    model = map(splits, fit_nls_bootstrap),
    coef_info = map(model, tidy)
  )
boot_models

boot_coefs <- 
  boot_models |>
  unnest(coef_info)
boot_coefs

percentile_intervals <- int_pctl(boot_models, coef_info)
percentile_intervals[percentile_intervals$term == "b", ".lower"]

library(glue)

label_vec <- c(
  k = glue("Slope(k) : {round(percentile_intervals[percentile_intervals$term == 'k', '.lower'],2)} ~ {round(percentile_intervals[percentile_intervals$term == 'k', '.upper'], 2)}"),
  b = glue("Intercept(b) : {round(percentile_intervals[percentile_intervals$term == 'b', '.lower'],2)} ~ {round(percentile_intervals[percentile_intervals$term == 'b', '.upper'], 2)}")
  
)
label_vec

ggplot(boot_coefs, aes(estimate)) +
  geom_histogram(bins = 30) +
  facet_wrap(~term, scales="free", labeller = labeller(term = label_vec)) +
  geom_vline(aes(xintercept = .lower), data = percentile_intervals, color = 'red') +
  geom_vline(aes(xintercept = .estimate), data = percentile_intervals, color = 'blue') +
  geom_vline(aes(xintercept = .upper), data = percentile_intervals, color = 'red') +
  geom_text(
    data = percentile_intervals,
    aes(x =.estimate, y = Inf, label = round(.estimate, 2)),
    vjust = 2,
    color = 'blue',
    size = 3
  )

boot_aug <- 
  boot_models |>
  sample_n(200) |>
  mutate(augmented = map(model, augment)) |>
  unnest(augmented)
boot_aug

ggplot(boot_aug, aes(wt, mpg)) +
  geom_line(aes(y = .fitted, group = id), alpha = .2, color = 'blue') +
  geom_point()

fit_spline_on_bootstrap <- function(split) {
  data <- analysis(split)
  smooth.spline(data$wt, data$mpg, df = 4)
}

boots

boot_splines <- 
  boots |>
  sample_n(200) |>
  mutate(
    spline = map(splits, fit_spline_on_bootstrap),
    aug_train = map(spline, augment)
  )

boot_splines

splines_aug <- 
  boot_splines |>
  unnest(aug_train)
splines_aug

ggplot(splines_aug, aes(x, y)) +
  geom_line(aes(y = .fitted, group = id), alpha = .2, color = 'blue') +
  geom_point()




