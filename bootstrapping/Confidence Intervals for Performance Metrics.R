# [목표]
# 데이터를 학습·검증·테스트로 initial_validation_split()으로 나눈 뒤, 검증 세트 성능의 불확실성을 추정
# 검증 세트(혹은 테스트 세트)에서 나온 단일 성능 값만으로도, 
#   그 예측값들을 부트스트랩하면 “성능 지표의 신뢰구간”을 추정할 수 있다는 점을 보여줌

library(tidymodels)

str(deliveries)

deliveries

set.seed(991)

delivery_split <- initial_validation_split(deliveries, prop = c(.6, .2),
                                           # 세 세트 모두에서 time_to_delivery 분포가 비슷하게 유지되도록 층화(stratified) 분할
                                           strata = time_to_delivery) 

delivery_train <- training(delivery_split)
delivery_valid <- validation(delivery_split)
delivery_test <- testing(delivery_split)

# “train + validation” 정보를 이용해, 검증 세트를 활용하는 리샘플 객체를 만듬
delivery_rs <- validation_set(delivery_split)

# MARS (다변량 적응 회귀 스플라인)
# ==> 여러 설명변수(또는 여러 반응)를 동시에 고려해 스플라인 회귀를 한다는 의미
# - 비모수 회귀 기법으로, 
# - 여러 개의 입력 변수를 사용할 때 
# - 비선형성·구간별 다른 기울기·상호작용을 
# - 자동으로 잡아 주는 “조각 선형(piecewise linear) 회귀”
# 변량(variate) : 어떤 현상을 수량으로 나타낸 자료값 자체

mars_spec <- 
  mars(
    num_terms = tune(),    # 모델이 쓸 조각의 개수(복잡도)를 여러 값을 시험해서 가장 좋은 값을 자동 설정정
    prod_degree = 2,       # 변수끼리 두 개까지 곱한 상호작용 효과까지만 모델에 허용
    prune_method = "none"  # 자동 가지치기를 하지 말고, 만든 항들을 그대로 두라
  ) |>
  set_mode('regression')   # 타깃이 연속형(time_to_delivery 같은 숫자)이니, 이 스펙은 회귀 모델로 사용하겠다”는 선언


grid <- tibble(num_terms = 2:50)
ctrl <- control_grid(save_pred = T)

mars_res <- 
  mars_spec |>
  tune_grid(
    time_to_delivery ~ .,
    resamples = delivery_rs,   # train 부분으로 모델을 학습하고, validation 부분에서 RMSE, R²를 계산.
    grid = grid,
    control = ctrl
  )

# tune_grid : 미리 정한 여러 설정값 조합을 전부 시험해 보고, 성능이 가장 좋은 설정을 고름