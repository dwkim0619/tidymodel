library(tidymodels)
set.seed(1234)

data(ad_data, package = "modeldata")
str(ad_data)

ad_data |>
  select(Genotype, Class) # 유전자형 Genotype 와 인지상태 Class

# 연구 질문 =>
# 유전자형과 인지 기능(정상 vs 장애) 사이에 통계적으로 유의한 연관이 있는지, 
# 그리고 관측된 유전자형 분포가 메타분석(문헌)에서 보고된 분포와 일치하는지?

# 카이제곱 독립성 검정의 카이제곱 통계량
observed_indep_statistic <- ad_data |>
  specify(Genotype ~ Class) |>
  calculate(stat = "Chisq")

# “Genotype와 Class가 독립이다”라는 귀무가설 아래에서, 
# 카이제곱 통계량의 귀무분포(시뮬레이션 분포)를 만듬
null_distribution_simulated <- ad_data |>
  specify(Genotype ~ Class) |>
  # 귀무가설을 “Genotype와 Class는 독립이다”로 설정
  # 즉, 유전자형 분포가 Class(정상/장애)에 의해 달라지지 않는다고 가정
  hypothesise(null = "independence") |>
  generate(reps = 5000, type = "permute") |>
  calculate(stat = "Chisq")

null_distribution_simulated |>
  visualise() +
  shade_p_value(observed_indep_statistic, direction = "greater")

ad_data |>
  specify(Genotype ~ Class) |>
  # 귀무가설을 “Genotype와 Class는 독립이다”로 설정
  # 즉, 유전자형 분포가 Class(정상/장애)에 의해 달라지지 않는다고 가정
  hypothesise(null = "independence") |>
  visualise(method = "theoretical") +
  shade_p_value(observed_indep_statistic, direction = "greater")

null_distribution_simulated |>
  visualise(method = "both") +
  shade_p_value(observed_indep_statistic, direction = "greater")

null_distribution_simulaed |>
  get_p_value(observed_indep_statistic, "greater")

chisq_test(ad_data, Genotype ~ Class)


meta_rates <- c("E2E2" = 0.71, "E2E3" = 11.4, "E2E4" = 2.32,
                "E3E3" = 61.0, "E3E4" = 22.6, "E4E4" = 2.22)
meta_rates <- meta_rates/sum(meta_rates) # these add up to slightly > 100%
meta_rates

obsr_rates <- table(ad_data$Genotype) / nrow(ad_data)
obsr_rates

round(cbind(obsr_rates, meta_rates) * 100, 2)

# calculating the null distribution
observed_gof_statistic <- ad_data |>
  specify(response = Genotype) |>
  hypothesize(null = "point", p = meta_rates) |>
  calculate(stat = "Chisq")

# generating a null distribution
null_distribution_gof <- ad_data |>
  specify(response = Genotype) |>
  hypothesise(null = "point", p = meta_rates) |>
  generate(reps = 5000, type = "draw") |>
  calculate(stat = "Chisq")

null_distribution_gof |>
  visualise() +
  shade_p_value(observed_gof_statistic, direction = "greater")

p_value_gof <- null_distribution_gof |>
  get_p_value(observed_gof_statistic, direction = "greater")
p_value_gof

chisq_test(ad_data, response = Genotype, p = meta_rates)
