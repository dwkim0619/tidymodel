# 관측된 데이터에서 나타난 효과/차이가 진짜 실질적인 것인지 아니면 단순한 우연인지?

# <<통계적 추론 기본 개념>>
# 1.귀무가설(null hypothesis)을 세움 : "아무런 효과나 차이가 없다." 
# 2.관측 통계량을 계산 : 평균, 비율, 차이 등을 계산
# 3.귀무가설 하에서 기대되는 분포를 구성 : resampling/permutation/bootstrap 등을 통해 만든 분포
# 4.p-value 계산 : 귀무가설 하에서 관측 통계량보다 더 극단적인 값이 나올 확률
# 5.p-value가 충분히 작으면 귀무가설 기각

# <<infer 4가지 핵심 함수>>
# 1.specify() - 분석할 변수/관계를 지정, 관심 있는 결과 변수나 설명 변수를 선택
# 2.hypothesize() - 귀무가설 선언
# 3.generate() - 귀무분포 데이터 생성
# 4.calulate() - 통계량 계산

# Is the effect or difference in our observed data 
#  real, or due to chance?

# 우리의 관측된 데이터에서 나타난 효과나 차이가 실제로 존재하는 것인지, 아니면 우연에 의한 것인지 알고 싶은 것!
# ==> 
# 1.“아무 일도 일어나지 않는” 어떤 세계에서 나온 것이라고 가정. 즉, 관측된 효과가 단지 랜덤한 우연 때문에 나타났다고 보는 것 --> 귀무가설
# 2.그다음, 데이터로부터 관측된 효과를 요약하는 검정 통계량을 계산
# 3.이 검정 통계량을 사용해 p값을 계산하는데, p값은 귀무가설이 참이라고 가정했을 때 지금과 같은 데이터를 얻을 확률을 의미
# 4.이 확률이 미리 정해 둔 유의수준 𝛼 보다 작다면, 귀무가설을 기각

library(tidymodels)

set.seed(1234)

data(gss)
glimpse(gss)

gss |>
  specify(response = age)

gss |>
  specify(age ~ partyid)

gss |>
  # 범주형 변수 college의 여러 수준 중에서 "degree"를 성공(success) 으로 간주하겠다는 의미
  # 이후 비율 추정·검정에서 “대학 학위가 있는 사람의 비율”을 대상으로 하겠다는 설정
  specify(response = college, success = "degree")


# 정당 성향에 따라 대학 학위 보유 여부 비율이 달라지는지(독립성 검정)를 위한 설정
gss |>
  specify(college ~ partyid, success = "degree") |>
  # 귀무가설(null) 을 “college와 partyid는 서로 독립이다”, 즉 정당 성향과 대학 학위 보유 여부 사이에 관계가 없다고 선언
  hypothesise(null = "independence")

# "주당 평균 근로 시간이 40시간이다”라는 점(점 추정량)에 대한 가설을 설정
gss |>
  specify(response = hours) |>
  hypothesise(null = "point", mu = 40) |>
  generate(reps = 5000, type = "bootstrap")
# type
# 1.bootstrap: 각 반복(replicate)마다, 원래 표본 크기와 같은 크기의 표본을 복원추출
# 2.permute: 각 반복마다, 표본 안의 값들을 비복원추출로 무작위 재배치하여 기존의 짝짓기(관계)를 깨뜨린 새로운 표본을 만듬
# 3.simulate: 각 반복마다, hypothesize()에서 지정한 모수값을 가진 이론 분포로부터 난수를 생성하여 값을 뽑음(현재 점 추정(point estimate) 을 검정할 때에만 사용)


# 나이와 정당 성향이 서로 독립이라는 가설 하에서, 두 변수의 관계가 없다고 가정하고 5000번 퍼뮤테이션 표본을 생성해 귀무분포를 만들 준비
gss |>
  specify(partyid ~ age) |>
  hypothesise(null = "independence") |>
  generate(reps = 5000, type = "permute")


point_estimate <- gss |>
  specify(response = hours) |>
  calculate(stat = "mean")

point_estimate

set.seed(693)

# "주당 평균 근로 시간이 40시간이다”라는 점(점 추정량)에 대한 가설을 설정
null_dist <- gss |>
  specify(response = hours) |>
  hypothesise(null = "point", mu = 40) |>
  generate(reps = 5000, type = "bootstrap") |>
  calculate(stat = "mean")

null_dist |>
  visualise() +
  # 귀무분포 그래프에서 p값에 해당하는 영역을 색으로 칠해 주는 함수
  shade_p_value(obs_stat = point_estimate, direction = "two_sided")

p_value <- null_dist |>
  get_p_value(obs_stat = point_estimate, direction = "two_sided")

# 주당 평균 근로 시간이 실제로 40시간이라고 가정했을 때, 
# 지금 표본 평균이 40에서 이렇게까지 멀리(1.382시간 차이) 떨어져 관측될 확률이 0.038이다” 라는 뜻
# point_estimate : 41.4
# p_value : 0.038

null_dist |>
  get_confidence_interval(
    point_estimate = point_estimate,
    level = .95,
    type = "se"
  )

set.seed(533)

null_f_distn <- gss %>%
   specify(age ~ partyid) %>%
   hypothesize(null = "independence") %>%
   generate(reps = 5000, type = "permute") %>%
   calculate(stat = "F")


null_f_distn_theoretical <- gss %>%
   specify(age ~ partyid) %>%
   hypothesize(null = "independence") %>%
   calculate(stat = "F")

F_hat <- gss |>
   specify(age ~ partyid) |>
   calculate(stat = "F")

null_f_distn_theoretical |>
  visualise(method = "theoretical") +
  shade_p_value(obs_stat = F_hat, direction = "greater")

null_f_distn |>
  # method 인자는 visualize()가 어떤 방식의 귀무분포를 그래프에 그릴지를 정하는 옵션
  # 1.null_f_distn 안에 들어 있는 랜덤화 기반 F 분포(히스토그램/밀도)
  # 2.같은 자유도를 가진 이론적 F 분포 곡선
  visualise(method = "both") +
  shade_p_value(obs_stat = F_hat, direction = "greater")
