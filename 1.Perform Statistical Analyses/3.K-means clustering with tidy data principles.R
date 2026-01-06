# K-means : K개의 평균(centroid)을 사용하는 군집화 알고리즘
#   - 평균 = centroid : 군집의 중심점(중심질량)
#                      각 클러스터에 속한 데이터들의 좌표 평균을 계산해 그 군집을 대표하는 하나의 점으로 사용 => centroid
#   - K : 클러스터(군집) 개수
#   - 각 점이 속한 군집 중심과의 거리 제곱합이 최소화가 되도록,
#     이 평균들을 반복해서 다시 계산하고 군집 배정을 갱신

library(tidymodels)

set.seed(27)

centers <- tibble(
  cluster = factor(1:3),
  num_points = c(100, 150, 50),
  x1 = c(5, 0, -3),
  x2 = c(-1, 1, -2)
)
centers

labelled_point <- 
  centers |>
  mutate(
    x1 = map2(num_points, x1, rnorm),
    x2 = map2(num_points, x2, rnorm)
  ) |>
  select(-num_points) |>
  unnest(cols = c(x1, x2))

labelled_point |>
  ggplot(aes(x1, x2, color = cluster)) +
  geom_point(alpha = .7)


points <- 
  labelled_point |>
  select(-cluster)

points
kclust <- kmeans(points, centers = 3)
kclust

summary(kclust)

augment(kclust, points)

tidy(kclust)

kclusts <- 
  tibble(k = 1:9) |>
  mutate(
    kclust = map(k, ~kmeans(points, .x)),
    tidied = map(kclust, tidy),
    glanced = map(kclust, glance),
    augmented = map(kclust, augment, points)
  )
kclusts

clusters <- 
  kclusts |>
    unnest(cols = c(tidied))
clusters

assignments <- 
  kclusts |>
    unnest(cols = c(augmented))
assignments

clusterings <- 
  kclusts |>
    unnest(cols = c(glanced))
clusterings

p1 <- 
  ggplot(assignments, aes(x1, x2)) +
  geom_point(aes(color = .cluster), alpha = .8) +
  facet_wrap(~ k)
p1

clusters

p2 <- p1 + geom_point(data = clusters, size = 10, shape = "x")
p2

clusterings |>
  ggplot(aes(k, tot.withinss)) +
  geom_line() +
  geom_point()
