install.packages(c("tidyverse", "here", "amerika", "tictoc", "kohonen", "doParallel", "patchwork", "ppclust"))

library(tidyverse)
library(here)
library(amerika)
library(tictoc)
library(kohonen)
library(patchwork)
library(ppclust)

ess <- read_rds("https://github.com/Dekelowra/Data/blob/main/ess.rds?raw=true")

set.seed(1234)

ess_sample = sample_n(ess, 5000) # Было принято решение взять лишь часть выборки для того, чтобы размер используемого массива данных
                                 # хотя бы примерно совпадал с размером массива данных, используемого автором книги; это обусловлено тем, что
                                 # при использовании массива данных большего размера размер "решетки" (lattice) 10x10 может быть неподходящим.

ess_scaled <- ess_sample[ ,1:32] %>% 
  scale()

search_grid <- somgrid(xdim = 10, 
                       ydim = 10, 
                       topo = "rectangular",
                       neighbourhood.fct = "gaussian")

{
  tic()
  som_fit <- som(ess_scaled,
                 grid = search_grid,
                 alpha = c(0.1, 0.001),
                 radius = 1,
                 rlen = 500, 
                 dist.fcts = "euclidean", 
                 mode = "batch") 
  toc()
} # Процесс занял около 20 секунд

som_fit$changes %>% 
  as_tibble() %>% 
  rename(., changes = V1) %>% 
  mutate(., iteration = seq(1:length(changes))) %>% 
  ggplot(aes(iteration, changes)) +
  geom_line() +
  labs(x = "Training Iteration",
       y = "Mean Distance to Closest Node") +
  theme_minimal()+
  theme(axis.title = element_text(size=15),
        axis.text = element_text(size=17),
        legend.text = element_text(size=13),
        legend.title = element_text(size=15))

# Как можно видеть, алгоритм стабилизировался где-то на 350 итерациях. 

point_colors <- c(amerika_palettes$Republican[2], 
                  amerika_palettes$Democrat[2])

neuron_colors <- c(amerika_palettes$Republican[3], 
                   amerika_palettes$Democrat[3])

kmeans_clusters <- som_fit$codes[[1]] %>% 
  kmeans(., centers = 2)

class_assign_km <- map_dbl(kmeans_clusters$cluster, ~{
  if(. == 1) 2
  else 1
}
)

plot(som_fit, 
     type = "mapping", 
     pch = 21, 
     bg = point_colors[as.factor(ess_sample$voted)],
     shape = "straight",
     bgcol = neuron_colors[as.integer(class_assign_km)],
     main = " "); add.cluster.boundaries(x = som_fit, clustering = class_assign_km, 
                                         lwd = 5, lty = 5)

fcm_clusters <- som_fit$codes[[1]] %>% 
  ppclust::fcm(., centers = 2)

class_assign_fcm <- map_dbl(fcm_clusters$cluster, ~{
  if(. == 1) 2
  else 1
}
)

plot(som_fit, 
     type = "mapping", 
     pch = 21, 
     bg = point_colors[as.factor(ess_sample$voted)],
     shape = "straight",
     bgcol = neuron_colors[as.integer(class_assign_fcm)],
     main = " "); add.cluster.boundaries(x = som_fit, clustering = class_assign_fcm, 
                                         lwd = 5, lty = 5)

plot(som_fit, 
     type = "codes")

som_fit$codes %>% 
  as.data.frame() %>% 
  ggplot(aes(trstplt, trstprt)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = "Trust in politicians", 
       y = "Trust in political parties") +
  theme_minimal()+
  theme(axis.title = element_text(size=15),
        axis.text = element_text(size=17),
        legend.text = element_text(size=13),
        legend.title = element_text(size=15))

som_fit$codes %>% 
  as.data.frame() %>% 
  ggplot(aes(trstep, trstun)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = "Trust in the European Parliament", 
       y = "Trust in the United Nations") +
  theme_minimal()+
  theme(axis.title = element_text(size=15),
        axis.text = element_text(size=17),
        legend.text = element_text(size=13),
        legend.title = element_text(size=15))

som_fit$codes %>% 
  as.data.frame() %>% 
  ggplot(aes(freehms, hmsfmlsh)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = "Gays and lesbians are free to live as they wish", 
       y = "Ashamed if close family member is gay or lesbian") +
  theme_minimal()+
  theme(axis.title = element_text(size=15),
        axis.text = element_text(size=17),
        legend.text = element_text(size=13),
        legend.title = element_text(size=15))

som_fit$codes %>% 
  as.data.frame() %>% 
  ggplot(aes(euftf, impcntr)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = "European integration should go further/ has gone too far", 
       y = "Allowing many/few immigratnts from poorer countries outside Europe") +
  theme_minimal()+
  theme(axis.title = element_text(size=15),
        axis.text = element_text(size=17),
        legend.text = element_text(size=13),
        legend.title = element_text(size=15))

# Autoencoders

library(h2o)

# Пакет h2o нужно устанавливать вручную
# Ссылка для скачивания: https://h2o-release.s3.amazonaws.com/h2o/rel-3.46.0/7/index.html

library(doParallel)

set.seed(1234)

ess_sample$voted <- factor(ess_sample$voted)

my_h2o <- h2o.init()

cores <- detectCores() - 1 
cluster <- makeCluster(cores, setup_timeout = 0.5)
registerDoParallel(cluster) 

ess_h2o <- ess_sample %>% 
  as.h2o()

options(timeout = 500) # Данную строку пришлось добавить из-за того, что при разделении массива данных с помощью команды ниже 
                       # возникала ошибка, сообщающая о превышении лимита времени.

split_frame <- h2o.splitFrame(ess_h2o, 
                              ratios = c(0.6, 0.2), 
                              seed = 1234)

split_frame %>% 
  glimpse()

train <- split_frame[[1]]
validation <- split_frame[[2]]
test <- split_frame[[3]]

response <- "voted"

predictors <- setdiff(colnames(train), response)

{
  tic()
  autoencoder <- h2o.deeplearning(x = predictors, 
                                  training_frame = train,
                                  autoencoder = TRUE,
                                  reproducible = TRUE,
                                  seed = 1234,
                                  hidden = c(16),
                                  epochs = 100,
                                  activation = "Tanh",
                                  validation_frame = test)
  toc()
} # Процесс занял около 6 секунд

codings_train <- h2o.deepfeatures(autoencoder, 
                                  data = train, 
                                  layer = 1) %>% 
  as.data.frame() %>%
  mutate(voted = as.vector(train[ , 33]))

{
  p1 <- ggplot(codings_train, aes(x = DF.L1.C1, 
                                  y = DF.L1.C2, 
                                  color = factor(voted))) +
    geom_point(alpha = 0.6) + 
    stat_ellipse() +
    scale_color_manual(values=c(amerika_palettes$Republican[1], 
                                amerika_palettes$Democrat[1]),
                       name="Voted",
                       breaks=c("0", "1"),
                       labels=c("Didn't vote", "Voted")) +
    labs(title = "Deep Features 1 & 2",
         color = "Democrat") + 
    theme_minimal()
  
  # (3 and 4)
  p2 <- ggplot(codings_train, aes(x = DF.L1.C3, 
                                  y = DF.L1.C4, 
                                  color = factor(voted))) +
    geom_point(alpha = 0.6) + 
    stat_ellipse() +
    scale_color_manual(values=c(amerika_palettes$Republican[1], 
                                amerika_palettes$Democrat[1]),
                       name="Voted",
                       breaks=c("0", "1"),
                       labels=c("Didn't vote", "Voted")) +
    labs(title = "Deep Features 3 & 4",
         color = "Democrat") + 
    theme_minimal()
  
  # 5 & 6
  p3 <- ggplot(codings_train, aes(x = DF.L1.C5, 
                                  y = DF.L1.C6, 
                                  color = factor(voted))) +
    geom_point(alpha = 0.6) + 
    stat_ellipse() +
    scale_color_manual(values=c(amerika_palettes$Republican[1], 
                                amerika_palettes$Democrat[1]),
                       name="Voted",
                       breaks=c("0", "1"),
                       labels=c("Didn't vote", "Voted")) +
    labs(title = "Deep Features 5 & 6",
         color = "Democrat") + 
    theme_minimal()
  
  # 7 & 8
  p4 <- ggplot(codings_train, aes(x = DF.L1.C7, 
                                  y = DF.L1.C8, 
                                  color = factor(voted))) +
    geom_point(alpha = 0.6) + 
    stat_ellipse() +
    scale_color_manual(values=c(amerika_palettes$Republican[1], 
                                amerika_palettes$Democrat[1]),
                       name="Voted",
                       breaks=c("0", "1"),
                       labels=c("Didn't vote", "Voted")) +
    labs(title = "Deep Features 7 & 8",
         color = "Democrat") + 
    theme_minimal()
  
  # 9 & 10
  p5 <- ggplot(codings_train, aes(x = DF.L1.C9, 
                                  y = DF.L1.C10, 
                                  color = factor(voted))) +
    geom_point(alpha = 0.6) + 
    stat_ellipse() +
    scale_color_manual(values=c(amerika_palettes$Republican[1], 
                                amerika_palettes$Democrat[1]),
                       name="Voted",
                       breaks=c("0", "1"),
                       labels=c("Didn't vote", "Voted")) +
    labs(title = "Deep Features 9 & 10",
         color = "Democrat") + 
    theme_minimal()
  
  # 11 & 12
  p6 <- ggplot(codings_train, aes(x = DF.L1.C11, 
                                  y = DF.L1.C12, 
                                  color = factor(voted))) +
    geom_point(alpha = 0.6) + 
    stat_ellipse() +
    scale_color_manual(values=c(amerika_palettes$Republican[1], 
                                amerika_palettes$Democrat[1]),
                       name="Voted",
                       breaks=c("0", "1"),
                       labels=c("Didn't vote", "Voted")) +
    labs(title = "Deep Features 11 & 12",
         color = "Democrat") + 
    theme_minimal()
  
  # 13 & 14
  p7 <- ggplot(codings_train, aes(x = DF.L1.C13, 
                                  y = DF.L1.C14, 
                                  color = factor(voted))) +
    geom_point(alpha = 0.6) + 
    stat_ellipse() +
    scale_color_manual(values=c(amerika_palettes$Republican[1], 
                                amerika_palettes$Democrat[1]),
                       name="Voted",
                       breaks=c("0", "1"),
                       labels=c("Didn't vote", "Voted")) +
    labs(title = "Deep Features 13 & 14",
         color = "Democrat") + 
    theme_minimal()
  
  # 15 & 16
  p8 <- ggplot(codings_train, aes(x = DF.L1.C15, 
                                  y = DF.L1.C16, 
                                  color = factor(voted))) +
    geom_point(alpha = 0.6) + 
    stat_ellipse() +
    scale_color_manual(values=c(amerika_palettes$Republican[1], 
                                amerika_palettes$Democrat[1]),
                       name="Voted",
                       breaks=c("0", "1"),
                       labels=c("Didn't vote", "Voted")) +
    labs(title = "Deep Features 15 & 16",
         color = "Democrat") + 
    theme_minimal()
  
  # view together
  library(patchwork)
  
  (p1 + p2 + p3 + p4) / 
    (p5 + p6 + p7 + p8)
  }
  
codings_val <- h2o.deepfeatures(object = autoencoder, 
                                data = validation, 
                                layer = 1) %>%
  as.data.frame() %>%
  mutate(voted = as.factor(as.vector(validation[ , 33]))) %>%
  as.h2o()

deep_features <- setdiff(colnames(codings_val), response)

deep_net <- h2o.deeplearning(y = response,
                             x = deep_features,
                             training_frame = codings_val,
                             reproducible = TRUE, 
                             ignore_const_cols = FALSE,
                             seed = 1234,
                             hidden = c(8, 8), 
                             epochs = 100,
                             activation = "Tanh")

test_3 <- h2o.deepfeatures(object = autoencoder, 
                           data = test, 
                           layer = 1)

test_pred <- h2o.predict(deep_net, test_3, type = "response") %>%
  as.data.frame() %>%
  mutate(truth = as.vector(test[, 33]))

print(h2o.predict(deep_net, test_3) %>%
        as.data.frame() %>%
        mutate(truth = as.vector(test[, 33])) %>%
        group_by(truth, predict) %>%
        summarise(n = n()) %>%
        mutate(freq = n / sum(n)))

table(h2o.predict(deep_net, test_3)) # Данная строка не работает

## Feature importance
fimp <- as.data.frame(h2o.varimp(deep_net)) %>% 
  arrange(desc(relative_importance))

# viz relative
fimp %>% 
  ggplot(aes(x = relative_importance, 
             y = reorder(variable, -relative_importance))) +
  geom_point(color = "dark red", 
             fill = "dark red", 
             alpha = 0.5,
             size = 5) +
  labs(x = "Relative Importance",
       y = "Feature") + 
  theme_minimal()+
  theme(axis.title = element_text(size=15),
        axis.text = element_text(size=17),
        legend.text = element_text(size=13),
        legend.title = element_text(size=15))

fimp %>% 
  ggplot(aes(x = percentage, 
             y = reorder(variable, -percentage))) +
  geom_point(color = "dark red", 
             fill = "dark red", 
             alpha = 0.5,
             size = 5) +
  labs(x = "Percentage",
       y = "Feature") +
  theme_minimal()+
  theme(axis.title = element_text(size=15),
        axis.text = element_text(size=17),
        legend.text = element_text(size=13),
        legend.title = element_text(size=15))

codings_val2 <- h2o.deepfeatures(object = autoencoder, 
                                 data = validation, 
                                 layer = 1) %>%
  as.data.frame() %>%
  mutate(voted = as.factor(as.vector(validation[ , 33]))) 

tr <- ggplot(codings_train, aes(x = DF.L1.C1, 
                                y = DF.L1.C9, 
                                color = factor(voted))) +
  geom_point(alpha = 0.6) + 
  stat_ellipse() +
  scale_color_manual(values=c(amerika_palettes$Republican[1], 
                              amerika_palettes$Democrat[1]),
                     name="Voted",
                     breaks=c("0", "1"),
                     labels=c("Didn't vote", "Voted")) +
  labs(title = "Training Set",
       color = "Democrat") + 
  theme_minimal()+
  theme(axis.title = element_text(size=15),
        axis.text = element_text(size=17),
        legend.text = element_text(size=13),
        legend.title = element_text(size=15))

val <- ggplot(codings_val2, aes(x = DF.L1.C1, 
                                y = DF.L1.C9, 
                                color = factor(voted))) +
  geom_point(alpha = 0.6) + 
  stat_ellipse() +
  scale_color_manual(values=c(amerika_palettes$Republican[1], 
                              amerika_palettes$Democrat[1]),
                     name="Voted",
                     breaks=c("0", "1"),
                     labels=c("Didn't vote", "Voted")) +
  labs(title = "Validation Set",
       color = "Democrat") + 
  theme_minimal()+
  theme(axis.title = element_text(size=15),
        axis.text = element_text(size=17),
        legend.text = element_text(size=13),
        legend.title = element_text(size=15))

(tr + val)

h2o.shutdown()
stopCluster(cluster)
