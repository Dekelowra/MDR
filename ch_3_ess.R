install.packages(c("tidyverse", "here", "lle", "plot3D",
                   "amerika", "parallel", "ggrepel", 
                   "tictoc", "patchwork", "dplyr"))

install.packages('snowfall')
# Данная библиотека нужна для работы библиотеки lle.
# Библиотеку lle нужно устанавливать вручную.
# Ссылка на архив с lle (я использую версию 1.1): https://cran.r-project.org/src/contrib/Archive/lle/

# Приношу извинения за безобразные (слишком длинные) подписи к осям графиков.

library(tidyverse)
library(here)
library(lle)
library(plot3D)
library(amerika)
library(parallel)
library(ggrepel)
library(tictoc)
library(patchwork)
library(dplyr)

ess <- read_rds("https://github.com/Dekelowra/Data/blob/main/ess.rds?raw=true")

set.seed(1234)

skimr::skim(ess)

ess_sample = sample_n(ess, 5000) 
# Пришлось взять лишь небольшую часть выборки, так как R не смог выполнить алгоритм при использовании всего массива данных.

ess_sample_scaled <- ess_sample[, 1:32] %>% 
  scale() %>% 
  as_tibble()

{
  par(mfrow = c(2,2))
  scatter3D(ess_sample_scaled$polintr, 
            ess_sample_scaled$actrolga, 
            ess_sample_scaled$cptppola,
            bty = "f",
            pch = 1,
            phi = 7,
            theta = 25,
            colkey = FALSE,
            col = ramp.col(c(amerika_palettes$Republican[1], 
                             amerika_palettes$Democrat[1])),
            main = "Interest in Politics",
            xlab = "Interested in politics",
            ylab = "Ability to take active role in political group",
            zlab = "Ability to participate in politics"
  )
  
  scatter3D(ess_sample_scaled$trstprl, 
            ess_sample_scaled$trstplc, 
            ess_sample_scaled$trstplt,
            bty = "f",
            pch = 1,
            phi = 7,
            theta = 25,
            colkey = FALSE,
            col = ramp.col(c(amerika_palettes$Republican[1], 
                             amerika_palettes$Democrat[1])),
            main = "Trust in Political Institutions",
            xlab = "Trust in country's parliament",
            ylab = "Trust in the police",
            zlab = "Trust in politicians"
  )
  scatter3D(ess_sample_scaled$freehms, 
            ess_sample_scaled$hmsfmlsh, 
            ess_sample_scaled$hmsacld,
            bty = "f",
            pch = 1,
            phi = 7,
            theta = 25,
            colkey = FALSE,
            col = ramp.col(c(amerika_palettes$Republican[1], 
                             amerika_palettes$Democrat[1])),
            main = "Attitude towards sexual minorities",
            xlab = "Freedom for these people",
            ylab = "Ashamed if family member is such a person",
            zlab = "These people have a right to adopt children"
  )
  scatter3D(ess_sample_scaled$imbgeco, 
            ess_sample_scaled$imueclt, 
            ess_sample_scaled$imwbcnt,
            bty = "f",
            pch = 1,
            phi = 7,
            theta = 25,
            colkey = FALSE,
            col = ramp.col(c(amerika_palettes$Republican[1], 
                             amerika_palettes$Democrat[1])),
            main = "Immigration",
            xlab = "Influence on economy",
            ylab = "Influence on culture",
            zlab = "Influence on country"
  )
  par(mfrow = c(1,1))
  }

cores <- detectCores() - 1

tic() 
find_k <- calc_k(ess_sample_scaled,
                 m = 2, 
                 parallel = TRUE,
                 cpus = cores) 
toc()

# Процесс занял около 16 минут на 11 ядрах

find_k %>% 
  arrange(rho)

find_k[which.min(find_k$rho), ]

# Оптимальное значение k = 14.

optimal_k_rho <- find_k %>% 
  arrange(rho) %>% 
  filter(rho == min(.))

find_k %>% 
  arrange(rho) %>% 
  ggplot(aes(k, rho)) +
  geom_line() +
  geom_point(color = ifelse(find_k$k == min(find_k$k), 
                            "red", 
                            "black")) +
  geom_vline(xintercept = optimal_k_rho$k, 
             linetype = "dashed", 
             color = "red") +
  geom_label_repel(aes(label = k),
                   box.padding = unit(0.5, 'lines')) +
  labs(x = "Neighborhood Size (k)",
       y = expression(rho)) +
  theme_minimal()

{
  tic() 
  lle_fit <- lle(ess_sample_scaled,
                 m = 2,
                 nnk = TRUE,
                 k = 14)
  toc() 
  } # Процесс занял около 3 минут на 11 ядрах

lle_viz <- ess_sample %>% 
  ggplot(aes(x = lle_fit$Y[,1], # scores for d1
             y = lle_fit$Y[,2], # scores for d2
             col = factor(voted))) +
  geom_point() +
  stat_ellipse() +
  scale_color_manual(values=c(amerika_palettes$Republican[1], 
                              amerika_palettes$Democrat[1]),
                     name="Voted",
                     breaks=c("0", "1"),
                     labels=c("Didn't vote", 
                              "Voted")) +
  labs(x = "First Dimension",
       y = "Second Dimension",
       title = "LLE") + 
  theme_minimal()+
  theme(axis.title = element_text(size=15),
        axis.text = element_text(size=17),
        legend.text = element_text(size=13),
        legend.title = element_text(size=15))
lle_viz

pca_fit <- ess_sample[, 1:32] %>% 
  scale() %>% 
  prcomp()

pca_viz <- ess_sample %>% 
  ggplot(aes(pca_fit$x[, 1], 
             pca_fit$x[, 2], 
             col = factor(voted))) +
  geom_point() +
  stat_ellipse() +
  scale_color_manual(values=c(amerika_palettes$Republican[1], 
                              amerika_palettes$Democrat[1]),
                     name="Voted",
                     breaks=c("0", "1"),
                     labels=c("Didn't vote", 
                              "Voted")) +
  labs(x = "Principal Component 1",
       y = "Principal Component 2",
       title = "PCA") +
  theme_minimal()+
  theme(axis.title = element_text(size=15),
        axis.text = element_text(size=17),
        legend.text = element_text(size=13),
        legend.title = element_text(size=15))

library(patchwork)

lle_viz + pca_viz

# На обоих графиках видно, что респонденты, которые проголосовали на последних выборах и которые не проголосовали,
# не образуют четких групп. 

p1 <- ess_sample %>% 
  ggplot(aes(polintr, cptppola, 
             color = factor(voted))) +
  geom_density_2d() + 
  scale_color_manual(values=c(amerika_palettes$Republican[1], 
                              amerika_palettes$Democrat[1]),
                     name="Voted",
                     breaks=c("0", "1"),
                     labels=c("Didn't vote", 
                              "Voted")) +
  labs(x = "Interest in politics",
       y = "Confident in own ability to participate in politics") +
  theme_minimal()+
  theme(axis.title = element_text(size=12),
        axis.text = element_text(size=12),
        legend.text = element_text(size=13),
        legend.title = element_text(size=15))

p2 <- ess_sample %>% 
  ggplot(aes(trstep, trstun, 
             color = factor(voted))) +
  geom_density_2d() + 
  scale_color_manual(values=c(amerika_palettes$Republican[1], 
                              amerika_palettes$Democrat[1]),
                     name="Voted",
                     breaks=c("0", "1"),
                     labels=c("Didn't vote", 
                              "Voted")) +
  labs(x = "Trust in the European Parliament",
       y = "Trust in the United Nations") +
  theme_minimal()+
  theme(axis.title = element_text(size=12),
        axis.text = element_text(size=12),
        legend.text = element_text(size=13),
        legend.title = element_text(size=15))

p3 <- ess_sample %>% 
  ggplot(aes(stfdem, gincdif, 
             color = factor(voted))) +
  geom_density_2d() + 
  scale_color_manual(values=c(amerika_palettes$Republican[1], 
                              amerika_palettes$Democrat[1]),
                     name="Voted",
                     breaks=c("0", "1"),
                     labels=c("Didn't vote", 
                              "Voted")) +
  labs(x = "How satisfied with the way democracy works in country",
       y = "Government should reduce differences in income levels") +
  theme_minimal()+
  theme(axis.title = element_text(size=12),
        axis.text = element_text(size=12),
        legend.text = element_text(size=13),
        legend.title = element_text(size=15))

p4 <- ess_sample %>% 
  ggplot(aes(imwbcnt, imueclt, 
             color = factor(voted))) +
  geom_density_2d() + 
  scale_color_manual(values=c(amerika_palettes$Republican[1], 
                              amerika_palettes$Democrat[1]),
                     name="Voted",
                     breaks=c("0", "1"),
                     labels=c("Didn't vote", 
                              "Voted")) +
  labs(x = "Immigrants make country worse or better place to live",
       y = "Country's cultural life undermined or enriched by immigrants") +
  theme_minimal()+
  theme(axis.title = element_text(size=12),
        axis.text = element_text(size=12),
        legend.text = element_text(size=13),
        legend.title = element_text(size=15))

# На графках видно, что и в изначальном массиве данных нет четкого разделения на тех, кто проголосовал и кто не проголосовал.

(p1 + p2) /
  (p3 + p4)
