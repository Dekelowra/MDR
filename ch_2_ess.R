install.packages(c("tidyverse", "here", "corrr", "amerika",
                   "factoextra", "patchwork", "ggrepel"))

library(tidyverse)
library(here)
library(corrr)
library(amerika)
library(factoextra)
library(patchwork)
library(ggrepel)

# Для отработки кода из Главы 2 было принято решение сменить массив данных, так как обработанные данные Manifesto Project
# оказались не очень удачными для работы с методом главных компонент из-за их нескоррелированности, которая, скорее всего,
# обусловлена "низким качеством" исходных данных.
# Я работаю с массивом данных European Social Survey, а именно с результатами опросов по политической тематике.
# Ссылка на исходный массив данных: https://github.com/Dekelowra/MDR/blob/main/ESS11-subset.sav
# Ссылка на кодировочную книгу (codebook): https://github.com/Dekelowra/MDR/blob/main/ESS11-subset%20codebook.pdf
# Ссылка на код с предварительной обработкой данных (preprocessing): https://github.com/Dekelowra/MDR/blob/main/ch_1_ess.R

# Также я все-таки буду использовать синтаксис с %>%, так как, на мой взгляд, он является более понятным и удобным.

ess <- read_rds("https://github.com/Dekelowra/Data/blob/main/ess.rds?raw=true")

ess %>%
  select(-voted) %>% 
  correlate(use = "pairwise.complete.obs",
            method = "pearson",
            quiet = TRUE) %>% 
  focus(polintr) %>%
  mutate(term = reorder(term, polintr)) %>%
  ggplot(aes(term, polintr)) +
  geom_col() + 
  coord_flip() + 
  labs(y = "Interested in Politics", 
       x = "All Other Questions") +
  theme_minimal() +
  theme(axis.title = element_text(size=15),
        axis.text = element_text(size=10))

# Как можно видеть, интерес к политике не очень сильно скоррелирован с остальными переменными.

ess %>%
  select(-voted) %>% 
  correlate(use = "pairwise.complete.obs",
            method = "pearson",
            quiet = TRUE) %>% 
  network_plot(colors = c(amerika_palettes$Democrat[1], 
                          amerika_palettes$Republican[1]),
               curved = FALSE) 

# На графике "сети корреляций" можно выделить три группы. В первой группе (слева снизу) собраны вопросы
# об эффективности политической системы и возможности влияния на политику. Во второй группе (слева сверху) собраны вопросы
# о доверии различным органам власти и степени удовлетворенности различными политическими институтами.
# В третьей группе (справа сверху) собраны вопросы об отношении к меньшинствам, в частности, к людям с нетрадиционной
# ориентацией и мигрантам.

pca_fit <- ess[,-33] %>%
  scale() %>% 
  prcomp(); summary(pca_fit)

variance <- tibble(
  var = pca_fit$sdev^2,
  var_exp = var / sum(var),
  cum_var_exp = cumsum(var_exp)
) %>%
  mutate(pc = row_number())

pve <- ggplot(variance, aes(pc, var_exp)) +
  geom_point() +
  geom_line() +
  geom_label_repel(aes(label = pc), size = 4) +
  labs(x = "Principal Component",
       y = "PVE") +
  theme_minimal() +
  theme(axis.title = element_text(size=15),
        axis.text = element_text(size=17))

cpve <- ggplot(variance, aes(pc, cum_var_exp)) +
  geom_point() +
  geom_line() +
  geom_label_repel(aes(label = pc), size = 4) +
  labs(x = "Principal Component",
       y = "Cumulative PVE") + 
  theme_minimal() +
  theme(axis.title = element_text(size=15),
        axis.text = element_text(size=17))

pve + cpve

scree1 <- fviz_screeplot(pca_fit, main = "", addlabels = TRUE, choice = "variance")+
  theme(text = element_text(size=15),
        axis.text = element_text(size=17))
scree2 <- fviz_screeplot(pca_fit, main = "", addlabels = TRUE, choice = "eigenvalue")+
  theme(axis.title = element_text(size=15),
        axis.text = element_text(size=17))

scree1 + scree2

# Как можно видеть на графиках, первые две главные компоненты объясняют довольно небольшой объем дисперсии (около 40%),
# поэтому работать лишь с ними было бы, разумеется, нецелесообразно.
# В качестве условной независимой переменной-аналога democrat из книги выступает переменная voted, отражающая то, проголосовал ли респондент
# на последних выборах.

pca_fit %>% 
  fviz_pca_biplot(label = "var",
                  col.var = amerika_palettes$Republican[2],
                  col.ind = amerika_palettes$Democrat[3]) +
  labs(title = "") +
  theme_minimal()+
  theme(axis.title = element_text(size=15),
        axis.text = element_text(size=17))
 
pca_fit %>% 
  fviz_pca_var(col.var = "contrib") +
  scale_color_gradient(high = amerika_palettes$Democrat[1], 
                       low = amerika_palettes$Republican[1]) +
  labs(color = "Contribution",
       title = "") +
  theme_minimal()+
  theme(axis.title = element_text(size=15),
        axis.text = element_text(size=17))

# На графиках видно, что первая ГК характеризуется переменной, отражающей интерес к политике, 
# а также вопросами о политической системе и удовлетворенностью жизнью. Вторая ГК же характеризуется
# переменной, отражающей то, как респондент располагает себя на шкале "левые-правые", а также лояльностью
# к лидерам и ценностью подчинения власти. 

ess %>% 
  ggplot(aes(pca_fit$x[, 1], 
             pca_fit$x[, 2], 
             col = factor(voted))) +
  geom_point() +
  stat_ellipse() +
  scale_color_manual(values=c(amerika_palettes$Republican[1], 
                              amerika_palettes$Democrat[1]),
                     name="Voted",
                     breaks=c("0", "1"),
                     labels=c("No", "Yes")) +
  labs(x = "Principal Component 1",
       y = "Principal Component 2") +
  theme_minimal()+
  theme(axis.title = element_text(size=15),
        axis.text = element_text(size=17),
        legend.text = element_text(size=13),
        legend.title = element_text(size=15))

# На графике видно, что респонденты, которые проголосовали на последних выборах и которые не проголосовали,
# не образуют четких групп.


