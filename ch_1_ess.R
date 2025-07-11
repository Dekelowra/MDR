install.packages(c("tidyverse", "here", "skimr", "naniar", "recipes", "tictoc", "knitr", "haven"))

library(tidyverse)
library(skimr)
library(naniar)
library(recipes)
library(tictoc)
library(knitr)
library(haven)
library(here)

ess_start <- read_sav("https://github.com/Dekelowra/Data/blob/main/ESS11-subset.sav?raw=true")

ess_raw <- ess_start %>%
  select(polintr, psppsgva, actrolga, psppipla, cptppola, trstprl, trstlgl, trstplc,
         trstplt, trstprt, trstep, trstun, lrscale, stflife, stfeco, stfgov, stfdem,
         stfedu, stfhlth, gincdif, freehms, hmsfmlsh, hmsacld, euftf, lrnobed, loylead,
         imsmetn, imdfetn, impcntr, imbgeco, imueclt, imwbcnt, vote) %>%
  mutate(voted = case_when(vote == 1 ~ 1, 
                           vote == 2 ~ 0, 
                           vote == 3 ~ 0, 
                           vote == 7 ~ NA, 
                           vote == 8 ~ NA,
                           vote == 9 ~ NA), 
         polintr = replace(polintr, polintr > 4, NA), 
         psppsgva = replace(psppsgva, psppsgva > 5, NA),
         actrolga = replace(actrolga, actrolga > 5, NA),
         psppipla = replace(psppipla, psppipla > 5, NA),
         cptppola = replace(cptppola, cptppola > 5, NA),
         trstprl = replace(trstprl, trstprl > 10, NA),
         trstlgl = replace(trstlgl, trstlgl > 10, NA),
         trstplc = replace(trstplc, trstplc > 10, NA),
         trstplt = replace(trstplt, trstplt > 10, NA),
         trstprt = replace(trstprt, trstprt > 10, NA),
         trstep = replace(trstep, trstep > 10, NA),
         trstun = replace(trstun, trstun > 10, NA),
         lrscale = replace(lrscale, lrscale > 10, NA),
         stflife = replace(stflife, stflife > 10, NA),
         stfeco = replace(stfeco, stfeco > 10, NA),
         stfgov = replace(stfgov, stfgov > 10, NA),
         stfdem = replace(stfdem, stfdem > 10, NA),
         stfedu = replace(stfedu, stfedu > 10, NA),
         stfhlth = replace(stfhlth, stfhlth > 10, NA),
         gincdif = replace(gincdif, gincdif > 5, NA),
         freehms = replace(freehms, freehms > 5, NA),
         hmsfmlsh = replace(hmsfmlsh, hmsfmlsh > 5, NA),
         hmsacld = replace(hmsacld, hmsacld > 5, NA),
         euftf = replace(euftf, euftf > 10, NA),
         lrnobed = replace(lrnobed, lrnobed > 5, NA),
         loylead = replace(loylead, loylead > 5, NA),
         imsmetn = replace(imsmetn, imsmetn > 4, NA),
         imdfetn = replace(imdfetn, imdfetn > 4, NA),
         impcntr = replace(impcntr, impcntr > 4, NA),
         imbgeco = replace(imbgeco, imbgeco > 10, NA),
         imueclt = replace(imueclt, imueclt > 10, NA),
         imwbcnt = replace(imwbcnt, imwbcnt > 10, NA)
  ) %>% 
  glimpse()

ess_raw <- ess_raw %>%
  select(-vote)

ess_raw %>% 
  select(order(desc(colnames(.)))) %>% 
  gg_miss_which() + 
  labs(caption = "Note: Gray = Missing, Black = Complete")

ess_raw %>% 
  gg_miss_case_cumsum()

ess_raw %>% 
  gg_miss_var()

ess_raw %>% 
  gg_miss_var(show_pct = TRUE)

ess_raw <- ess_raw %>%
  drop_na(voted)

ess_raw %>% 
  gg_miss_var(voted) 

ess_raw %>% 
  gg_miss_upset()

recipe <- recipe(voted ~ ., 
                 data = ess_raw) %>%
  step_impute_knn(all_predictors())

{ 
  tic()
  ess_imputed <- prep(recipe) %>% 
    juice()
  toc()
  }

skim(ess_raw)
skim(ess_imputed)

ess <- ess_imputed

saveRDS(ess, 
        file = "ess.rds")
