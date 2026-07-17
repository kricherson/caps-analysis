library(readxl)
library(tidyverse)
library(janitor)
library(ggridges)


#read in NW data
nw24 <- read_xlsx('data/NWFSC FY24 FY25 CAPS data Union request April 2026.xlsx', sheet = "FY24") |>
  clean_names() |>
  mutate(year = "FY24") |>
  rename(score = fy24_score,
         rif = rif_credit) |>
  mutate(center = "NWFSC")

nw25 <- read_xlsx('data/NWFSC FY24 FY25 CAPS data Union request April 2026.xlsx', sheet = "FY25") |>
  clean_names() |>
  mutate(year = "FY25") |>
  rename(score = fy25_score) |>
  mutate(center = "NWFSC")

#Read in AK data
#note: no info on RIF credits for AK 2024
#note: no "pool" column - can we assume it is the same as path? looks like NWFSC combined ZA/ZP in 2024
#note: only ZA and ZP in the AK data in 2025? No ZT or ZS? They do have ZS in 2024
ak24 <- read_xlsx('data/FY24 and FY25 AKC CAPS Information for IFPTE Request.xlsm', sheet = "AKC FY24") |>
  clean_names() |>
  mutate(year = "FY24") |>
  mutate(center = "AFSC") |> 
  mutate(percent_increase = percent_increase*100) #this is getting read in as a proportion, quick fix

ak25 <- read_xlsx('data/FY24 and FY25 AKC CAPS Information for IFPTE Request.xlsm', sheet = "AKC FY25") |>
  clean_names() |>
  mutate(year = "FY25") |>
  rename(rif = rif_credit) |>
  mutate(center = "AFSC")

#Combine all together 
all <- bind_rows(nw24, nw25, ak24, ak25) |>
  select(-x1) |>
  mutate(percent_increase = ifelse(is.na(percent_increase) & center == "AFSC",
                                   0,
                                   percent_increase),
         performance_pay_increase = ifelse(is.na(performance_pay_increase) & center == "NWFSC",
                                           0,
                                           performance_pay_increase)) |> #assuming blank means no increase at AFSC
  mutate(pool = ifelse(is.na(pool), 
                       path,  
                       pool)) #assuming pool is path at AFSC, but we should check

#Also keep separate AK/NW data
nw <- filter(all, center == "NWFSC")
ak <- filter(all, center == "AFSC")

#summary stats by year/center
all |> group_by(year, center) |>
  summarise(quant70 = quantile(score, .7),
            mean_score = mean(score),
            med_score = median(score),
            mean_bonus = mean(bonus),
            med_bonus = median(bonus),
            mean_inc = mean(performance_pay_increase),
            med_inc = median(performance_pay_increase),
            total_bonus = sum(bonus),
            total_inc = sum(performance_pay_increase),
            prop_5rif = sum(rif == 5)/n(),
            prop_10rif = sum(rif == 10)/n())

#summary stats by year/center/paypool
all |> group_by(year, center, pool) |>
  summarise(quant70 = quantile(score, 0.7),
            mean_score = mean(score),
            med_score = median(score),
            mean_bonus = mean(bonus),
            med_bonus = median(bonus),
            mean_inc = mean(performance_pay_increase),
            med_inc = median(performance_pay_increase),
            total_bonus = sum(bonus),
            total_inc = sum(performance_pay_increase),
            prop_5rif = sum(rif == 5)/n(),
            prop_10rif = sum(rif == 10)/n(),
            n = n()) |>
  arrange(center, year) %>% 
  as.data.frame()

#check to see whether the raises, RIF credits seem to fall along the 70-30 split
#note that managers in the pool are not included in our dataset, so the quantile calculations only reflect BU employees
all2 <- all |>
  group_by(year, center, pool) |>
  mutate(quant70 = quantile(score, 0.70),
         in_top_30 = score >= quant70, #to check - should this be > or >=?
         got_raise = (!is.na(performance_pay_increase) & performance_pay_increase > 0) | 
           (!is.na(percent_increase) & percent_increase > 0),
         got_10_rifcreds = rif == 10)

#this is probably not that informative without manager scores included
all2 |>
  group_by(year, center, pool, quant70, in_top_30, got_raise, got_10_rifcreds) |>
  summarise(n = n()) %>% 
  View()

ggplot(all, aes(x = score, fill = pool))+
  geom_histogram(position = "dodge")+
  facet_grid(year~center)
#seems like more very low scores in FY25

#try ridgeplots
ggplot(bind_rows(nw24, nw25), aes(x = score, y = year))+
  geom_density_ridges2(quantile_lines =TRUE)+
  ggtitle("NWFSC")

ggplot(bind_rows(ak24, ak25), aes(x = score, y = year))+
  geom_density_ridges2(quantile_lines =TRUE)+
  ggtitle("AFSC")

#plot score against pay increase
ggplot(nw, aes(x = score, y = performance_pay_increase, color = year, group = year))+
  geom_point()

ggplot(ak, aes(x = score, y = percent_increase, color = year, group = year))+
  geom_point()

#median? bonus/raise for a given score across years. what kind of a hit do you expect to get for a given score?
#ANCOVA bonus ~ score + year

nwfsc_pay_mod <- aov(performance_pay_increase ~ score + year, data = nw) 
nwfsc_bonus_mod <- aov(bonus ~ score + year, data = nw)

