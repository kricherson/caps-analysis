library(readxl)
library(tidyverse)
library(janitor)
library(ggridges)

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


#note: no info on RIF credits for AK 2024
ak24 <- read_xlsx('data/FY24 and FY25 AKC CAPS Information for IFPTE Request.xlsm', sheet = "AKC FY24") |>
  clean_names() |>
  mutate(year = "FY24") |>
  mutate(center = "AFSC")

ak25 <- read_xlsx('data/FY24 and FY25 AKC CAPS Information for IFPTE Request.xlsm', sheet = "AKC FY25") |>
  clean_names() |>
  mutate(year = "FY25") |>
  rename(rif = rif_credit) |>
  mutate(center = "AFSC")

all <- bind_rows(nw24, nw25, ak24, ak25) |>
  select(-x1) |>
  mutate(percent_increase = ifelse(is.na(percent_increase) & center == "AFSC",
                                   0,
                                   percent_increase),
         performance_pay_increase = ifelse(is.na(performance_pay_increase) & center == "NWFSC",
                                           0,
                                           performance_pay_increase)) #assuming blank means no increase at AFSC

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
  as.data.frame()

#check to see whether the raises, RIF credits seem to fall along the 70-30 split
all2 <- all |>
  group_by(year, center, pool) |>
  mutate(quant70 = quantile(score, 0.70),
         in_top_30 = score > quant70, #to check - should this be > or >=?
         got_raise = performance_pay_increase > 0 | percent_increase == 0,
         got_10_rifcreds = rif == 10)

all2 |>
  group_by(year, center, pool, quant70, in_top_30, got_raise, got_10_rifcreds) |>
  summarise(n = n()) |>
  arrange(year, center, desc(n))

ggplot(all, aes(x = score, fill = pool))+
  geom_histogram(position = "dodge")+
  facet_grid(year~center)


m1 <- lm(score ~ pool*band + year , data = all)

summary(m1)

ggplot(nw, aes(x = score, y = year))+
  geom_density_ridges2()

#media bonus/raise for a given score across years
#can share with WG but ask them not to share outside
#ANCOVA bonus ~ score + year
