library(readxl)
library(tidyverse)
library(janitor)
library(ggridges)
library(ggrepel)
library(mgcv)


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
                       pool)) |> #assuming pool is path at AFSC, but we should check
  mutate(in_top_30 = ifelse(rif == 10, 1, 0)) |> #categorical variable for whether score was in the top 30th based on RIF credits
#Also keep separate AK/NW data
  mutate(
    year = factor(year, levels = c("FY24", "FY25")),
    path = factor(path),
    year_top30 = interaction(year, in_top_30, drop = TRUE)
  )

nw <- filter(all, center == "NWFSC") |>
  mutate(got_raise = ifelse(performance_pay_increase > 0, 1, 0))

ak <- filter(all, center == "AFSC") |>
  mutate(got_raise = ifelse(percent_increase > 0, 1, 0))

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

#Can we estimate what the 70-30 cutoff was based on RIF credits?
rif_cutoff <- all |> 
  group_by(year, center, path) |> 
  summarise(
    highest_5 = max(score[rif == 5]),
    lowest_10 = min(score[rif == 10]),
    .groups = "drop"
  )
#Potentially something a bit funky with NWFSC ZA/ZP rif credits in 2024 IF they should be considered the same pool

ggplot(rif_cutoff |> filter(!is.na(highest_5)), aes(y = factor(year))) +
  # Draw the connecting gap segment
  geom_segment(aes(x = highest_5, xend = lowest_10, yend = factor(year)), 
               color = "grey60", linewidth = 1) +
  # Point for highest 5 RIF
  geom_point(aes(x = highest_5, color = "Highest RIF 5"), size = 3) +
  # Point for lowest 10 RIF
  geom_point(aes(x = lowest_10, color = "Lowest RIF 10"), size = 3) +
  facet_grid(center ~ path) +
  scale_color_manual(values = c("Highest RIF 5" = "#2b5c8f", "Lowest RIF 10" = "#d95f02")) +
  labs(
    title = "Estimated 70-30 Cutoff",
    x = "Score Boundary",
    y = "Year",
    color = "Cutoff Marker"
  ) +
  geom_text_repel(
    aes(x = highest_5, label = round(highest_5, 1)),
    nudge_x = -0.5,
    direction = "x",
    size = 3.2,
    fontface = "bold",
    segment.color = NA # Hides leader lines
  ) +
  
  # Repelled Labels for RIF 10 (pushed right)
  geom_text_repel(
    aes(x = lowest_10, label = round(lowest_10, 1)),
    nudge_x = 0.5,
    direction = "x",
    size = 3.2,
    fontface = "bold",
    segment.color = NA
  )+
  theme_minimal()

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

afsc_pay_mod <- aov(percent_increase ~ score + year, data = ak)
afsc_bonus_mod <- aov(bonus ~ score + year, data = ak) 

#anova probably not the best model. What about a hurdle model? GAM?
nw_binom <- gam(
  got_raise ~
    year * in_top_30 +
    s(score, by = interaction(year, in_top_30), k = 6) +
    s(path, bs = "re"),
  family = binomial(link = "logit"),
  data = nw,
  method = "REML"
)


