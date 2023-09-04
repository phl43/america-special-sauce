library(tidyverse)
library(haven)
library(srvyr)
library(ggtext)

# the data can be downloaded at https://usa.ipums.org/usa/
data <- read_sav("Data/ACS - 2017-2021.sav") %>%
  mutate(
    EDUCD = ifelse(EDUCD == 999, NA, EDUCD),
    INCTOT = ifelse(INCTOT == 9999999, NA, INCTOT),
    non_citizen = BPL > 120 & CITIZEN %in% c(2, 3),
    age_group = case_when(
      AGE < 18 ~ "Under 18",
      AGE >= 18 & AGE <= 24 ~ "18 to 24",
      AGE > 24 & AGE <= 34 ~ "25 to 34",
      AGE > 34 & AGE <= 44 ~ "35 to 44",
      AGE > 44 & AGE <= 54 ~ "45 to 54",
      AGE > 54 & AGE <= 64 ~ "55 to 64",
      AGE > 64 ~ "65 and more",
      TRUE ~ NA_character_
    ),
    sex = as_factor(SEX),
    country = as_factor(BPL),
    education = case_when(
      EDUCD < 101 ~ "Non-college educated",
      EDUCD >= 101 ~ "College educated"
    ) 
  )

svy <- as_survey(
  data,
  weight = PERWT,
  repweights = matches("REPWTP[0-9]+"),
  type = "JK1",
  scale = 4/ 80 ,
  rscales = rep(1, 80),
  mse = TRUE
) %>%
  filter(non_citizen & age_group != "Under 18") %>%
  group_by(country, education, sex, age_group) %>%
  summarize(
    total_income = survey_mean(INCTOT, na.rm = TRUE, vartype = "ci"),
    N = n()
  ) %>%
  ungroup()

ggplot(svy %>% filter(sex == "Male", age_group == "35 to 44", education == "Non-college educated", N > 150), aes(x = country, y = total_income)) +
  geom_pointrange(
    aes(ymin = total_income_low, ymax = total_income_upp),
    color = "steelblue"
  ) +
  theme_bw() +
  ggtitle(
    "Average total income of non-college educated male immigrants between the age of 35 and 44 in the US by country of origin (2017-2021)",
    subtitle = "(the bars are 95% confidence intervals and only groups with more than 150 people in the sample are shown)"
  ) +
  xlab("Country/region of origin") +
  ylab("Average total income") +
  scale_y_continuous(label = scales::dollar_format()) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 90, vjust = 0.5)
  ) +
  labs(
    caption = "Source: US Census Bureau - American Community Survey - Chart by Philippe Lemoine (@phl43)"
  )

ggsave(
  "Average total income of non-college educated male immigrants between the age of 35 and 44 in the US by country of origin.png",
  width = 15,
  height = 6
  )

# source: https://www.cairn.info/revue-francaise-de-sociologie-2013-1-page-5.htm (table D, exact matching 1)
school_performance_france <- tribble(
  ~country, ~test_score_difference, ~test_score_difference_low, ~test_score_difference_upp,
  "Algeria", -0.17, -0.17 - 1.96 * 0.11, -0.17 + 1.96 * 0.11,
  "China/South East Asia", 0.55, 0.55 - 1.96 * 0.16, 0.55 + 1.96 * 0.16,
  "Southern Europe", -0.36, -0.36 - 1.96 * 0.13, -0.36 + 1.96 * 0.13,
  "Gulf of Guinea", -0.34, -0.34 - 1.96 * 0.26, -0.34 + 1.96 * 0.26,
  "Morocco", -0.17, -0.17 - 1.96 * 0.14, -0.17 + 1.96 * 0.14,
  "Sahel", -0.64, -0.64 - 1.96 * 0.19, -0.64 + 1.96 * 0.19,
  "Tunisia", -0.17, -0.17 - 1.96 * 0.16, -0.15 + 1.96 * 0.15,
  "Turkey", -0.66, -0.66 - 1.96 * 0.14, -0.66 + 1.96 * 0.14
)

ggplot(school_performance_france, aes(x = country, y = test_score_difference)) +
  geom_pointrange(
    aes(ymin = test_score_difference_low, ymax = test_score_difference_upp),
    color = "steelblue"
  ) +
  theme_bw() +
  ggtitle(
    "Difference in school performance between children of immigrants and children of natives with a comparable socio-economic background in France",
    subtitle = "(the bars are 95% confidence intervals and exact one-to-many matching was used for the comparison)"
  ) +
  xlab("Country/region of origin") +
  ylab("Test score difference") +
  labs(caption = "Source: Mathieu Ichou, \"Différences d'origine et origine des différences : les résultats scolaires des enfants d'émigrés/immigrés en France du début de l'école primaire à la fin du collège\", *Revue française de sociologie*, Vol. 54, No. 1, pp. 5-52, 2013 - Chart by Philippe Lemoine (@phl43)") +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_textbox_simple(
      maxwidth = 0.55,
      hjust = 1,
      padding = margin(15, 0, 0, 0)
    ),
    axis.text.x = element_text(angle = 90, vjust = 0.5)
  )

ggsave(
  "Difference in school performance between children of immigrants and children of natives after controlling for socio-economic background in France.png",
  width = 15,
  height = 6
)
