library(tidyverse)
library(haven)
library(modelsummary)
library(panelView)
library(fixest)
library(broom)
library(glue)
library(gt)
library(RColorBrewer)


#### Data Set-Up

dta <- read_dta("data/Final_Main.dta")
dd <- read_csv("data/ddrevisited_data_v1.csv")

dd <- dd |> 
  select(
    year, cowcode, regime
  ) |> 
  rename(ccode = cowcode) |>
  mutate(year = year - 4)
  

dta <- dta |> 
  left_join(dd, by = c("year" = "year", "ccode" = "ccode")) |> 
  filter(year >= 1987 & year <= 2004) |>
  mutate(ParliDem = as.numeric(regime == 0),
         MixedDem = as.numeric(regime == 1),
         PresDem = as.numeric(regime == 2),
         CivDict = as.numeric(regime == 3),
         MilDict = as.numeric(regime == 4),
         RoyalDict = as.numeric(regime == 5))

#### Descriptive Statistics Table

table1 <- dta |> 
  select(
    new_empinxavg, polity2avg, ParliDem, MixedDem, PresDem, 
    CivDict, MilDict, RoyalDict, EV, l2CPcol2, covihme_ayem, 
    covwdi_exp, covwdi_fdi, covwdi_imp, covwvs_rel, 
    coviNY_GDP_PETR_RT_ZS, covdemregion, covloggdp, covloggdpC
  ) |> 
  mutate_all(~ ifelse(. == -99, NA, .)) |> 
  pivot_longer(cols = everything()) |> 
  group_by(name) |> 
  summarize(
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    n = sum(!is.na(value))
  ) |> 
  mutate(category = recode(
    name,
    new_empinxavg = "Outcome",
    polity2avg = "Outcome",
    ParliDem = "Outcome",
    MixedDem = "Outcome",
    PresDem = "Outcome",
    CivDict = "Outcome",
    MilDict = "Outcome",
    RoyalDict = "Outcome",
    EV = "Treatment",
    l2CPcol2 = "Treatment",
    .default = "Covariate")) |> 
  mutate(name = recode_factor(
    name,
    new_empinxavg = "CIRI Human Empowerment Index Score (4-Year Average)",
    polity2avg = "Polity IV Score (4-Year Average)",
    ParliDem = "Parliamentary Democracy (4 Years Lead)",
    MixedDem = "Mixed (semi-presidential) Democracy (4 Years Lead)",
    PresDem = "Presidential Democracy (4 Years Lead)",
    CivDict = "Civilian Dictatorship (4 Years Lead)",
    MilDict = "Military Dictatorship (4 Years Lead)",
    RoyalDict = "Royal Dictatorship (4 Years Lead)",
    EV = "EU Aid (millions of log once-lagged dollars)",
    l2CPcol2 = "Twice Lagged Former Colony Status",
    covihme_ayem = "Average Years Education (Male)",
    covwdi_exp = "Log Exports",
    covwdi_fdi = "FDI",
    covwdi_imp = "Log Imports",
    covwvs_rel = "Religiosity",
    coviNY_GDP_PETR_RT_ZS = "Petroleum Revenues",
    covdemregion = "Democracies in Region",
    covloggdp = "Log GDP",
    covloggdpC = "Log GDP per Capita")) |>
  arrange(name) |> 
  group_by(category) |> 
  gt() |> 
  cols_align("left", columns = name) |> 
  cols_label(mean = "Mean", sd = "Std. Dev.", name = "") |> 
  fmt_number(columns = c(mean, sd)) |> 
  fmt_integer(columns = n) |> 
  tab_style(style = cell_text(weight = "bold"), locations = cells_row_groups())

gtsave(table1, file = "tables/table1.png")


#### Figure

dta <- dta |> 
  mutate(regime = factor(regime))


dta_count <- dta |> 
  filter(!is.na(regime)) |> 
  group_by(year, regime) |> 
  summarise(count = n())


regime_labels <- c("Parliamentary Democracy",
                   "Mixed Democracy",
                   "Presidential Democracy",
                   "Civilian Dictatorship",
                   "Military Dictatorship",
                   "Royal Dictatorship")

ggplot(dta_count, aes(x = year, y = count, fill = factor(regime))) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = brewer.pal(6, "Set3"), 
                    breaks = 0:5, 
                    labels = regime_labels) +
  labs(title = "Regime Types Over Time",
       x = "Year",
       y = "Count",
       fill = "Regime Type") +
  theme_minimal()

#### Regression Models

fit1 <- feols(new_empinxavg ~ 1 | ccode + year | EV ~ l2CPcol2, data = dta)
fit2 <- feols(polity2avg ~ 1 | ccode + year | EV ~ l2CPcol2, data = dta)
fit3 <- feols(ParliDem ~ 1 | ccode + year | EV ~ l2CPcol2, data = dta)
fit4 <- feols(MixedDem ~ 1 | ccode + year | EV ~ l2CPcol2, data = dta)
fit5 <- feols(PresDem ~ 1 | ccode + year | EV ~ l2CPcol2, data = dta)
fit6 <- feols(CivDict ~ 1 | ccode + year | EV ~ l2CPcol2, data = dta)
fit7 <- feols(MilDict ~ 1 | ccode + year | EV ~ l2CPcol2, data = dta)
fit8 <- feols(RoyalDict ~ 1 | ccode + year | EV ~ l2CPcol2, data = dta)

model1 <- modelsummary(
  list(
    "CIRI Human Empowerment Index Score (4-Year Average)" = fit1,
    "Polity IV Score (4-Year Average)" = fit2,
    "Parliamentary Democracy (Lead 4 Years)" = fit3,
    "Mixed Democracy (Lead 4 Years)" = fit4,
    "Presidential Democracy (Lead 4 Years)" = fit5,
    "Civilian Dictatorship (Lead 4 Years)" = fit6,
    "Military Dictatorship (Lead 4 Years)" = fit7,
    "Royal Dictatorship (Lead 4 Years)" = fit8
  ),
  coef_rename = c(
    "fit_EV" = "EU Aid"
  ),
  gof_map = c("nobs", "FE: ccode", "FE: year"),
  output = "gt") |> 
  gtsave("models/model1.png")



covariates <- str_subset(colnames(dta), "cov")
spec1_form <- glue("new_empinxavg ~ {str_c(covariates, collapse = ' + ')} | ccode + year | EV ~ l2CPcol2")
spec2_form <- glue("polity2avg ~ {str_c(covariates, collapse = ' + ')} | ccode + year | EV ~ l2CPcol2")
spec3_form <- glue("ParliDem ~ {str_c(covariates, collapse = ' + ')} | ccode + year | EV ~ l2CPcol2")
spec4_form <- glue("MixedDem ~ {str_c(covariates, collapse = ' + ')} | ccode + year | EV ~ l2CPcol2")
spec5_form <- glue("PresDem ~ {str_c(covariates, collapse = ' + ')} | ccode + year | EV ~ l2CPcol2")
spec6_form <- glue("CivDict ~ {str_c(covariates, collapse = ' + ')} | ccode + year | EV ~ l2CPcol2")
spec7_form <- glue("MilDict ~ {str_c(covariates, collapse = ' + ')} | ccode + year | EV ~ l2CPcol2")
spec8_form <- glue("RoyalDict ~ {str_c(covariates, collapse = ' + ')} | ccode + year | EV ~ l2CPcol2")


fit9 <-  feols(as.formula(spec1_form), data = dta)
fit10 <-  feols(as.formula(spec2_form), data = dta)
fit11 <-  feols(as.formula(spec3_form), data = dta)
fit12 <-  feols(as.formula(spec4_form), data = dta)
fit13 <-  feols(as.formula(spec5_form), data = dta)
fit14 <-  feols(as.formula(spec6_form), data = dta)
fit15 <-  feols(as.formula(spec7_form), data = dta)
fit16 <-  feols(as.formula(spec8_form), data = dta)


model2 <- modelsummary(
  list(
    "CIRI Human Empowerment Index Score (4-Year Average)" = fit9,
    "Polity IV Score (4-Year Average)" = fit10,
    "Parliamentary Democracy (Lead 4 Years)" = fit11,
    "Mixed Democracy (Lead 4 Years)" = fit12,
    "Presidential Democracy (Lead 4 Years)" = fit13,
    "Civilian Dictatorship (Lead 4 Years)" = fit14,
    "Military Dictatorship (Lead 4 Years)" = fit15,
    "Royal Dictatorship (Lead 4 Years)" = fit16
  ),
  gof_map = c("nobs", "FE: ccode", "FE: year", ""),
    coef_rename = c(
    "fit_EV" = "EU Aid"
  ),
  coef_omit = "cov",
  output = "gt"
  )|> 
  gtsave("models/model2.png")

