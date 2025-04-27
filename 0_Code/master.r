# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#
# PROJECT:		 Empirical exercise : market potential and development.
# AUTHORS:     Vivan Sharma, Guillaume Pousse
# TASK:				 To replicate Redding and Venables (2004) regressions and graphs for a recent set of years.
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Load packages
library(haven)
library(dplyr)
library(tidyr)
library(data.table)
library(fixest)
library(modelsummary)
library(patchwork)
library(ggplot2)

# Clear all
rm(list = ls())

# Replace the Working Directory to where .zip file is stored
wd <- "/Users/glpou/Documents/SCIENCESPO/M2/S4/Trade/HW1/"
setwd(wd)

# Load data
df <- read_dta("1_Data/biltrade.dta")

# Preliminary cleaning
df <- df %>%
  mutate(
    # Log transformations
    log_gdpo = log(gdp_o),
    log_gdpd = log(gdp_d),
    ln_distw = log(distw),
    
    # Handling 'flow' and creating log flow
    nflow = ifelse(flow == 0, 1, flow),
    ln_nflow = log(nflow),
    ln_flow = log(flow),
    
    # GDP per capita and its log
    gdppco = gdp_o / (pop_o * 1e6),
    log_gdppco = log(gdppco)
  )

# # # # # # # # # # # # # 
# ---- PART A: 2016 only
# # # # # # # # # # # # #

df2016 <- df %>%
  filter(year == 2016)

# First stage gravity estimations:
# OLS
ols2016 <- feols(
  log(flow) ~ log(distw) + contig + comlang_off + comcur + fta_wto | iso_o + iso_d, 
  cluster = ~iso_o, 
  data = df2016
)
coef_ols2016 <- coef(ols2016)
ols_fe = fixef(ols2016)$iso_d

# Poisson 
pois2016 <- fepois(
  flow ~ log(distw) + contig + comlang_off + comcur + fta_wto | iso_o + iso_d, 
  cluster = ~iso_o, 
  data = df2016
)
coef_pois2016 <- coef(pois2016)
pois_fe = fixef(pois2016)$iso_d

model1a <- list(
  "OLS" = feols(log(flow) ~ log(distw) + contig + comlang_off + comcur + fta_wto | iso_o + iso_d,  cluster = ~iso_o,  data = df2016),
  "Poisson" = fepois(flow ~ log(distw) + contig + comlang_off + comcur + fta_wto | iso_o + iso_d,  cluster = ~iso_o, data = df2016)
)

table1_final <- modelsummary(
  model = model1a,
  stars = TRUE,
  coef_map = c(
    "log(distw)" = "log(Distance)",
    "contig" = "Contiguity",
    "comlang_off" = "Common language",
    "comcur" = "Common currency",
    "fta_wto" = "FTAs"
  ),
  gof_omit = "AIC|BIC|RMSE|Std.|Within",  
  output = "latex_tabular",
  title = "Gravity estimation",
  notes = "Standard errors clustered at the exporter level (iso_o).",
)

writeLines(as.character(table1_final), con = "2_Output/table1.tex")

# Construct FMP:
df2016 <- df2016 %>%
  mutate(
    phi_ols = 
      (distw ^ coef_ols2016["log(distw)"]) *
      (ifelse(contig == 1, exp(coef_ols2016["contig"]), 1)) *
      (ifelse(comlang_off == 1, exp(coef_ols2016["comlang_off"]), 1)) *
      (ifelse(comcur == 1, exp(coef_ols2016["comcur"]), 1)) *
      (ifelse(fta_wto == 1, exp(coef_ols2016["fta_wto"]), 1)),
    
    phi_pois = 
      (distw ^ coef_pois2016["log(distw)"]) *
      (ifelse(contig == 1, exp(coef_pois2016["contig"]), 1)) *
      (ifelse(comlang_off == 1, exp(coef_pois2016["comlang_off"]), 1)) *
      (ifelse(comcur == 1, exp(coef_pois2016["comcur"]), 1)) *
      (ifelse(fta_wto == 1, exp(coef_pois2016["fta_wto"]), 1))
  )

setDT(df2016)

df2016[, ols_fehat := exp(ols_fe[paste0(iso_d)])]
df2016[, pois_fehat := exp(pois_fe[paste0(iso_d)])]

df2016[, mult_ols := phi_ols * ols_fehat]
df2016[, mult_phi := phi_pois * pois_fehat]

# Sum by exporter
df2016[, fmp_ols := sum(mult_ols, na.rm = TRUE), by = iso_o]
df2016[, fmp_pois := sum(mult_phi, na.rm = TRUE), by = iso_o]

model1b <- list(
  "OLS" = feols(log(gdppco) ~ log(fmp_ols), cluster = ~iso_o, data = df2016),
  "OLS" = feols(log(gdppco) ~ log(fmp_ols) | iso_d, cluster = ~iso_o, data = df2016),
  "Poisson" = fepois(log(gdppco) ~ log(fmp_pois), cluster = ~iso_o, data = df2016),
  "Poisson" = fepois(log(gdppco) ~ log(fmp_pois) | iso_d, cluster = ~iso_o, data = df2016)
)

table2_final <- modelsummary(
  model = model1b,
  shape = term ~ model,
  stars = TRUE,
  coef_map = c(
    "log(fmp_ols)" = "ln FMP",
    "log(fmp_pois)" = "ln FMP"
  ),
  gof_omit = "AIC|BIC|RMSE|Std.|Within",  
  output = "latex_tabular",
  title = "Fundamental Market Potential and GDP per Capita",
  notes = "Standard errors clustered at the exporter level (iso_o).",
)

writeLines(as.character(table2_final), con = "2_Output/table2.tex")

## Plot
# Extract unique data for 2016 
plot_data = unique(df2016[, .(iso_o, gdppco, fmp_ols, fmp_pois)])
plot_data = plot_data[!is.na(gdppco)]

# Filter to remove zero-FMP observations
plot_data = plot_data[!((fmp_ols < 0.00001) | (fmp_pois < 0.00001))]

#Adjust theme
common_theme <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

fmp_ols_plot <- ggplot(data = plot_data, 
                       aes(x = log(fmp_ols), y = log(gdppco), 
                           label = iso_o)) +
  geom_text(size = 3) +
  theme_minimal(base_size = 14) +
  labs(x = "ln FMP (OLS)", y = "ln GDP per Capita") +
  common_theme

fmp_pois_plot <- ggplot(data = plot_data, 
                       aes(x = log(fmp_pois), y = log(gdppco), 
                           label = iso_o)) +
  geom_text(size = 3) +
  theme_minimal(base_size = 14) +
  labs(x = "ln FMP (Poisson)", y = "ln GDP per Capita") +
  common_theme

totplot <- fmp_ols_plot + fmp_pois_plot

ggsave("2_Output/figure1_combined.pdf", plot = totplot, width = 14, height = 6)

# # # # # # # # # # # # # 
# ---- PART B: 2004-2016
# # # # # # # # # # # # #

dfnew <- df %>%
  filter(year %in% 2004:2016)

# First stage gravity estimations:
# OLS
olsnew <- feols(
  log(flow) ~ log(distw) + contig + comlang_off + comcur + fta_wto | iso_o + iso_d, 
  cluster = ~iso_o, 
  data = dfnew
)
coef_olsnew <- coef(olsnew)
ols_fenew = fixef(ols2016)$iso_d

# Poisson 
poisnew <- fepois(
  flow ~ log(distw) + contig + comlang_off + comcur + fta_wto | iso_o + iso_d, 
  cluster = ~iso_o, 
  data = dfnew
)
coef_poisnew <- coef(poisnew)
pois_fenew = fixef(poisnew)$iso_d

model3 <- list(
  "OLS" = feols(log(flow) ~ log(distw) + contig + comlang_off + comcur + fta_wto | iso_o + iso_d,  cluster = ~iso_o,  data = dfnew),
  "Poisson" = fepois(flow ~ log(distw) + contig + comlang_off + comcur + fta_wto | iso_o + iso_d,  cluster = ~iso_o, data = dfnew)
)

table3_final <- modelsummary(
  model = model3,
  stars = TRUE,
  coef_map = c(
    "log(distw)" = "log(Distance)",
    "contig" = "Contiguity",
    "comlang_off" = "Common language",
    "comcur" = "Common currency",
    "fta_wto" = "FTAs"
  ),
  gof_omit = "AIC|BIC|RMSE|Std.|Within",  
  output = "latex_tabular",
  title = "Gravity estimation",
  notes = "Standard errors clustered at the exporter level (iso_o).",
)

writeLines(as.character(table3_final), con = "2_Output/table3.tex")

# Construct FMP:
dfnew<- dfnew %>%
  mutate(
    newphi_ols = 
      (distw ^ coef_olsnew["log(distw)"]) *
      (ifelse(contig == 1, exp(coef_ols2016["contig"]), 1)) *
      (ifelse(comlang_off == 1, exp(coef_ols2016["comlang_off"]), 1)) *
      (ifelse(comcur == 1, exp(coef_ols2016["comcur"]), 1)) *
      (ifelse(fta_wto == 1, exp(coef_ols2016["fta_wto"]), 1)),
    
    newphi_pois = 
      (distw ^ coef_poisnew["log(distw)"]) *
      (ifelse(contig == 1, exp(coef_pois2016["contig"]), 1)) *
      (ifelse(comlang_off == 1, exp(coef_pois2016["comlang_off"]), 1)) *
      (ifelse(comcur == 1, exp(coef_pois2016["comcur"]), 1)) *
      (ifelse(fta_wto == 1, exp(coef_pois2016["fta_wto"]), 1))
  )

setDT(dfnew)

dfnew[, newols_fehat := exp(ols_fenew[paste0(iso_d)])]
dfnew[, newpois_fehat := exp(pois_fenew[paste0(iso_d)])]

dfnew[, newmult_ols := newphi_ols * newols_fehat]
dfnew[, newmult_phi := newphi_pois * newpois_fehat]

# Sum by exporter
dfnew[, newfmp_ols := sum(newmult_ols, na.rm = TRUE), by = iso_o]
dfnew[, newfmp_pois := sum(newmult_phi, na.rm = TRUE), by = iso_o]

model4 <- list(
  "OLS" = feols(log(gdppco) ~ log(newfmp_ols) | year, cluster = ~iso_o, data = dfnew),
  "OLS" = feols(log(gdppco) ~ log(newfmp_ols) | iso_d + year, cluster = ~iso_o, data = dfnew),
  "Poisson" = fepois(log(gdppco) ~ log(newfmp_pois)| year, cluster = ~iso_o, data = dfnew),
  "Poisson" = fepois(log(gdppco) ~ log(newfmp_pois) | iso_d + year, cluster = ~iso_o, data = dfnew)
)

table4_final <- modelsummary(
  model = model4,
  shape = term ~ model,
  stars = TRUE,
  coef_map = c(
    "log(newfmp_ols)" = "ln FMP",
    "log(newfmp_pois)" = "ln FMP"
  ),
  gof_omit = "AIC|BIC|RMSE|Std.|Within",  
  output = "latex_tabular",
  title = "Fundamental Market Potential and GDP per Capita",
  notes = "Standard errors clustered at the exporter level (iso_o).",
)

writeLines(as.character(table4_final), con = "2_Output/table4.tex")

# # # # 
# END
# # # #