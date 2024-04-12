---
title: "From Tax to Equality"
subtitle: "Replikationsberichtfile für die Masterhausarbeit im Modul 3: Normative und positive politische Theorie"
author: "Lucas Schwarz"
date: "2024-03-20"
format:
  html:
    code-fold: false
    embed-resources: true
toc: true
number-sections: true
---

# Einführung in den Replikationsbericht 
## Verwendung des Berichts als HTML
Dieses HTML-Dokument wurde mit Quarto für R erstellt. Diese HTML-Datei kann über Firefox oder jeden chromium-basierten Browser (Google Chrome, Microsoft Edge, etc) lokal und sicher geöffnet werden. Mit dieser kann jeder Schritt der Analyse nachvollzogen werden. An der Rechten Seite kann mithilfe des interaktiven Inhaltsverzeichnisses einfach zu den verschiedenen Kapiteln navigiert werden.

## Verwendung des Quarto-Dokuments (HA_Justice.qmd) in RStudio
Das qmd. sollte in der Lage sein, den finalen Datensatz "equaltax_balanced.csv" beim Ausführen in RStudio aus dem Verzeichnis zu importieren und alle Berechnungen auszuführen. Zudem können im HTML-Dokument die Ergebnisse der Berechnungen eingesehen und alle Schritte nachvollzogen werden. Während des Bearbeitungsprozesses wurden diverse Datensetze getestet in verschiedenen Konfigurationen. Überbleibsel davon sind noch im Code vorhanden. Der korrekte Datensatz, der zur Analyse benutzt wurde, wurde am Ende des Kapitel "Data Preparation" erstellt. Dieser kann ab Kapitel 3 "Descriptive Statistics" einfach importiert werden und für alle folgenden Analysen verwendet werden. Der Vorbereitungscode muss daher nicht extra ausgeführt werden.

::: {.callout-important}
## **Wichtig**
**Dazu muss der gesamte Ordner "Schwarz_TaxtoEquality_HA-Replikation" als Projekt in R geöffnet werden und dieser zuvor vollständig heruntergeladen worden sein.**
:::

# Setup

```{r}
#| label: setup
#| echo: true
#| message: false
#| warning: false
# devtools::install_github('Mikata-Project/ggthemr')

if(!require("pacman")) {install.packages("pacman");library(pacman)}
p_load(here, tidyverse, countrycode, readxl, scales, patchwork, readxl, rio, plm, lmtest, vdemdata, wid, sjPlot, vtable, Amelia, stargazer, sandwich)

```

# Data Preparation

## Import Datasets
### Import WID Dataset

```{r}
#| echo: true
#| eval: false
# install.packages("devtools")
# devtools::install_github("WIDworld/wid-r-tool")

# Define the specific function for low code download of the WID dataset
wid_downloader <- function(indicator, population, name){
  df <-  download_wid(
                        indicators = indicator,
                        areas = "all",
                        years = "all",
                        perc = population,
                        ages = "992",
                        pop = "j",
                        metadata = FALSE,
                        include_extrapolations = TRUE,
                        verbose = FALSE
                      )

  filtered_df <- df %>%
    rename_with(~ name, .cols = "value") %>%
    select(-variable, -percentile)

  var_name <- paste0("WID_", name)
    
  assign(var_name, filtered_df, envir = .GlobalEnv)
}

# Download the indicators

## Wealth Inequality Measures
wid_downloader("ghweal", "p0p100", "Wealth_Gini") # Wealth Gini coefficients
wid_downloader("shweal", "p0p50", "Wealth_Share_bottom50") # Wealth Share of the bottom 50%
wid_downloader("shweal", "p90p100", "Wealth_Share_top10") # Wealth Share of the top 10%
wid_downloader("shweal", "p99p100", "Wealth_Share_top1") # Wealth Share of the top 1%

## Income Inequality Measures
wid_downloader("gdiinc", "p0p100", "IncomePost_Gini") # Post-tax income Gini coefficients
wid_downloader("gptinc", "p0p100", "IncomePre_Gini") # Pre-tax income Gini coefficients
wid_downloader("scainc", "p0p50", "IncomeDispPost_Share_bottom50") # Post-tax disposale income share of the bottom 50%
wid_downloader("scainc", "p90p100", "IncomeDispPost_Share_top10") # Post-tax disposale income share of the top 10%
wid_downloader("scainc", "p99p100", "IncomeDispPost_Share_top1") # Post-tax disposale income share of the top 1%

# Join all the dataframes
WID_dfs_full <- list(WID_Wealth_Gini, WID_Wealth_Share_bottom50, WID_Wealth_Share_top10, WID_Wealth_Share_top1)

WID_dfs_gini <- list(WID_Wealth_Gini)

WID_sublister <- function(list, df_name){
  df <- reduce(list, inner_join, by = c("country", "year"))
  df$iso3c <- countrycode(df$country, "iso2c", "iso3c")
  filtered_df <- df %>%
  rename( Country = iso3c,
          Year = year) %>%
  select(-country)

  assign(df_name, filtered_df, envir = .GlobalEnv)
}

WID_sublister(WID_dfs_full, "WID_full")
WID_sublister(WID_dfs_gini, "WID_gini")


rm(WID_Wealth_Share_top1, WID_Wealth_Share_top10, WID_Wealth_Share_bottom50, WID_Wealth_Gini, WID_IncomeDispPost_Share_top1, WID_IncomeDispPost_Share_top10, WID_IncomeDispPost_Share_bottom50, WID_IncomePost_Gini, WID_IncomePre_Gini, list_of_WID_dfs, name, WID_dfs_full, WID_dfs_gini)

write.csv(WID_gini, "data/processed_data/WID_Inequality_gini.csv")
write.csv(WID_full, "data/processed_data/WID_Inequality_full.csv")
```


### Import OECD Tax Revenue Data

```{r}
#| echo: true
#| eval: false
OECD_TaxRev_Raw <- read_csv(here("data", "OECD", "OECD_TaxRev_on_EIG.csv"))

Data_Prep_oecd <- function(dataset, tax_code, tax_var){

  for(i in tax_code){
    # Filter the dataframe and select necessary columns
    filtered_df <- dataset %>%
      filter(GOV == "NES", TAX == i, VAR == tax_var) %>%
      select(c("COU", "Country", "Year", "Value", "GOV", "Level of government", "TAX", "Tax revenue", "VAR", "YEA"))

    # Get the unique value from the "Tax revenue" column and prepend "OECD_"
    var_name <- paste0("OECD_TaxRev_", as.character(substr(as.character((unique(filtered_df$"Tax revenue"))), start = 1, stop = 4)))

    # Rename the 'Value' column and select necessary columns
    filtered_df <- filtered_df %>%
      rename(!!unique(filtered_df$"Tax revenue") := Value) %>%
      select(-c("GOV", "Level of government", "Tax revenue", "TAX", "YEA", "VAR"))

    # Assign the dataframe to a new variable with the name stored in var_name
    assign(var_name, filtered_df, envir = .GlobalEnv)
  }
}

OECD_TaxRev_Variables <- (c("1000", "2000", "3000", "4000", "4100", "4200", "4300", "5000", "6000"))

# Run the function to prepare the data
Data_Prep_oecd(OECD_TaxRev_Raw, OECD_TaxRev_Variables, "TAXPER") # TaxRev of Total TaxRev in Percent
Data_Prep_oecd(OECD_TaxRev_Raw, "TOTALTAX", "TAXGDP") # Total TaxRev of GDP in Percent

# Join all the dataframes
list_of_OECD_dfs <- list(OECD_TaxRev_1000, OECD_TaxRev_2000, OECD_TaxRev_3000, OECD_TaxRev_4000, OECD_TaxRev_4100, OECD_TaxRev_4200, OECD_TaxRev_4300, OECD_TaxRev_5000, OECD_TaxRev_6000, OECD_TaxRev_Tota)

OECD_TaxRev <- reduce(list_of_OECD_dfs, inner_join, by = c("Country", "Year", "COU"))

OECD_TaxRev <- OECD_TaxRev %>%
  rename( "Country_Name" = "Country",
          "Country" = "COU",
          "TaxRev_Income_IPC_1000" = "1000 Taxes on income, profits and capital gains",
          "TaxRev_Social_SSC_2000" = "2000 Social security contributions (SSC)",
          "TaxRev_Payroll_PW_3000" = "3000 Taxes on payroll and workforce",
          "TaxRev_Property_4000" = "4000 Taxes on property",
          "TaxRev_ImmovProp_4100" = "4100 Recurrent taxes on immovable property",
          "TaxRev_NetWealth_4200" = "4200 Recurrent taxes on net wealth",
          "TaxRev_EIG_4300" = "4300 Estate, inheritance and gift taxes",
          "TaxRev_GoodsServices_5000" = "5000 Taxes on goods and services",
          "TaxRev_Others_6000" = "6000 Taxes other than 1000, 2000, 3000, 4000 and 5000",
          "TaxRev_TotalofGDP" = "Total tax revenue")

rm(OECD_TaxRev_1000, OECD_TaxRev_2000, OECD_TaxRev_3000, OECD_TaxRev_4000, OECD_TaxRev_4100, OECD_TaxRev_4200, OECD_TaxRev_4300, OECD_TaxRev_5000, OECD_TaxRev_6000, list_of_OECD_dfs, OECD_TaxRev_Raw, OECD_TaxRev_Variables, OECD_TaxRev_Tota)

write.csv(OECD_TaxRev, "data/processed_data/OECD_TaxRev.csv")

```

### Import World Bank Indicators

```{r}
#| echo: true
#| eval: false
WB_Unemployment <- read_csv(here("data", "World Bank","WB_Unemployment.csv"), skip = 4)

WB_Unemployment <- WB_Unemployment %>%
  pivot_longer(-c("Country Name", "Country Code", "Indicator Name", "Indicator Code"), names_to = "Year", values_to = "Anzahl") %>%
  rename(Country = "Country Code",
         Unemployment_rate = "Anzahl") %>%
  mutate(Year = as.double(Year)) %>%
  select(Country, Year, Unemployment_rate)

WB_GDP_constant <- read_csv(here("data", "World Bank","WB_GDP_ConstantUSD.csv"), skip = 4)

WB_GDP_constant_long <- WB_GDP_constant %>%
  pivot_longer(-c("Country Name", "Country Code", "Indicator Name", "Indicator Code"), names_to = "Year", values_to = "Anzahl") %>%
  rename(Country = "Country Code",
         GDP_constant = "Anzahl") %>%
  mutate(Year = as.double(Year),  
         GDP_constant_billions = GDP_constant / 1e9) %>%
  select(Country, Year, GDP_constant_billions)

WB_GDP_current <- read_csv(here("data", "World Bank","WB_GDP_CurrentUSD.csv"), skip = 4)

WB_GDP_current_long <- WB_GDP_current %>%
  pivot_longer(-c("Country Name", "Country Code", "Indicator Name", "Indicator Code"), names_to = "Year", values_to = "Anzahl") %>%
  rename(Country = "Country Code",
         GDP_current = "Anzahl") %>%
  mutate(Year = as.double(Year),  
         GDP_current_billions = GDP_current / 1e9) %>%
  select(Country, Year, GDP_current_billions)

WB_GDP_long <- inner_join(WB_GDP_constant_long, WB_GDP_current_long, by = c("Country", "Year"))

WB <- inner_join(WB_Unemployment, WB_GDP_long, by = c("Country", "Year"))

rm(WB_GDP_constant, WB_GDP_constant_long, WB_GDP_current, WB_GDP_current_long, WB_Unemployment, WB_GDP_long)

write.csv(WB, "data/processed_data/WB_Indicators.csv")


```

### Import GC Tax Dataset

```{r}
#| echo: true
#| eval: false
# Import the dataset
GC_EIGTopRate <- read_delim(here("data", "GC Tax", "GC_TopMarginalEIGRates.csv"), delim = ";")
# Replace comma with dot and convert string to numeric
GC_EIGTopRate$Value <- as.numeric(gsub(",", ".", GC_EIGTopRate$Value))

# Convert ISO 2-letter codes to ISO 3-letter codes
GC_EIGTopRate$Country <- countrycode(GC_EIGTopRate$Country1, "country.name", "iso3c")

# Get the unique value from the "Tax revenue" column and prepend "OECD_"
var_name <- paste0("GC_", as.character(as.character((unique(GC_EIGTopRate$"Threshold vs Marginal (Thresholds)")))))

GC_EIGTopRate <- GC_EIGTopRate %>%
    select(Country, Year, Value) %>%
    rename_with(~ var_name, .cols = "Value")

rm(var_name)

write.csv(GC_EIGTopRate, "data/processed_data/GC_EIGTopRate.csv")


```


### Import VDEM Data

```{r}
#| echo: true
#| eval: false
# First, you need to have the devtools package installed
# now, install the vdemdata package directly from GitHub
#devtools::install_github("vdeminstitute/vdemdata")

VDEM <- vdem %>%
    select(c(country_text_id, year, v2peedueq, v2x_corr, v2x_libdem)) %>%
    rename( "Country" = "country_text_id",
            "Year" = "year")

write.csv(VDEM, "data/processed_data/VDEM.csv")

```

### Import Welfare Regimes

```{r}
#| echo: true
#| eval: false
Regime <- read_delim(here("data", "Welfare_Regimes","welfare_regimes_Main.csv"), delim = ";")

# Convert ISO 2-letter codes to ISO 3-letter codes
Regime$Country_Code_2 <- countrycode(Regime$Country_Name, "country.name", "iso3c")

Regime <- Regime %>%
  rename( "Country" = "Country_Code_2",
          "Country_Name" = "Country_Name",
          "welfare_regime" = "welfare_type") %>%
  select(Country, welfare_regime)

write.csv(Regime, "data/processed_data/Welfare_Regimes.csv")
```


## Merge Datasets

```{r}
#| echo: true
#| eval: false
# Join all the dataframes
dfs_all <- list(OECD_TaxRev, WID_full, VDEM, WB)
tax_all <- reduce(dfs_all, inner_join, by = c("Country", "Year"))

dfs_minimal <- list(OECD_TaxRev, WID_gini, VDEM, WB)
tax_minimal <- reduce(dfs_minimal, inner_join, by = c("Country", "Year"))

tax_minimal_regime <- merge(tax_minimal, Regime, by = c("Country"))
tax_all_regime <- merge(tax_all, Regime, by = c("Country"))


rm(WID_full, WID_gini, OECD_TaxRev, VDEM, WB, dfs_all, dfs_minimal, GC_EIGTopRate)

write.csv(tax_minimal, "data/processed_data/tax_minimal.csv")
write.csv(tax_all, "data/processed_data/tax_all.csv")
write.csv(tax_minimal_regime, "data/processed_data/tax_minimal_regime.csv")
write.csv(tax_all_regime, "data/processed_data/tax_all_regime.csv")
```
 

## Perfom Multiple Imputation

```{r}
#| echo: true
#| eval: false
# Imputation for tax_minimal_regime -dataset
tax_minimal_regime <- read_csv(here("data", "processed_data", "tax_minimal_regime.csv"))

tax_minimal_regime_imp0 <- tax_minimal_regime %>%
  select(-Country_Name, -welfare_regime)
tax_minimal_regime_imp0 <- as.data.frame(tax_minimal_regime_imp0)
tax_minimal_regime_imp <- amelia(tax_minimal_regime_imp0, m = 5, ts = "Year", cs = "Country")

tax_minimal_regime_imp_data_1 <- tax_minimal_regime_imp$imputations$imp1
tax_minimal_regime_imp_data_2 <- tax_minimal_regime_imp$imputations$imp2
tax_minimal_regime_imp_data_3 <- tax_minimal_regime_imp$imputations$imp3
tax_minimal_regime_imp_data_4 <- tax_minimal_regime_imp$imputations$imp4
tax_minimal_regime_imp_data_5 <- tax_minimal_regime_imp$imputations$imp5

plot(density(tax_minimal_regime$Unemployment_rate, na.rm=TRUE))
lines(density(tax_minimal_regime_imp$imputations$imp1$Unemployment_rate), col="red", lty=3, lwd=2)
lines(density(tax_minimal_regime_imp$imputations$imp2$Unemployment_rate), col="green", lty=3, lwd=2)
lines(density(tax_minimal_regime_imp$imputations$imp3$Unemployment_rate), col="blue", lty=3, lwd=2)
lines(density(tax_minimal_regime_imp$imputations$imp4$Unemployment_rate), col="orange", lty=3, lwd=2)
lines(density(tax_minimal_regime_imp$imputations$imp4$Unemployment_rate), col="purple", lty=3, lwd=2)

plot(density(tax_minimal_regime$GDP_constant_billions, na.rm=TRUE))
lines(density(tax_minimal_regime_imp$imputations$imp1$GDP_constant_billions), col="red", lty=3, lwd=2)
lines(density(tax_minimal_regime_imp$imputations$imp2$GDP_constant_billions), col="green", lty=3, lwd=2)
lines(density(tax_minimal_regime_imp$imputations$imp3$GDP_constant_billions), col="blue", lty=3, lwd=2)
lines(density(tax_minimal_regime_imp$imputations$imp4$GDP_constant_billions), col="orange", lty=3, lwd=2)
lines(density(tax_minimal_regime_imp$imputations$imp4$GDP_constant_billions), col="purple", lty=3, lwd=2)

# Choose imputed dataset
tax_minimal_regime_imp <- tax_minimal_regime_imp_data_2

Regime <- read_csv(here("data", "processed_data", "Welfare_Regimes.csv"))
tax_minimal_regime_imp <- merge(tax_minimal_regune_imp, Regime, by = c("Country"))

rm(tax_minimal_regime_imp0, tax_minimal_regime_imp_data_1, tax_minimal_regime_imp_data_2, tax_minimal_regime_imp_data_3, tax_minimal_regime_imp_data_4, tax_minimal_regime_imp_data_5, Regime, tax_minimal_regime)

```

## Removing Outliers

```{r}
#| echo: true
#| eval: false
# Remove outliers
# Assuming df is your data frame, group_var is your grouping variable, and var is your variable
zero_groups <- tax_minimal_regime_imp %>%
  group_by(Country) %>%
  summarise(sum_var = sum(TaxRev_EIG_4300, na.rm = TRUE)) %>%
  filter(sum_var <= 1)
zero_groups

tax_minimal_regime_imp_reduced <- tax_minimal_regime_imp %>%
  filter(Country != "AUS",
         Country != "EST",
         Country != "NZL")

rm(tax_minimal_regime_imp)

```

## Balancing the datasets

```{r}
#| echo: true
#| eval: false
equaltax <- tax_minimal_regime_imp_reduced %>%
  filter(Year >= 1995, Year <= 2021) %>%
  group_by(Country) %>%
  filter(n() >= 26) %>%
  ungroup()

rm(tax_minimal_regime_imp_reduced)

write.csv(equaltax, "data/processed_data/equaltax_balanced.csv")

```



# Desciptive Statistics

## Summary Stats of the Equaltax Dataset

```{r}
# Summary of the tax_minimal dataset
equaltax <- read_csv(here("data", "processed_data", "equaltax_balanced.csv")) 

table1 <- sumtable(equaltax,
         vars=c('Country', 'Year', 'TaxRev_Income_IPC_1000', 'TaxRev_EIG_4300', 'TaxRev_ImmovProp_4100', 'TaxRev_NetWealth_4200', 'TaxRev_TotalofGDP', 'v2x_libdem', 'v2peedueq', 'Unemployment_rate', 'GDP_constant_billions'),
         summ=c('notNA(x)',
                'mean(x)',
                'median(x)',
                'sd(x)',
                'min(x)',
                'pctile(x)[25]',
                'pctile(x)[75]',
                'max(x)'),
                # out = 'latex',
                # file = "latex/tables/table1.tex"
                )
table1
```

## Graphical Setup

```{r}
mein_theme <- function(){
    theme_minimal(base_size = 15) +
     theme( plot.title = element_text(family="Roboto", face="bold", size=15),
            axis.title.x = element_text(family="Roboto", face="plain", size=12),
            axis.title.y = element_text(family="Roboto", face="plain", size=12),
            legend.title = element_text(family="Roboto", face="bold", size=12),
            text = element_text(family="Roboto", face="plain", size=12)) 
}

palette1 <- c("#fd7f6f", "#7eb0d5", "#b2e061", "#bd7ebe", "#ffb55a", "#ffee65", "#beb9db", "#fdcce5", "#8bd3c7")
nb.cols <- 23
mycolors <- colorRampPalette(palette1)(nb.cols)

windowsFonts(Merriweather=windowsFont("Roboto"))

```


## Descriptive Graphs

```{r}
equaltax <- read_csv(here("data", "processed_data", "equaltax_balanced.csv")) 


### Figure 1 - Vermögensungleichheit über die Zeit

equaltax %>% 
  ggplot(aes(x=Year, y=Wealth_Gini)) +
    facet_wrap(~Country) +
    geom_line(color="#69b3a2") +
    labs(x = "",
         y = "Wealth Gini Index") +
    mein_theme()


### Figure 2 - EIG Tax Revenue über die Zeit

equaltax %>% 
  ggplot(aes(x=Year, y=TaxRev_EIG_4300)) +
    facet_wrap(~Country) +
    geom_line(color="#69b3a2") +
    labs(x = "",
         y = "EIG Tax Revenue (%-Anteil an gesamten Tax Revenue)") +
    mein_theme()


### Figure 3 - EIG Tax Revenue und Vermögensungleichheit

# Create a scatter plot of the Wealth Gini and TaxRev_EIG_4300
fe.dummy<-lm(Wealth_Gini ~ TaxRev_Income_IPC_1000 + TaxRev_EIG_4300 + TaxRev_ImmovProp_4100 + TaxRev_NetWealth_4200 + TaxRev_TotalofGDP + v2x_libdem + v2peedueq + GDP_constant_billions + Unemployment_rate + factor(Country)-1, data=equaltax) 
summary(fe.dummy)

# Create a new data frame with the predicted values and the original data
df <- data.frame("yhat" = fe.dummy$fitted, equaltax)

ggplot(df, aes(x = TaxRev_EIG_4300, y = yhat, colour = Country)) +
  geom_point() +
  labs(x = "EIG TAX", y = "predicted Wealth Inequality") +
  geom_smooth(method = "lm", se = FALSE, size = 0.75) +
  geom_smooth(method = "lm", se = FALSE, color = "black", size = 1.5) +
  scale_color_manual(values = mycolors) +
  labs(x = "EIG-Tax Revenue (%-Anteil an gesamten Tax Revenue)",
       y = "Wealth Gini-Koeffizient",
       colour = "Land") +
  mein_theme()



```

# Regression Models


## Deciding for a specific model

```{r}
# Estimate a fixed effects model
model_fe <- plm(Wealth_Gini ~ TaxRev_Income_IPC_1000 + TaxRev_EIG_4300 + TaxRev_ImmovProp_4100 + TaxRev_NetWealth_4200 + TaxRev_TotalofGDP + v2x_libdem + v2peedueq + Unemployment_rate + GDP_constant_billions, 
             data = equaltax, model = "within", effect = "individual", index = c("Country", "Year"))
summary(model_fe)
# Compute panel-corrected standard errors
coeftest(model_fe, vcov = vcovHC(model_fe, type = "HC3"))

# Estimate a random effects model
model_re <- plm(Wealth_Gini ~ TaxRev_Income_IPC_1000 + TaxRev_EIG_4300 + TaxRev_ImmovProp_4100 + TaxRev_NetWealth_4200 + TaxRev_TotalofGDP + v2x_libdem + v2peedueq + Unemployment_rate + GDP_constant_billions, 
             data = equaltax,  model = "random", effect = "individual", index = c("Country", "Year"))
summary(model_re)
# Compute panel-corrected standard errors
coeftest(model_re, vcov = vcovHC(model_re, type = "HC3"))

# Perform a Hausman test
phtest(model_fe, model_re) # fe Model wins 

# Against Beck-Katz
model_beckkatz <- plm(Wealth_Gini ~ lag(Wealth_Gini, 1) + TaxRev_Income_IPC_1000 + TaxRev_EIG_4300 + TaxRev_ImmovProp_4100 + TaxRev_NetWealth_4200 + TaxRev_TotalofGDP + v2x_libdem + v2peedueq + Unemployment_rate + GDP_constant_billions, 
         data = equaltax, model = "pooling", effect = "individual", index= c("Country", "Year"))
summary(model_beckkatz)
#  beck-katz approach leads to way to low standard errors

```

## Summary Thoughts of the Modelling Decision
Based on the updated comments, here is a revised conclusion:

The Beck-Katz approach was found to significantly underestimate standard errors, especially in the context of unbalanced panel data, and is therefore not recommended for this analysis.

Instead, an Ordinary Least Squares (OLS) regression model with Fixed Effects (FE) and a Lagged Dependent Variable should be used. To address the issue of heteroskedasticity, Panel-Corrected Standard Errors (PCSEs) should be calculated post-estimation.

In the process of data preparation, countries with a very low number of observations or those that have no data after 1980 should be excluded from the analysis to avoid potential bias or inaccuracies.

Furthermore, the assumptions of the random effects model do not align with the expectations of the research question. This misalignment is reflected in the coefficients of the random effects model, which, when compared to the coefficients of the fixed effects model, do not make sense as they partially point in the wrong direction.

Finally, to ensure the validity and reliability of the model, other diagnostic tests should be performed. These could include tests for multicollinearity, serial correlation, and stationarity, among others. These additional checks will help to confirm the appropriateness of the chosen model and the robustness of the findings.

## SE Correction Tests

```{r}
bgtestASE <- function(model){
  # Compute vcovHC standard errors
  se_hc <- sqrt(diag(vcovHC(model, type = "HC3")))

  # Adjust residuals
  resid_adj <- residuals(model) / se_hc

  # Perform Breusch-Godfrey test on adjusted residuals
  bgtest(resid_adj ~ fitted.values(model), order = 1)
}

bgtestPCSE <- function(model){
  # Compute vcovBK standard errors
  se_hc <- sqrt(diag(vcovBK(model, cluster = "time")))

  # Adjust residuals
  resid_adj <- residuals(model) / se_hc

  # Perform Breusch-Godfrey test on adjusted residuals
  bgtest(resid_adj ~ fitted.values(model), order = 1)
}
bgtestDKSE <- function(model){
  # Compute vcovSCC standard errors
  se_hc <- sqrt(diag(vcovSCC(model, cluster = "time")))

  # Adjust residuals
  resid_adj <- residuals(model) / se_hc

  # Perform Breusch-Godfrey test on adjusted residuals
  bgtest(resid_adj ~ fitted.values(model), order = 1)
}
```

### Arellano standard errors (ASE)-FE-Models 

```{r}
equaltax <- read_csv(here("data", "processed_data", "equaltax_balanced.csv")) 

model_fe <- plm(Wealth_Gini ~ TaxRev_Income_IPC_1000 + TaxRev_EIG_4300 + TaxRev_ImmovProp_4100 + TaxRev_NetWealth_4200 + TaxRev_TotalofGDP + v2x_libdem + v2peedueq + GDP_constant_billions + Unemployment_rate, 
             data = equaltax, model = "within", effect = "individual", index = c("Country", "Year"))

## Standard Model
# Preusch-Pagan Test ohne Korrektur
pbgtest(model_fe) # Autokorrelation vorhanden
# Preusch-Pagan Test mit ASE-Korrektur
bgtestASE(model_fe) # Korrektur hilft gegen Autokorrelation
# Store Model with ASE Correction
model_feASE <- coeftest(model_fe, vcov = vcovHC(model_fe, type = "HC3"))
```

### Panel corrected standard errors (PCSE)-FE-Models 

```{r}
equaltax <- read_csv(here("data", "processed_data", "equaltax_balanced.csv")) 

model_fe <- plm(Wealth_Gini ~ + TaxRev_Income_IPC_1000 + TaxRev_EIG_4300 + TaxRev_ImmovProp_4100 + TaxRev_NetWealth_4200 + TaxRev_TotalofGDP + v2x_libdem + v2peedueq + GDP_constant_billions + Unemployment_rate, 
             data = equaltax, model = "within", effect = "individual", index = c("Country", "Year"))
## Standard Model
# Preusch-Pagan Test ohne Korrektur
pbgtest(model_fe) # Autokorrelation vorhanden
# Preusch-Pagan Test mit PCSE-Korrektur
bgtestDKSE(model_fe) # Korrektur hilft gegen Autokorrelation
# Store Model with PCSE Correction
model_fePCSE <- coeftest(model_fe, vcov=vcovBK(model_fe, cluster = "time"))
```

### Driscoll and Kraay standard errors (DKSE)-FE-Models 

```{r}
equaltax <- read_csv(here("data", "processed_data", "equaltax_balanced.csv")) 

model_fe <- plm(Wealth_Gini ~ TaxRev_Income_IPC_1000 + TaxRev_EIG_4300 + TaxRev_ImmovProp_4100 + TaxRev_NetWealth_4200 + TaxRev_TotalofGDP + v2x_libdem + v2peedueq + GDP_constant_billions + Unemployment_rate, 
             data = equaltax, model = "within", effect = "individual", index = c("Country", "Year"))

## Standard Model
# Preusch-Pagan Test ohne Korrektur
pbgtest(model_fe) # Autokorrelation vorhanden
# Preusch-Pagan Test mit ASE-Korrektur
bgtestASE(model_fe) # Korrektur hilft gegen Autokorrelation
# Store Model with ASE Correction
model_feDKSE <- coeftest(model_fe, vcov=vcovSCC(model_fe, cluster = "time"))

```

### Lagged Dependent Variable Model

```{r}
equaltax <- read_csv(here("data", "processed_data", "equaltax_balanced.csv"))

# Model 4 - Extended-Control Model Plus
model_fe_lag <- plm(Wealth_Gini ~ lag(Wealth_Gini, 1) + TaxRev_Income_IPC_1000 + TaxRev_EIG_4300 + TaxRev_ImmovProp_4100 + TaxRev_NetWealth_4200 + TaxRev_TotalofGDP + v2x_libdem + v2peedueq + GDP_constant_billions + Unemployment_rate, 
             data = equaltax, model = "within", effect = "individual", index = c("Country", "Year"))
summary(model_fe_lag)

## Standard Model
# Preusch-Pagan Test ohne Korrektur
pbgtest(model_fe_lag) # Autokorrelation vorhanden trotz LDV
# Preusch-Pagan Test mit ASE-Korrektur
bgtestPCSE(model_fe_lag) # Korrektur hilft gegen Autokorrelation, wird durch LDV aber nicht besser

# Store Model with ASE Correction
model_feLDV_PCSE <- coeftest(model_fe_lag, vcov=vcovSCC(model_fe_lag, cluster = "time"))

```


### Create RegTable A1 with Model Comparison

```{r}
# Create a regtable with the results

stargazer(model_fe, model_fe, model_fe, model_fe_lag, type = "text",
          se = list(model_feASE[,2], model_fePCSE[,2], model_feDKSE[,2], model_feLDV_PCSE[,2]),
          p = list(model_feASE[,4], model_fePCSE[,4], model_feDKSE[,4], model_feLDV_PCSE[,4]),
          covariate.labels = c("LDV", "TaxRev Einkommen", "TaxRev EIG", "TaxRev Eigentum", "TaxRev Nettovermögen", "TaxRev Anteil an BIP", "Liberaler Demokratieindex", "Bildungsgleichheit", "BIP", "Arbeitslosenrate"), # Replace with your new covariate names
          column.labels = c("Arellano", "PCSE", "Driscoll/Kraay", "LDV+PCSE") # Replace with your new column names
          #, out = "manuscript/tables/tableA1.tex"
          )

# Achtung: LDV Werte müssen teilweise manuell entfernt werden
```

## Main Model Stack 

```{r}

equaltax <- read_csv(here("data", "processed_data", "equaltax_balanced.csv")) 
### Main Models (Wealth Gini as DP)

# Model 1 - Minimal Model
model_fe_1 <- plm(Wealth_Gini ~ TaxRev_EIG_4300, 
             data = equaltax, model = "within", effect = "individual", index = c("Country", "Year"))
summary(model_fe_1)

# Model 2 - First-Control Model
model_fe_2 <- plm(Wealth_Gini ~ TaxRev_EIG_4300 + TaxRev_Income_IPC_1000 + TaxRev_ImmovProp_4100 + TaxRev_NetWealth_4200 + TaxRev_TotalofGDP, 
             data = equaltax, model = "within", effect = "individual", index = c("Country", "Year"))
summary(model_fe_2)

# Model 3 - Extended-Control Model
model_fe_3 <- plm(Wealth_Gini ~ TaxRev_EIG_4300 + TaxRev_Income_IPC_1000 + TaxRev_ImmovProp_4100 + TaxRev_NetWealth_4200 + TaxRev_TotalofGDP + v2x_libdem + v2peedueq, 
             data = equaltax, model = "within", effect = "individual", index = c("Country", "Year"))
summary(model_fe_3)

# Model 4 - Extended-Control Model Plus
model_fe_4 <- plm(Wealth_Gini ~ TaxRev_EIG_4300 + TaxRev_Income_IPC_1000 + TaxRev_ImmovProp_4100 + TaxRev_NetWealth_4200 + TaxRev_TotalofGDP + v2x_libdem + v2peedueq + GDP_constant_billions + Unemployment_rate, 
             data = equaltax, model = "within", effect = "individual", index = c("Country", "Year"))
summary(model_fe_4)

```

### RegTable of Main Models 1-4

```{r}
# Compute panel-corrected standard errors
pcse_1 <- coeftest(model_fe_1, vcov=vcovBK(model_fe_1, cluster = "time"))
pcse_2 <- coeftest(model_fe_2, vcov=vcovBK(model_fe_2, cluster = "time"))
pcse_3 <- coeftest(model_fe_3, vcov=vcovBK(model_fe_3, cluster = "time"))
pcse_4 <- coeftest(model_fe_4, vcov=vcovBK(model_fe_4, cluster = "time"))

stargazer(model_fe_1, model_fe_2, model_fe_3, model_fe_4, type = "text",
          se = list(pcse_1[,2], pcse_2[,2], pcse_3[,2], pcse_4[,2]),
          p = list(pcse_1[,4], pcse_2[,4], pcse_3[,4], pcse_4[,4]),
          covariate.labels = c("TaxRev EIG", "TaxRev Einkommen", "TaxRev Eigentum", "TaxRev Nettovermögen", "TaxRev Anteil an BIP", "Liberaler Demokratieindex", "Bildungsgleichheit", "BIP", "Arbeitslosenrate") # Replace with your new covariate names
          #, out = "manuscript/tables/table2.tex"
          )
```



## Wealth Model Stack - Minimal Dataset- Standarized Models

```{r}
equaltax_std <- as.data.frame(scale(equaltax[, c("Wealth_Gini", "TaxRev_Income_IPC_1000", "TaxRev_EIG_4300", "TaxRev_ImmovProp_4100", "TaxRev_NetWealth_4200", "TaxRev_TotalofGDP", "v2x_libdem", "v2peedueq", "GDP_constant_billions", "Unemployment_rate")]))

# Add the 'Country' and 'Year' columns back to the dataframe
equaltax_std$Country <- equaltax$Country
equaltax_std$Year <- equaltax$Year

# Run the models with the standardized variables
# Model 1 - Minimal Model
model_fe_1_std <- plm(Wealth_Gini ~ TaxRev_EIG_4300, 
             data = equaltax_std, model = "within", effect = "individual", index = c("Country", "Year"))
summary(model_fe_1_std)

# Model 2 - First-Control Model
model_fe_2_std <- plm(Wealth_Gini ~ TaxRev_EIG_4300 + TaxRev_Income_IPC_1000 + TaxRev_ImmovProp_4100 + TaxRev_NetWealth_4200 + TaxRev_TotalofGDP, 
             data = equaltax_std, model = "within", effect = "individual", index = c("Country", "Year"))
summary(model_fe_2_std)

# Model 3 - Extended-Control Model
model_fe_3_std <- plm(Wealth_Gini ~ TaxRev_EIG_4300 + TaxRev_Income_IPC_1000 + TaxRev_ImmovProp_4100 + TaxRev_NetWealth_4200 + TaxRev_TotalofGDP + v2x_libdem + v2peedueq, 
             data = equaltax_std, model = "within", effect = "individual", index = c("Country", "Year"))
summary(model_fe_3_std)

# Model 4 - Extended-Control Model Plus
model_fe_4_std <- plm(Wealth_Gini ~ TaxRev_EIG_4300 + TaxRev_Income_IPC_1000 + TaxRev_ImmovProp_4100 + TaxRev_NetWealth_4200 + TaxRev_TotalofGDP + v2x_libdem + v2peedueq + GDP_constant_billions + Unemployment_rate, 
             data = equaltax_std, model = "within", effect = "individual", index = c("Country", "Year"))
summary(model_fe_4_std)

```

### RegTable of Standardized Models 1-4

```{r}

# Compute panel-corrected standard errors
pcse_1_std <- coeftest(model_fe_1_std, vcov=vcovBK(model_fe_1_std, cluster = "time"))
pcse_2_std <- coeftest(model_fe_2_std, vcov=vcovBK(model_fe_2_std, cluster = "time"))
pcse_3_std <- coeftest(model_fe_3_std, vcov=vcovBK(model_fe_3_std, cluster = "time"))
pcse_4_std <- coeftest(model_fe_4_std, vcov=vcovBK(model_fe_4_std, cluster = "time"))

# Create the regression table with stargazer
stargazer(model_fe_1_std, model_fe_2_std, model_fe_3_std, model_fe_4_std, type = "text",
          se = list(pcse_1_std[,2], pcse_2_std[,2], pcse_3_std[,2], pcse_4_std[,2]),
          p = list(pcse_1_std[,4], pcse_2_std[,4], pcse_3_std[,4], pcse_4_std[,4]),
          covariate.labels = c("TaxRev EIG", "TaxRev Einkommen", "TaxRev Eigentum", "TaxRev Nettovermögen", "TaxRev Anteil an BIP", "Liberaler Demokratieindex", "Bildungsgleichheit", "BIP", "Arbeitslosenrate") # Replace with your new covariate names
          #, out = "manuscript/tables/tableA2.tex"
          )
```


## Alternative Lag Tests

```{r}
equaltax <- read_csv(here("data", "processed_data", "equaltax_balanced.csv")) 


# Model 1 - Lag3-Model
model_alt_1 <- plm(Wealth_Gini ~ lag(TaxRev_EIG_4300, 3) + lag(TaxRev_Income_IPC_1000, 3) + lag(TaxRev_ImmovProp_4100, 3) + lag(TaxRev_NetWealth_4200, 3) + lag(TaxRev_TotalofGDP, 3) + v2x_libdem + v2peedueq + Unemployment_rate + GDP_constant_billions, 
             data = equaltax, model = "within", effect = "individual", index = c("Country", "Year"))
summary(model_alt_1)

# Model 2 - Lag5-Model
model_alt_2 <- plm(Wealth_Gini ~ lag(TaxRev_EIG_4300, 5) + lag(TaxRev_Income_IPC_1000, 5) + lag(TaxRev_ImmovProp_4100, 5) + lag(TaxRev_NetWealth_4200, 5) + lag(TaxRev_TotalofGDP, 5) + v2x_libdem + v2peedueq + Unemployment_rate + GDP_constant_billions, 
             data = equaltax, model = "within", effect = "individual", index = c("Country", "Year"))
summary(model_alt_2)

# Model 3 - Reverse-Lag5
model_alt_3 <- plm(Wealth_Gini ~ lag(TaxRev_EIG_4300, -5) + lag(TaxRev_Income_IPC_1000, -5) + lag(TaxRev_ImmovProp_4100, -5) + lag(TaxRev_NetWealth_4200, -5) + lag(TaxRev_TotalofGDP, -5) + v2x_libdem + v2peedueq + Unemployment_rate + GDP_constant_billions, 
             data = equaltax, model = "within", effect = "individual", index = c("Country", "Year"))
summary(model_alt_3)

# Model 4 - Reverse Lag5+Extended-Control Model
model_alt_4 <- plm(TaxRev_EIG_4300 ~ lag(Wealth_Gini, 5), 
             data = equaltax, model = "within", effect = "individual", index = c("Country", "Year"))
summary(model_alt_4)
```

### RegTable of Causality Models 1-4

```{r}
# Compute panel-corrected standard errors
pcse_alt1 <- coeftest(model_alt_1, vcov=vcovBK(model_alt_1, cluster = "time"))
pcse_alt2 <- coeftest(model_alt_2, vcov=vcovBK(model_alt_2, cluster = "time"))
pcse_alt3 <- coeftest(model_alt_3, vcov=vcovBK(model_alt_3, cluster = "time"))
pcse_alt4 <- coeftest(model_alt_4, vcov=vcovBK(model_alt_4, cluster = "time"))


# Create the regression table with stargazer
stargazer(model_alt_1, model_alt_2, model_alt_3, model_alt_4, type = "text",
          se = list(pcse_alt1[,2], pcse_alt2[,2], pcse_alt3[,2], pcse_alt4[,2]),
          p = list(pcse_alt1[,4], pcse_alt2[,4], pcse_alt3[,4], pcse_alt4[,4])
          # covariate.labels = c("TaxRev Einkommen", "TaxRev EIG", "TaxRev Eigentum", "TaxRev Nettovermögen", "TaxRev Anteil an BIP", "Liberaler Demokratieindex", "Bildungsgleichheit", "BIP", "Arbeitslosenrate"), # Replace with your new covariate names
          #, out = "manuscript/tables/tableA3.tex"
          )


```


# Additional Regression diagnostics


```{r}
lm_model <- lm(Wealth_Gini ~ TaxRev_Income_IPC_1000 + TaxRev_EIG_4300 + TaxRev_ImmovProp_4100 + TaxRev_NetWealth_4200 + TaxRev_TotalofGDP + v2x_libdem + v2peedueq + GDP_constant_billions + Unemployment_rate, 
             data = equaltax)
summary(lm_model) 

# Check for multicollinearity
library(performance)
check_collinearity(lm_model)


equaltax %>%
  count(Country) %>%
  print(n = 100)

print(min(equaltax$Year, na.rm = TRUE))
print(max(equaltax$Year, na.rm = TRUE))


```

