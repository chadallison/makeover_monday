Makeover Monday Visualizations
================

Sharing weekly visualizations from the [Makeover
Monday](https://data.world/makeovermonday) series.

------------------------------------------------------------------------

# Contents

- [Importing Required Packages](#importing-required-packages)
- [Week 14, 2023: Chicago Hate
  Crimes](#week-14-2023-chicago-hate-crimes)
- [Week 15, 2023: The DougScore](#week-15-2023-the-dougscore)
- [Week 16, 2023: Retirement Ages Around the
  World](#week-16-2023-retirement-ages-around-the-world)
- [Week 17, 2023: Biggest Tomato & Potato
  Producers](#week-17-2023-biggest-tomato-potato-producers)
- [Week 18, 2023: Federal Minimum Wage by
  State](#week-18-2023-federal-minimum-wage-by-state)
- [Week 26, 2023: Most Pressured to Drink with
  Workmates](#week-26-2023-most-pressured-to-drink-with-workmates)
- [Week 27, 2023: Alcohol Consumption in OECD
  Countries](#week-27-2023-alcohol-consumption-in-oecd-countries)
- [Script Runtime](#script-runtime)

### Importing Required Packages

``` r
tictoc::tic()

library(tidyverse)
library(tidymodels)
library(lubridate)
library(tvthemes)
library(janitor)
library(patchwork)
library(readxl)
library(vip)

theme_custom = theme_avatar() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, size = 9, vjust = 2.5, face = "italic"),
        panel.grid.major = element_line(linewidth = 0.5, colour = "#D6D0C4"),
        panel.grid.minor = element_line(linewidth = 0.5, colour = "#D6D0C4"))

theme_set(theme_custom)
custom_olive = "#8C9F88"
options(scipen = 999)
```

### Week 14, 2023: Chicago Hate Crimes

*Data about hate crimes in Chicago from the Chicago Police Department*

<details>
<summary>
View Code
</summary>

``` r
df = clean_names(read_excel("data/chicago_hate_crimes.xlsx"))

five_digit_dates = df |>
  filter(nchar(date) == 5) |>
  mutate(date = as.Date(as.numeric(date), origin = "1899-01-01"))

digits_21_dates = df |>
  filter(nchar(date) == 21) |>
  mutate(date = mdy(substr(date, 1, 9)))

digits_22_dates = df |>
  filter(nchar(date) == 22) |>
  mutate(date = mdy(substr(date, 1, 10)))

df2 = bind_rows(five_digit_dates, digits_21_dates, digits_22_dates)

df2 |>
  count(date) |>
  group_by(year(date)) |>
  mutate(cum_n = cumsum(n)) |>
  ungroup() |>
  mutate(date = as_date(paste0("2020-", substr(as.character(date), 6, 10))),
         `year(date)` = factor(`year(date)`)) |>
  rename(year = "year(date)") |>
  filter(year %in% 2017:2023) |>
  ggplot(aes(date, cum_n)) +
  geom_line(aes(col = year), linewidth = 2) +
  scale_x_date(date_labels = c("December", "January", "April", "July", "October")) +
  theme(legend.position = "right") +
  labs(x = NULL, y = "Cumulative Sum",
       title = "Cumulative Sum of Hate Crimes in Chicago, 2017-2023", col = "Year")
```

</details>

![](README_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

------------------------------------------------------------------------

### Week 15, 2023: The DougScore

*Which cars are the best cars ever driven by Doug Demuro?*

<details>
<summary>
View Code
</summary>

``` r
df = clean_names(read_excel("data/doug_data.xlsx"))

makes_df = df |>
  count(make) |>
  filter(n >= 12) |>
  mutate(make_n = paste0(make, " (", n, ")"))

boxplots = df |>
  right_join(makes_df, by = "make") |>
  ggplot(aes(reorder(make_n, dougscore), dougscore)) +
  geom_boxplot(aes(fill = make_n), show.legend = F) +
  coord_flip() +
  scale_fill_manual(values = c("#002420", "#EB0D3F", "#1B5FAA", "#009ADA",
                               "#A87A25", "#972626", "#00A551", "#004377",
                               "#CC0000", "#F7DE9F", "#DFE1E0", "#FF8000",
                               "#565F64", "#B12B28", "#004489", "#F5313E")) +
  labs(x = NULL, y = "DougScore", title = "Boxplots of DougScores by Vehicle Make",
       subtitle = "Only Vehicles with 12+ Observations Included") +
  theme(plot.subtitle = element_text(hjust = 0.5, size = 9, face = "italic", vjust = 2))

df = df |>
  select(year, styling, acceleration, handling, fun_factor,
         cool_factor, features, comfort, quality, practicality, value, dougscore)

# cars_split = initial_split(df, strata = dougscore)
# cars_train = training(cars_split)
# cars_test = testing(cars_split)
cars_rec = recipe(dougscore ~ ., data = df)
# cars_prep = prep(cars_rec)
# juiced = juice(cars_prep)

# these hyperparameters were obtained from tuning
tune_spec = rand_forest(trees = 153, mtry = 7, min_n = 2) |>
  set_mode("regression") |>
  set_engine("ranger")

tune_wf = workflow() |>
  add_recipe(cars_rec) |>
  add_model(tune_spec)

# cars_folds = vfold_cv(cars_train, v = 5)
# doParallel::registerDoParallel()
# tune_res = tune_grid(tune_wf, resamples = cars_folds, grid = 25)
# best_rmse = select_best(tune_res, "rmse")
# final_rf = finalize_model(tune_spec, best_rmse)

vip_plot = tune_spec |>
  set_engine("ranger", importance = "permutation") |>
  fit(dougscore ~ ., data = df) |>
  vip(geom = "point") +
  labs(title = "Variable Importance for Predicting DougScore")

boxplots / vip_plot
```

</details>

![](README_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

------------------------------------------------------------------------

### Week 16, 2023: Retirement Ages Around the World

*At what age do people retire around the world?*

<details>
<summary>
View Code
</summary>

``` r
df = clean_names(read_excel("data/market_exit_age.xlsx")) |>
  mutate(country = ifelse(country == "China (People's Republic of)", "China", country))

top_countries = df |>
  group_by(country, gender) |>
  summarise(age = round(mean(average_age), 3),
            .groups = "drop") |>
  pivot_wider(id_cols = country, names_from = "gender", values_from = "age") |>
  mutate(diff = men - women) |>
  filter(country != "European Union (27 countries)") |>
  slice_max(diff, n = 10) |>
  pull(country)

bottom_countries = df |>
  group_by(country, gender) |>
  summarise(age = round(mean(average_age), 3),
            .groups = "drop") |>
  pivot_wider(id_cols = country, names_from = "gender", values_from = "age") |>
  mutate(diff = men - women) |>
  filter(country != "European Union (27 countries)") |>
  slice_min(diff, n = 10) |>
  pull(country)

df |>
  group_by(country, gender) |>
  summarise(age = round(mean(average_age), 3),
            .groups = "drop") |>
  pivot_wider(id_cols = country, names_from = "gender", values_from = "age") |>
  mutate(diff = men - women) |>
  filter(country %in% c(top_countries, bottom_countries)) |>
  mutate(pos_lab = ifelse(diff > 0, round(diff, 3), ""),
         neg_lab = ifelse(diff < 0, round(diff, 3), "")) |>
  ggplot(aes(reorder(country, diff), diff)) +
  geom_col(aes(fill = diff), show.legend = F) +
  geom_text(aes(label = pos_lab), size = 3, hjust = -0.25) +
  geom_text(aes(label = neg_lab), size = 3, hjust = 1.25) +
  annotate("text", x = 10, y = 9.5, label = "Dashed line indicates break between top/bottom ten", size = 3, alpha = 0.5) +
  coord_flip(ylim = c(-1, 12)) +
  scale_fill_gradient(low = "#AC92B7", high = "#5A8555") +
  geom_vline(xintercept = 10.5, linetype = "dashed", alpha = 0.5) +
  labs(x = NULL, y = "Difference in Retirement Age (Men - Women)",
       title = "Differences in Retirement Ages by Gender",
       subtitle = "Only countries with ten largest or smallest differences included") +
  theme(axis.text.x = element_blank())
```

</details>

![](README_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

------------------------------------------------------------------------

### Week 17, 2023: Biggest Tomato & Potato Producers

*Production of tomatoes and potatoes by country*

<details>
<summary>
View Code
</summary>

``` r
df = clean_names(read_csv("data/tomato_production.csv", col_types = cols()))

df |>
  group_by(item, year, element) |>
  summarise(value = sum(value),
            .groups = "drop") |>
  ggplot(aes(year, value)) +
  geom_line(aes(col = element), linewidth = 2) +
  facet_wrap(vars(item), strip.position = "bottom", nrow = 2) +
  labs(x = NULL, y = "Value", col = NULL,
       title =  "Production of Potatoes and Tomatoes, 1961 to 2021",
       subtitle = "While area harvested has stayed roughly the same, production has increased over time") +
  theme(legend.position = "right") +
  scale_y_continuous(labels = comma) +
  scale_color_manual(values = c("springgreen4", "lightgoldenrod3", "plum3"))
```

</details>

![](README_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

------------------------------------------------------------------------

### Week 18, 2023: Federal Minimum Wage by State

*How many people earned the federal minimum wage or less in each state?*

<details>
<summary>
View Code
</summary>

``` r
df2015 = read_excel("data/min_wage_state.xlsx", sheet = 7) |> mutate(year = 2015)
df2016 = read_excel("data/min_wage_state.xlsx", sheet = 6) |> mutate(year = 2016)
df2017 = read_excel("data/min_wage_state.xlsx", sheet = 5) |> mutate(year = 2017)
df2018 = read_excel("data/min_wage_state.xlsx", sheet = 4) |> mutate(year = 2018)
df2019 = read_excel("data/min_wage_state.xlsx", sheet = 3) |> mutate(year = 2019)
df2020 = read_excel("data/min_wage_state.xlsx", sheet = 2) |> mutate(year = 2020)
df2021 = read_excel("data/min_wage_state.xlsx", sheet = 1) |> mutate(year = 2021)
df = clean_names(bind_rows(df2021, df2020, df2019, df2018, df2017, df2016, df2015))

df |>
  group_by(year) |>
  summarise(mean_total = round(mean(total), 2),
            mean_at = round(mean(at_minimum_wage), 2),
            mean_below = round(mean(below_minimum_wage), 2)) |>
  pivot_longer(!year, names_to = "metric", values_to = "value") |>
  mutate(metric = case_when(metric == "mean_total" ~ "Total",
                            metric == "mean_at" ~ "At Minimum Wage",
                            metric == "mean_below" ~ "Below Minimum Wage"),
         metric = factor(metric, levels = c("Total", "At Minimum Wage", "Below Minimum Wage"))) |>
  ggplot(aes(year, value)) +
  geom_point(aes(col = metric), size = 3) +
  geom_line(aes(col = metric), linewidth = 2) +
  scale_x_continuous(labels = 2015:2021, breaks = 2015:2021) +
  scale_y_continuous(labels = paste0(seq(0, by = 0.5, to = 3), "%"), breaks = seq(0, by = 0.5, to = 3)) +
  scale_color_manual(values = c("#D4B8E3", "#A1B8DE", "#8DAD91")) +
  labs(x = NULL, y = "Percent of Population", col = NULL,
       title = "Percent of Population at or Below Minimum Wage, 2015 to 2021") +
  theme(legend.position = "bottom")
```

</details>

![](README_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

------------------------------------------------------------------------

### Week 19, 2023: Makeover Monday Metadata Analysis

<details>
<summary>
View Code
</summary>

``` r
df = clean_names(read_excel("data/mm_metadata.xlsx"))
glimpse(df)
```

</details>

    ## Rows: 336
    ## Columns: 9
    ## $ week         <chr> "2016/w1", "2016/w2", "2016/w3", "2016/w4", "2016/w5", "2…
    ## $ title        <chr> "Bryce Harper Should Have Made $73 Million More", "The NB…
    ## $ size_kb      <dbl> 12, 121, 1, 3, 1, 2048, 1, 1, 1, 29, 22, 528, 1, 4, 3, 2,…
    ## $ rows         <dbl> 30, 666, 18, 49, 15, 3323, 16, 9, 10, 385, 671, 5595, 27,…
    ## $ columns      <dbl> 7, 24, 3, 4, 3, 21, 3, 2, 4, 6, 6, 6, 3, 5, 6, 6, 4, 3, 4…
    ## $ rows_columns <dbl> 210, 15984, 54, 196, 45, 69783, 48, 18, 40, 2310, 4026, 3…
    ## $ whos_week    <chr> "Andy", "Eva", "Andy", "Eva", "Andy", "Eva", "Andy", "Eva…
    ## $ week_number  <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17…
    ## $ year         <chr> "2016", "2016", "2016", "2016", "2016", "2016", "2016", "…

------------------------------------------------------------------------

### Week 26, 2023: Most Pressured to Drink with Workmates

<details>
<summary>
View Code
</summary>

``` r
df = read_excel("data/pressure_drink.xlsx")

df |>
  magrittr::set_colnames(c("rank", "profession", "pct_pressured")) |>
  ggplot(aes(reorder(profession, pct_pressured), pct_pressured)) +
  geom_col(aes(fill = pct_pressured), show.legend = F) +
  geom_text(aes(label = paste0(pct_pressured * 100, "%")), hjust = -0.25) +
  paletteer::scale_fill_paletteer_c("scico::tokyo") +
  coord_flip(ylim = c(0, 0.9)) +
  labs(x = NULL, y = "Percent of Workers Feeling Pressured to Drink with Coworkers",
       title = "Pressure to Drink with Coworkers by Profession in the UK") +
  theme(axis.text.x = element_blank())
```

</details>

![](README_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

------------------------------------------------------------------------

### Week 27, 2023: Alcohol Consumption in OECD Countries

<details>
<summary>
View Code
</summary>

``` r
df = clean_names(read_csv("data/alcohol_consumption.csv", col_types = cols()))

yearly_avg = df |>
  group_by(time) |>
  summarise(avg_litres = mean(litres_capita))

final_litres = yearly_avg |> slice_max(time, n = 1) |> pull(avg_litres)

rus_avg = df |>
  group_by(location) |>
  summarise(total = sum(litres_capita),
            avg = mean(litres_capita)) |>
  slice_max(avg, n = 1) |>
  pull(avg) |> round(2)

df |>
  select(location, time, litres_capita) |>
  left_join(yearly_avg, by = "time") |>
  ggplot() +
  geom_line(aes(time, litres_capita, col = location), alpha = 0.5, show.legend = F) +
  geom_line(aes(time, avg_litres)) +
  annotate("text", x = 2020, y = final_litres - 0.5, label = "Global Average") +
  labs(x = NULL, y = "Litres per Capita", title = "Alcohol Consumption in OECD Countries, 1960-2022",
       subtitle = paste0("Russia has highest average of ", rus_avg, " litres per capita")) +
  scale_x_continuous(breaks = seq(1960, 2022, by = 4)) +
  scale_y_continuous(breaks = seq(0, 30, by = 2))
```

</details>

![](README_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

### Script Runtime

    ## 6.21 sec elapsed
