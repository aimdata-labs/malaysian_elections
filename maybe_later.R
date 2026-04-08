Malaysia, famous for its cash handouts, should focus on disbursing cash regularly to its poorest citizens. Blanket distributions such as myKasih are wasteful in that the value of a ringgit is much different to those under the poverty line than those above it. 

<br>
  
  ```{r}
census_fed |> 
  ggplot(aes(x = population_density, y = poverty_incidence)) + 
  geom_point() + 
  scale_x_log10(labels = comma) + 
  scale_y_continuous(label = percent, 
                     breaks = seq(0, 1, .1)) +
  labs(title = "Federal constituency", 
       x = "Population density", 
       y = "Poverty incidence") +
  expand_limits(y = c(0, .75)) +
  
  census_dun |> 
  mutate(population_density = population_total / area_km2) |> 
  ggplot(aes(x = population_density, y = (poverty_incidence / 100))) + 
  geom_point() +
  scale_x_log10(labels = comma) + 
  scale_y_continuous(label = percent, 
                     breaks = seq(0, 1, .1)) +
  labs(title = "State constituency", 
       x = "Population density", 
       y = "Poverty incidence") + 
  
  plot_annotation(title = "Inverse relationship between gini and population density", 
                  subtitle = "R-squared of 0.28 amongst federal consituencies and 0.20 amongst state ones")
```
<br>
  
  Below, we observe the relationship between Gini (economic inequality, where 0 is perfect equality and 1 is where one person has all the income). 


<br>
  
  ```{r}
census_fed |> 
  ggplot(aes(x = population_density, y = gini)) + 
  geom_point() + 
  scale_x_log10(labels = comma) + 
  labs(title = "Federal constituency", 
       x = "Population density", 
       y = "Gini") +
  expand_limits(y = c(0, .78)) +
  
  census_dun |> 
  mutate(population_density = population_total / area_km2) |> 
  ggplot(aes(x = population_density, y = gini)) + 
  geom_point() +
  scale_x_log10(labels = comma) + 
  labs(title = "State constituency", 
       x = "Population density", 
       y = "Gini") + 
  expand_limits(y = c(0, .78)) +
  
  plot_annotation(title = "Very weak relationship between gini and population density", 
                  subtitle = "R-squared of 0.03 amongst federal constituencies and 0.04 amongst state constituencies")
```

<br>
  
  
  
  
  
  
  
  
  
  https://themalaysianreserve.com/2025/10/13/the-curious-case-of-a-dwindling-development-budget/
  
  
  
  Irrespective of this, grievances persist,

That the government would rather embrace further malapportionment than allow for greater federalism is telling

Perhaps one should look to politicians 





```{r}
ballots |> 
  filter(election == "GE-15" & federal == "Federal" & 
           str_detect(state, "Labuan|Sabah|Sarawk")) |> 
  filter(result == "won") |> 
  arrange(votes)
```


Federalism 
Neglect from the federal government 
Not enough autonomy or economic development 


```{r median-age-candidates}

ballots |> 
  mutate(year = year(date), 
         decade = year - year %% 10) |> 
  group_by(decade, federal) |> 
  summarise(mean_age = mean(age, na.rm = TRUE), 
            median_age = median(age, na.rm = TRUE)) |> 
  ggplot(aes(x = decade, y = median_age)) +
  geom_col(fill = "#31572c") + 
  geom_text(aes(label = median_age), 
            vjust = "inward", 
            colour = "grey80") +
  geom_line(aes(x = decade, y = population_median_age), 
            colour = "#e9c46a",
            data = 
              median_age |>
              mutate(decade = year - year %% 10) |>
              group_by(decade) |>
              summarise(population_median_age = mean(median_age),
                        .groups = "drop")) +
  scale_x_continuous(breaks = seq(1950, 2020, 10)) + 
  scale_y_continuous(breaks = seq(0, 60, 10)) +
  facet_wrap(~ federal) +
  theme(strip.background = element_rect(fill = "grey30")) + 
  labs(x = "", 
       y = "Median age of candidates", 
       title = "Median age of candidates in Federal and State elections", 
       subtitle = "Median age of general population in yellow")
```