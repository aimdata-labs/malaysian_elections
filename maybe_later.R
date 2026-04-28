Malaysia, famous for its cash handouts, should focus on disbursing cash regularly to its poorest citizens. Blanket distributions such as myKasih are wasteful in that the value of a ringgit is much different to those under the poverty line than those above it. 


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

  
  Below, we observe the relationship between Gini (economic inequality, where 0 is perfect equality and 1 is where one person has all the income). 


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

  
  
  
  
  https://themalaysianreserve.com/2025/10/13/the-curious-case-of-a-dwindling-development-budget/
  
  
  
Irrespective of this, grievances persist,

That the government would rather embrace further malapportionment than allow for greater federalism is telling

Perhaps one should look to politicians 






ballots |> 
  filter(election == "GE-15" & federal == "Federal" & 
           str_detect(state, "Labuan|Sabah|Sarawk")) |> 
  filter(result == "won") |> 
  arrange(votes)



Federalism 
Neglect from the federal government 
Not enough autonomy or economic development 

median-age-candidates

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


census_dun |> 
  mutate(population_poor = poverty_incidence / 100 * population_total, 
         east_malaysia = ifelse(
           str_detect(state, "Labuan|Sabah|Sarawak"), 
           "East Malaysia", 
           "Peninsula"
         )) |> 
  group_by(
    state, parlimen, east_malaysia
  ) |> 
  summarise(population_total = sum(population_total), 
            nationality_non_citizen = sum(nationality_non_citizen),
            population_poor = sum(population_poor),
            .groups = "drop") |> 
  mutate(
    non_citizen_pc = nationality_non_citizen / population_total, 
    poverty_incidence = population_poor / population_total
  ) |> 
  mutate(label = ifelse(
    non_citizen_pc > .15 | poverty_incidence > .15, 
    str_sub(parlimen, start = 6L), 
    ""
  )) |> 
  arrange(desc(poverty_incidence)) |> 
  ggplot(aes(x = non_citizen_pc, 
             y = poverty_incidence)) + 
  scale_x_continuous(labels = percent) + 
  scale_y_continuous(labels = percent) +
  geom_point(aes(colour = east_malaysia, 
                 size = population_total), 
             alpha = .7) + 
  scale_colour_manual(values = c("#414487ff", "#7ad151ff")) +
  scale_size_continuous(labels = comma) +
  geom_text(aes(label = label), 
            size = 2, 
            check_overlap = FALSE) +  
  labs(colour = "East Malaysia", 
       x = "% Non-citizens", 
       y = "Poverty incidence", 
       size = "Total population", 
       title = "The areas with the highest levels of poverty tend to be in East Malaysia") + 
  guides(
    size = guide_legend(override.aes = list(alpha = 1)), 
    colour = guide_legend(override.aes = list(alpha = 1, 
                                              size = 3))
  )
multiracial |> 
  mutate(count = 1) |> 
  filter(federal == "Federal") |> 
  summarise_at(
    vars(malay_only, chinese_only, indian_only, 
         sabah_only, sarawak_only, other_only, 
         multiracial, count), 
    ~ sum(.x, na.rm = TRUE)
  ) |> 
  pivot_longer(!count) |> 
  mutate(pc = value / count) |> 
  mutate(name = str_to_title(str_replace_all(name, "_", " "))) |> 
  ggplot(aes(x = pc, y = fct_reorder(name, pc))) + 
  geom_col(fill = "#e9d8a6") + 
  geom_text(aes(label = comma(value)), 
            hjust = "inward") +
  scale_x_continuous(labels = percent) + 
  labs(
    x = "% of federal elections", 
    y = "", 
    title = "Ethnicity of candidates in federal elections"
  )

multiracial <- ballots |> 
  mutate(ethnicity = ifelse(ethnicity == "Orang Asli", "Other", ethnicity)) |> 
  mutate(count = 1) |> 
  # I think this filter is fine
  filter(rank <= 3) |> 
  group_by(state, seat, date, federal, election) |> 
  summarise(malay_candidates = sum(count[ethnicity == "Malay"]), 
            chinese_candidates = sum(count[ethnicity == "Chinese"]), 
            indian_candidates = sum(count[ethnicity == "Indian"]), 
            sabah_candidates = sum(count[ethnicity == "Bumi Sabah"]), 
            sarawak_candidates = sum(count[ethnicity == "Bumi Sarawak"]), 
            other_candidates = sum(count[ethnicity == "Other"]), 
            .groups = "drop") |> 
  mutate(malay_chinese = ifelse(malay_candidates > 0 & chinese_candidates > 0, 1, 0), 
         malay_indian = ifelse(malay_candidates > 0 & indian_candidates > 0, 1, 0), 
         chinese_indian = ifelse(chinese_candidates > 0 & indian_candidates > 0, 1, 0), 
         malay_sabah = ifelse(malay_candidates > 0 & sabah_candidates > 0, 1, 0), 
         malay_sarawak = ifelse(malay_candidates > 0 & sarawak_candidates > 0, 1, 0), 
         chinese_sabah = ifelse(chinese_candidates > 0 & sabah_candidates > 0, 1, 0), 
         chinese_sarawak = ifelse(chinese_candidates > 0  & sarawak_candidates > 0, 1, 0), 
         indian_sabah = ifelse(indian_candidates > 0 & sabah_candidates > 0, 1, 0), 
         indian_sarawak = ifelse(indian_candidates > 0 & sarawak_candidates > 0, 1, 0), 
         malay_other = ifelse(malay_candidates > 0 & other_candidates > 0, 1, 0), 
         chinese_other = ifelse(chinese_candidates > 0 & other_candidates > 0, 1, 0), 
         indian_other = ifelse(indian_candidates > 0 & other_candidates > 0, 1, 0), 
         sabah_other = ifelse(sabah_candidates > 0 & other_candidates > 0, 1, 0), 
         sarawak_other = ifelse(sarawak_candidates > 0 & other_candidates > 0, 1, 0), 
         
         malay_chinese_indian = ifelse(malay_candidates > 0 & chinese_candidates > 0 & 
                                         indian_candidates > 0, 1, 0), 
         malay_chinese_other = ifelse(malay_candidates > 0 & chinese_candidates > 0 & 
                                        other_candidates > 0, 1, 0), 
         malay_chinese_sabah = ifelse(malay_candidates > 0 & chinese_candidates > 0 & 
                                        sabah_candidates > 0, 1, 0), 
         malay_chinese_sarawak = ifelse(malay_candidates > 0 & chinese_candidates > 0 & 
                                          sarawak_candidates > 0, 1, 0), 
         chinese_sabah_sarawak = ifelse(chinese_candidates > 0 & sabah_candidates > 0 & 
                                          sarawak_candidates > 0, 1, 0), 
         sabah_sarawak_other = ifelse(sabah_candidates > 0 & sarawak_candidates > 0 & 
                                        other_candidates > 0, 1, 0),
         
         malay_only = ifelse(malay_candidates > 0 & chinese_candidates == 0 & 
                               indian_candidates == 0 & sabah_candidates == 0 & 
                               sarawak_candidates == 0 & other_candidates == 0, 
                             1, 0), 
         chinese_only = ifelse(malay_candidates == 0 & chinese_candidates > 0 & 
                                 indian_candidates == 0 & sabah_candidates == 0 & 
                                 sarawak_candidates == 0 & other_candidates == 0, 
                               1, 0),
         indian_only = ifelse(malay_candidates == 0 & chinese_candidates == 0 & 
                                indian_candidates > 0 & sabah_candidates == 0 & 
                                sarawak_candidates == 0 & other_candidates == 0, 
                              1, 0),
         sabah_only = ifelse(malay_candidates == 0 & chinese_candidates == 0 & 
                               indian_candidates == 0 & sabah_candidates > 0 & 
                               sarawak_candidates == 0 & other_candidates == 0, 
                             1, 0),
         sarawak_only = ifelse(malay_candidates == 0 & chinese_candidates == 0 & 
                                 indian_candidates == 0 & sabah_candidates == 0 & 
                                 sarawak_candidates > 0 & other_candidates == 0, 
                               1, 0),
         other_only = ifelse(malay_candidates == 0 & chinese_candidates == 0 & 
                               indian_candidates == 0 & sabah_candidates == 0 & 
                               sarawak_candidates == 0 & other_candidates > 0, 
                             1, 0)
  ) |> 
  select(state, seat, date, federal, election, malay_chinese:other_only) |> 
  mutate(multiracial = ifelse(malay_only == 1 | chinese_only == 1 | indian_only == 1 |
                                sabah_only == 1 | sarawak_only == 1 | other_only == 1,
                              0, 1))  

ballots |> 
  mutate(ethnicity = ifelse(ethnicity == "Orang Asli", "Other", ethnicity)) |> 
  mutate(count = 1) |> 
  # I think this filter is fine
  filter(rank <= 3) |> 
  group_by(state, seat, date, federal, election) |> 
  summarise(malay_candidates = sum(count[ethnicity == "Malay"]), 
            chinese_candidates = sum(count[ethnicity == "Chinese"]), 
            indian_candidates = sum(count[ethnicity == "Indian"]), 
            sabah_candidates = sum(count[ethnicity == "Bumi Sabah"]), 
            sarawak_candidates = sum(count[ethnicity == "Bumi Sarawak"]), 
            other_candidates = sum(count[ethnicity == "Other"]), 
            .groups = "drop") |> 
  mutate(total_candidates = malay_candidates + chinese_candidates + indian_candidates + 
           sabah_candidates + sarawak_candidates + other_candidates) |> 
  pivot_longer(cols = malay_candidates:other_candidates, 
               names_to = "candidate_ethnicity", 
               values_to = "value") |> 
  mutate(candidate_ethnicity = str_remove_all(candidate_ethnicity, "_candidates")) |> 
  filter(value != 0) |>
  mutate(multiracial = ifelse(value != total_candidates, 1, 0)) |> 
  mutate(combination = ifelse(
    multiracial == 0, paste0(candidate_ethnicity, "_only"), "multiethnic"
  ))


PKR upholds its commitment to multiracialism, at least in the candidates that it has fielded. Additionally, the demographics of the seats it governs [largely mirrors](https://www.iseas.edu.sg/articles-commentaries/iseas-perspective/2024-21-a-deep-dive-into-malaysias-peoples-justice-party-pkr-by-james-chai/) the demographics of peninsula Malaysia.

ballots |>  
  filter(str_detect(election, "GE")) |>
  mutate(year = year(date)) |> 
  group_by(election) |> 
  mutate(year = min(year)) |>
  mutate(coalition = fct_lump(coalition, n = 7, w = votes), 
         coalition = forcats::fct_drop(coalition)) |>
  group_by(year, coalition) |> 
  summarise(votes = sum(votes), .groups = "drop") |>
  group_by(year) |> 
  mutate(total_votes = sum(votes)) |> 
  ungroup() |> 
  mutate(votes_pc = votes / total_votes) %>% 
  ggplot(aes(x = year, y = votes_pc, group = coalition)) + 
  geom_line(aes(colour = coalition), 
            linewidth = 1.05, 
            alpha = .8) + 
  scale_colour_viridis_d(option = "turbo", drop = TRUE, limits = force) +
  scale_x_continuous(breaks = seq(1955, 2026, 5)) +
  guides(colour = guide_legend(override.aes = list(linewidth = 2, 
                                                   alpha = 1))) + 
  scale_y_continuous(labels = percent, breaks = seq(0, .9, .1)) +
  labs(x = "", 
       y = "Share of popular vote", 
       title = "UMNO still has, very marginally, the largest share of votes",
       subtitle = "But it has never won more than 40% of votes since the formation of the Federation", 
       colour = "") + 
  theme(axis.text.x = element_text(size = 7))


ballots |> 
  mutate(count = 1) |> 
  filter(result %in% c("won", "won_uncontested")) |> 
  filter(str_detect(election, "SE")) |> 
  mutate(year = year(date)) |> 
  group_by(election) |> 
  mutate(year = min(year)) |> 
  ungroup() |> 
  mutate(party = fct_lump(party, n = 12, w = votes)) |> 
  group_by(year, party) |> 
  summarise(mps = sum(count), 
            .groups = "drop") |> 
  ggplot(aes(x = year, y = mps, group = party)) + 
  geom_line(aes(colour = party), 
            linewidth = 1.05, 
            alpha = .8) + 
  # scale_colour_manual(
  #   values = c(
  #     "UMNO" = "#30123BFF",
  #     "MCA" = "#4454C4FF",
  #     "MIC" = "#4490FEFF",
  #     "BERSATU" = "#1FC8DEFF",
  #     "PAS" = "#29EFA2FF",
  #     "GERAKAN" = "#7DFF56FF",
  #     "PBB" = "#C1F334FF",
  #     "PBS" = "#F1CA3AFF",
  #     "USNO" = "#FE922AFF",
  #     "SUPP" = "#EA4F0DFF",
  #     "PKR" = "#BE2102FF",
  #     "DAP" = "#7A0403FF",
  #     "Other" = "grey50"
  #     
  #   )
  # ) +
  scale_colour_viridis_d(option = "turbo") +
  scale_x_continuous(breaks = seq(1955, 2026, 5)) +
  scale_y_continuous(breaks = seq(0, 300, 20)) +
  guides(colour = guide_legend(override.aes = list(linewidth = 2, 
                                                   alpha = 1))) + 
  labs(x = "", 
       y = "Number of MPs", 
       title = "The collapse of UMNO has left PAS and DAP as the largest parties", 
       colour = "") + 
  theme(axis.text.x = element_text(size = 7))

[Marzuki Mohamad and Ibrahim Suffian](https://www.iseas.edu.sg/articles-commentaries/iseas-perspective/2023-20-malaysias-15th-general-election-ethnicity-remains-the-key-factor-in-voter-preferences-by-marzuki-mohamad-and-ibrahim-suffian/) argue that ethnicity remains the most important political and social cleavage, citing a stark dichotomy in how the Perikatan Nasional administration was perceived by Malays and non-Malays.  

census_fed |> 
  left_join(
    multiracial |> 
      filter(election %in% c("GE-12", "GE-13", "GE-14", "GE-15")) |> 
      mutate(type = case_when(
        multiracial == 1 ~ "Multiethnic", 
        malay_only == 1 ~ "Malay only", 
        chinese_only == 1 ~ "Chinese only", 
        indian_only == 1 ~ "Indian only", 
        sabah_only == 1 ~ "Bumi Sabah only", 
        sarawak_only == 1 ~ "Bumi Sarawak only"
      )) |> 
      select(seat, type, election), 
    by = c("parlimen" = "seat")
  ) |> 
  mutate(population_bumi = population_total * pc_bumi) |> 
  ggplot(aes(x = population_total, 
             y = pc_bumi)) + 
  geom_point(aes(colour = type, 
                 size = population_bumi)) +
  scale_x_log10(labels = comma) + 
  scale_y_continuous(labels = percent) +
  scale_colour_viridis_d() +
  scale_size_continuous(labels = comma) +
  labs(x = "Population of constituency", 
       y = "% of population who are Bumiputera", 
       colour = "Ethnicity of\ncandidates", 
       size = "2020 Bumiputera\npopulation") +
  guides(colour = guide_legend(
    override.aes = list(size = 2)
  )) + 
  facet_wrap(~ election)

ballots |> 
  filter(state %in% c("Sabah", "Sarawak")) |> 
  mutate(ethnicity = ifelse(
    ethnicity == "Orang Asli", "Other", ethnicity
  )) |> 
  mutate(year = year(date), 
         decade = year - year %% 10, 
         count = 1) |> 
  group_by(state, decade, ethnicity, federal) |> 
  summarise(count = sum(count)) |> 
  group_by(decade, federal) |> 
  mutate(total = sum(count)) |> 
  ungroup() |> 
  mutate(pc = count / total) |> 
  ggplot(aes(x = decade, 
             y = pc, 
             fill = ethnicity)) + 
  geom_col(position = "stack") + 
  facet_wrap(~ federal + state) + 
  scale_y_continuous(labels = percent) + 
  scale_x_continuous(breaks = seq(1950, 2020, 10)) +
  scale_fill_viridis_d() + 
  theme(axis.text.x = element_text(angle = 30, vjust = .5), 
        strip.background = element_rect(fill = "black")) + 
  labs(title = "Ethnicity of election candidates", 
       y = "% of candidates", 
       x = "") 

ballots |> 
  filter(date > "2006-01-01") |> 
  mutate(count = 1) |> 
  filter(rank <= 3) |> 
  mutate(party = fct_lump(party, n = 12)) |> 
  group_by(party, sex) |> 
  summarise(count = sum(count), .groups = "drop") |> 
  group_by(party) |> 
  mutate(total_candidates = sum(count)) |> 
  ungroup() |> 
  mutate(pc = count / total_candidates, 
         sex = ifelse(sex == "F", "Female", "Male")) |> 
  ggplot(aes(x = pc, y = fct_rev(party))) + 
  geom_col(aes(fill = sex)) + 
  geom_vline(xintercept = .25, linetype = "longdash", colour = "white", alpha = .4) +
  geom_vline(xintercept = .5, linetype = "longdash", colour = "white", alpha = .4) +
  geom_vline(xintercept = .75, linetype = "longdash", colour = "white", alpha = .4) +
  scale_fill_viridis_d() + 
  scale_x_continuous(label = percent) + 
  labs(x = "% of candidates", 
       y = "Party", 
       title = "Only 10.5% of candidates since 2006 have been women", 
       subtitle = "Only showing politicians who ran since the redelineation in 2006", 
       fill = "Sex")

census_fed |> 
  left_join(
    multiracial |> 
      filter(election == "GE-15") |> 
      mutate(type = case_when(
        multiracial == 1 ~ "Multiethnic", 
        malay_only == 1 ~ "Malay only", 
        chinese_only == 1 ~ "Chinese only", 
        indian_only == 1 ~ "Indian only", 
        sabah_only == 1 ~ "Bumi Sabah only", 
        sarawak_only == 1 ~ "Bumi Sarawak only"
      )) |> 
      select(seat, type), 
    by = c("parlimen" = "seat")
  ) |> 
  mutate(population_bumi = population_total * pc_bumi) |> 
  ggplot(aes(x = population_density, 
             y = pc_bumi)) + 
  geom_point(aes(colour = type, 
                 size = population_total)) +
  scale_x_log10(labels = comma) +
  scale_y_continuous(labels = percent) +
  scale_colour_viridis_d() +
  scale_size_continuous(labels = comma) +
  labs(x = "Population density", 
       y = "% of population who are Bumiputera", 
       colour = "Ethnicity of\ncandidates", 
       size = "Total\npopulation") +
  guides(colour = guide_legend(
    override.aes = list(size = 3)))
    
multiracial |> 
  filter(multiracial == 1) |> 
  left_join(
    ballots |> 
      filter(result %in% c("won", "won_uncontested")) |> 
      select(seat, date, winner_ethnicity = ethnicity), 
    by = c("seat", "date")
  ) |> 
  mutate(year = year(date), 
         decade = year - year %% 10) |> 
  select(-c(malay_only:other_only)) |> 
  pivot_longer(
    cols = malay_chinese:sabah_sarawak_other, 
    names_to = "combination", 
    values_to = "value"
  ) |> 
  filter(value > 0) |> 
  mutate(
    combination = ifelse(
      combination %in% c(
        "malay_chinese_other", 
        "malay_sarawak", 
        "indian_other", 
        "sabah_other", 
        "sarawak_other", 
        "malay_chinese_other", 
        "malay_chinese_sabah", 
        "malay_chinese_sarawak"
      ), 
      "other_combination", 
      combination
    )) |> 
  group_by(combination) |> 
  mutate(sort = sum(value)) |> 
  ungroup() |> 
  mutate(combination = str_replace_all(combination, "_", "/"), 
         combination = str_to_title(combination)) |> 
  ggplot(aes(
    x = value, 
    y = fct_reorder(combination, sort), 
    fill = winner_ethnicity
  )) + 
  geom_col() +
  scale_fill_viridis_d() +
  labs(title = "Multiethnic electoral races and their results",
       x = "Number of electoral races", 
       y = "", 
       fill = "Winner ethnicity") +
  facet_wrap(~ federal, scales = "free_x") + 
  theme(strip.background = element_rect(fill = "grey20"))

multiracial |> 
  filter(multiracial == 1) |> 
  left_join(
    ballots |> 
      filter(result %in% c("won", "won_uncontested")) |> 
      select(seat, date, winner_ethnicity = ethnicity), 
    by = c("seat", "date")
  ) |> 
  mutate(year = year(date), 
         decade = year - year %% 10) |> 
  select(-c(malay_only:other_only)) |> 
  pivot_longer(
    cols = malay_chinese:sabah_sarawak_other, 
    names_to = "combination", 
    values_to = "value"
  ) |> 
  mutate(winner_ethnicity = ifelse(
    winner_ethnicity == "Orang Asli", "Other", winner_ethnicity
  )) |> 
  ggplot(aes(x = decade, 
             y = value, 
             fill = winner_ethnicity)) + 
  geom_col() + 
  scale_fill_viridis_d() + 
  scale_x_continuous(breaks = seq(1950, 2020, 10)) + 
  facet_wrap(~ federal, scales = "free_y") + 
  labs(
    title = "Results of multiethnic electoral races"
  )

multiracial |>
  mutate(year = year(date), decade = year - year %% 10) |>
  group_by(decade, federal) |>
  summarise(
    n = n(),
    multiethnic = sum(multiracial),
    .groups = "drop"
  )  |>
  mutate(
    single_race = n - multiethnic,
    multiethnic_pc = multiethnic / n,
    single_race_pc = single_race / n
  ) |>
  select(decade, federal, n, multiethnic_pc, single_race_pc) |>
  pivot_longer(cols = multiethnic_pc:single_race_pc) |>
  ggplot(aes(x = decade, y = value, fill = name)) +
  geom_col() +
  scale_x_continuous(breaks = seq(1950, 2020, 10)) +
  facet_wrap(~ federal, scales = "free_y") +
  scale_fill_viridis_d(option = "cividis",
                           begin = .2,
                           end = .9)
  
ballots |> 
  filter(result %in% c("won", "won_uncontested") & 
           coalition %in% c("PH", "PR")) |> 
  mutate(year = year(date)) |> 
  ggplot(aes(x = votes_perc / 100, group = year, fill = -year)) + 
  geom_histogram() + 
  scale_x_continuous(labels = percent) + 
  facet_wrap(~ federal)

ballots |> 
  mutate(seat_date = paste0(seat, " ", date),
         votes_perc = ifelse(is.na(votes_perc) & result == "won_uncontested", 100, votes_perc)) |> 
  group_by(seat_date) |> 
  mutate(ph_pr = ifelse(coalition %in% c("PH", "PR"), 1, 0),
         include_sum = sum(ph_pr), 
         result = case_when(
           result == "won_uncontested" ~ "won", 
           result == "lost_deposit" ~ "lost", 
           TRUE ~ result
         )) |> 
  ungroup() |> 
  filter(include_sum > 0) |> 
  mutate(coalition = ifelse(
    coalition %in% c("USA", "ALONE", "GTA", "WARISAN-PLUS", "GASAK", "GRS"), "Other", coalition)) |> 
  filter(coalition != "Other") |> 
  ggplot(aes(x = reorder_within(seat_date, votes_perc, coalition), y = votes_perc)) + 
  geom_col(aes(fill = result), width = 1) + 
  geom_hline(yintercept = 50, linetype = 2, colour = "cornflowerblue", alpha = .5) +
  facet_wrap(~ coalition, scales = "free_x") + 
  scale_x_reordered() + 
  scale_fill_viridis_d(option = "turbo") + 
  theme(axis.text.x = element_blank(), 
        strip.background = element_rect(fill = "grey20")) + 
  labs(title = "Electoral result by vote share and coalition", 
       y = "Vote share", 
       x = "Electoral race")




Be that as it may, a closer look at BN and PN leaders’ approval rating among Malay voters reveals a slightly different scenario. PN Chairman and former Prime Minister, Muhyiddin Yassin, received a higher approval rating (67%) compared to BN Chairman, Zahid Hamidi (19%).

PKR has backtracked on its commitment to one-person-one-vote in its [internal elections](https://www.iseas.edu.sg/articles-commentaries/iseas-perspective/2024-21-a-deep-dive-into-malaysias-peoples-justice-party-pkr-by-james-chai/): executive committees at the national and division levels are now chosen by delegates. 

Efforts have been made in recent years to ease this burden. First, PKR adopted a hybrid selection system for the first time in 2022,[69] where the executive committees at the national and division levels are chosen by delegates (see Table 6 above). This halves the maximum votes available to the members, which had proven to be too many for members in the first place (‘over-democracy’).[70] Two, introducing online voting (via the ‘ADIL’ application) that will minimise logistics and potential cheating. In 2022, more

ballots |> 
  filter(date > "2006-01-01") |> 
  filter(result %in% c("won", "won_uncontested")) |> 
  mutate(count = 1) |>
  mutate(ethnicity = ifelse(ethnicity == "Orang Asli", "Other", ethnicity)) |> 
  mutate(party = fct_lump(party, n = 12)) |> 
  group_by(party, ethnicity) |> 
  summarise(count = sum(count), .groups = "drop") |> 
  group_by(party) |> 
  mutate(total_candidates = sum(count)) |> 
  ungroup() |> 
  mutate(pc = count / total_candidates) |> 
  ggplot(aes(x = pc, y = fct_rev(fct_relevel(party, 
                                             c("UMNO", "DAP", "PAS", "PKR", "PBB", 
                                               "Other", "BERSATU", "WARISAN", "MCA", 
                                               "AMANAH", "PRS", "PBS", "SUPP"))))) + 
  geom_col(aes(fill = ethnicity)) + 
  geom_vline(xintercept = .25, linetype = "longdash", colour = "white", alpha = .4) +
  geom_vline(xintercept = .5, linetype = "longdash", colour = "white", alpha = .4) +
  geom_vline(xintercept = .75, linetype = "longdash", colour = "white", alpha = .4) +
  scale_fill_viridis_d() + 
  scale_x_continuous(label = percent) + 
  labs(x = "% of MPs", 
       y = "Party", 
       title = "Ethnicity of winning candidates by party", 
       subtitle = "Since 2006; parties sorted by number of MPs", 
       fill = "Candidate\nethnicity")
        