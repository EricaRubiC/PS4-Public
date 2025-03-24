# COVID REPORT
# Evil data team
# 03.23.2022

# Here's what we could manage. Sorry. You do the rest.

# packages
  library(tidyverse)
  library(scales)
  library(lfe)
  library(modelsummary)
  library(gt)
  library(data.table)
  library(scales)


# create the dataset ------------------
## 2021-2022 deaths (BIG FILES) *This reports each county's new deaths between 2021-2022*
  covid <-
    data.table::fread('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties-2022.csv') %>%
    filter(!is.na(fips),
           !state %in% c('Puerto Rico', 'Hawaii', 'Northern Mariana Islands', 'Virgin Islands')) %>%
    select(fips, county, state, date, deaths) %>%
    group_by(fips, county, state) %>%
    summarise(deaths = max(deaths, na.rm = T) - min(deaths, na.rm = T), .groups = "drop")

## estimated mask usage from July 2020 survey 
  mask <-
    read_csv('https://raw.githubusercontent.com/nytimes/covid-19-data/master/mask-use/mask-use-by-county.csv') %>%
    mutate(
      fips = as.integer(COUNTYFP),
      always.mask = ALWAYS, #always masking *Not sure if this is correct*
      .keep = 'none'
    ) # for merging   

## prep CDC data from directory
  vax <-
    read_csv('cdc vax mar1.csv') %>%
    filter( # drop unknown/incomplete/questionable reports
      FIPS != 'UNK', 
      Recip_State != 'VI', 
      Completeness_pct > 0, 
      !is.na(Administered_Dose1_Recip)
    ) %>% 
    mutate(
      fips = as.integer(FIPS), 
      population = Census2019,
      vax.complete = Series_Complete_Pop_Pct, # percent vaxd
      svi.index = SVI_CTGY, # social vulnerability index
      .keep = 'none'
    )  

## merge  
  covid <-
    left_join(covid, mask) %>%
    left_join(vax) %>%
    #mutate(deaths.scaled = deaths / population * 100000) %>% *Not doing this anymore*
    ungroup() # scale by population
  
  rm(mask, vax)
  
  summary(covid)
# *The covid dataset now contains:County-level cumulative COVID deaths (adjusted for population size), Mask-wearing estimates (always.mask), Vaccination rates (vax.complete), Social Vulnerability Index (svi.index)*
  
  
# Plot 1 
  
# COVID deaths nationally ---------- 
# This creates a histogram of COVID-19 deaths by county across the U.S., using data in the covid dataframe
  
#Checking
  
new_covid <-  covid %>%
    group_by(state) %>% # Instead I am using states only as county names are shared by several states
    summarise(total_deaths = sum(deaths, na.rm = TRUE),  # Adding all new deaths in each state
              total_population = sum(population, na.rm = TRUE)) %>% 
    mutate(deaths_per_200k = (total_deaths / total_population) * 200,000) %>% # Comparison with other state populations
    arrange(desc(deaths_per_200k)) 
  
  check <- covid %>%
    select(state, population, deaths) %>%
    group_by(state) %>%
    summarise(total_population = sum(population, na.rm = TRUE)) %>%
    arrange(desc(total_population))
  
Check1 <-  covid %>%
  filter(!is.na(population)) %>%
  group_by(state) %>%
  summarise(total_population = sum(population)) %>%
  arrange(desc(total_population))

#Figure 1
  
  new_covid <- covid %>%
    group_by(state) %>%  # Instead I am using states only as county names are shared by several states
    summarise(
      total_deaths = sum(deaths, na.rm = TRUE),  # Adding all new deaths in each state
      total_population = sum(population, na.rm = TRUE)
    ) %>%
    mutate(deaths_per_200k = (total_deaths / total_population) * 100000) %>%  # Comparison with other state populations
    arrange(desc(deaths_per_200k)) %>%
    mutate(color_group = factor(row_number() %% 2))  # 
  
  new_covid %>%
    ggplot(aes(x = reorder(state, deaths_per_200k), y = deaths_per_200k)) +
    geom_col(aes(fill = color_group)) +  #Making the shift in colors
    coord_flip() +
    scale_y_continuous(expand = c(0, 0)) +
    scale_fill_manual(values = c("1" = "lightblue", "0" = "lightgray")) +  # alternating colors
    labs(
      title = 'Figure 1. Rates of New COVID-19 Deaths by State (2021-2022)',
      x = 'State',
      y = 'Deaths Per 200K Residents of Total Population' 
    ) +
    theme_bw(base_size = 9) +
    theme(
      plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
      plot.caption = element_text(
        family = 'Times New Roman', 
        hjust = 0.5, 
        size = 15,
        face = "bold"
      ),
      axis.text = element_text(size = 6, face = 'bold'),
      #axis.title = element_text(size = 10, face = 'bold'), 
      panel.grid = element_blank(), 
      panel.background = element_blank(),
      panel.border = element_rect(colour = "black", size = 1) ,
      legend.position = "none" 
    )
  
## stat summary and more
  summary(new_covid$deaths_per_200k)
  summary(new_covid$total_deaths)
  
  # find some examples? #


# Mask usage ----------------------- #Make this better
## VIZ: "Always wears a mask"
  
# Figure 2

summary(covid$always.mask)
  covid %>%
    group_by(state) %>%
    summarise(
      mask_wearers = sum(always.mask, na.rm = TRUE), # Group by state and percent of mask wearers in each state
      total_population = sum(population, na.rm = TRUE) 
    ) %>%
    mutate(
      mask_percentage = (mask_wearers / total_population) * 100  # percent of mask wearers
    ) %>%
    arrange(desc(mask_percentage)) %>%
    ggplot(aes(x = reorder(state, mask_percentage), y = mask_percentage)) +
    scale_y_continuous(expand = c(0, 0)) +
    geom_col(fill = "lightblue") +  # 
    coord_flip() +
    labs(
      title = "Mask Usage by State (July 2022)",
      x = "State",
      y = "% of People Wearing Masks"
    ) + 
    theme_bw(base_size = 9) +
    theme(
      plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
      plot.caption = element_text(
        family = 'Times New Roman', 
        hjust = 0.5, 
        size = 15,
        face = "bold"
      ),
      axis.text = element_text(size = 6, face = 'bold'),  
      #axis.title = element_text(size = 10, face = 'bold'),  
      panel.grid = element_blank(), 
      panel.background = element_blank(),
      panel.border = element_rect(colour = "black", size = 1) ,
      legend.position = "none" 
    )
  
   
tab1<- table(covid$always.mask)
## helpers
  summary(covid$always.mask)
  # find hi/lo counties?


# Rates of vaccination -------------

## VIZ: overall vax rates
  covid %>%
    ggplot(aes(x = vax.complete)) +
    geom_histogram(color = 'gray40', fill = 'lightblue') +
    labs(
      title = "Vaccination Rates in the United Staes",
      x = "Counties in the U.S.",
      y = "Number of People Vaccinated"
    ) + 
    theme_bw(base_size = 9) +
    theme(
      plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
      plot.caption = element_text(
        family = 'Times New Roman', 
        hjust = 0.5, 
        size = 15,
        face = "bold"
      ),
      axis.text = element_text(size = 6, face = 'bold'),  
      panel.grid = element_blank(), 
      panel.background = element_blank(),
      panel.border = element_rect(colour = "black", size = 1) ,
      legend.position = "none" 
    ) +  
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0))

## VIZ: vax rates by Social Vulnerability Index category
  covid %>%
    filter(!is.na(svi.index)) %>% 
    ggplot(aes(y = vax.complete, x = svi.index, color = svi.index)) +
    geom_boxplot()+
    labs(
      title = "Vaccination Rates Among Different Social Vulnerability Indices",
      x = "Social Vulnerability Index (SVI) Categories",
      y = "Vaccination Completion Rate (%)",
      fill = "SVI Category"
    ) +
    theme_bw(base_size = 12)
    theme(
      plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
      axis.text = element_text(size = 6, face = "bold"),
      axis.title = element_text(size = 12),
      panel.grid = element_blank(),
      panel.border = element_rect(colour = "black", size = 1),
      legend.position = "none"
    )
  
## find high/low counties
    covid %>%
      select(State = state, County = county, Vaccination_Rate = vax.complete) %>%
      arrange(desc(Vaccination_Rate)) %>%
      slice_head(n = 5) %>%
      bind_rows(
        covid %>%
          select(State = state, County = county, Vaccination_Rate = vax.complete) %>%
          arrange(Vaccination_Rate) %>%
          slice_head(n = 5)
      ) %>%
      knitr::kable(digits = 2, col.names = c("State", "County", "Vaccination Rate"))

# Impact on 2022 COVID deaths ------
## regression estimates
  mods <- 
    list(
      m1 = felm(deaths ~ always.mask + population + svi.index | state, data = covid),
      m2 = felm(deaths ~ vax.complete + population + svi.index | state, data = covid),
      m3 = felm(deaths ~ always.mask + vax.complete + svi.index | state, data = covid)
    )

## regression table

    modelsummary(
      mods,  
      gof_map = c('nobs'),  
      stars = TRUE,  
      coef_map = c(
        "always.mask" = "Mask-Wearing Rate",
        "vax.complete" = "Vaccination Rate",
        "population" = "Population Size",
        "svi.index" = "Social Vulnerability Index"
      ),  
      output = 'gt',  
      title = "Impact of Mask-Wearing and Vaccination on COVID-19 Deaths (2022)"
    )
