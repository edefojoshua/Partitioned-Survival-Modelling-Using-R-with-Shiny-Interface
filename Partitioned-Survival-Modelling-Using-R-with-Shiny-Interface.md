httpsPartitioned Survival Modelling Using R with Shiny Interface.
================
Joshua Edefo
2024-11-24

Load Necessary Libraries

``` r
# Load Necessary Libraries
library(survival)
```

    ## Warning: package 'survival' was built under R version 4.3.3

``` r
library(ggplot2)
```

    ## Warning: package 'ggplot2' was built under R version 4.3.3

``` r
library(dplyr)
```

    ## Warning: package 'dplyr' was built under R version 4.3.3

``` r
library(shiny)
```

    ## Warning: package 'shiny' was built under R version 4.3.3

``` r
library(shinythemes)
```

    ## Warning: package 'shinythemes' was built under R version 4.3.3

``` r
library(writexl)
```

    ## Warning: package 'writexl' was built under R version 4.3.3

``` r
library(DT)
```

    ## Warning: package 'DT' was built under R version 4.3.3

``` r
# Simulate Survival Data
set.seed(123)
time <- seq(0, 36, by = 1) # Time in months

# Function to generate survival data based on selected distribution
generate_survival_data <- function(distribution) {
  if (distribution == "exponential") {
    surv_os <- exp(-0.05 * time) # OS: Exponential survival curve
    surv_pfs <- exp(-0.08 * time) # PFS: Exponential survival curve
  } else if (distribution == "weibull") {
    surv_os <- 1 - pweibull(time, shape = 1.5, scale = 10) # Weibull survival curve
    surv_pfs <- 1 - pweibull(time, shape = 1.3, scale = 8) # Weibull survival curve
  } else if (distribution == "lognormal") {
    surv_os <- 1 - plnorm(time, meanlog = 0, sdlog = 0.5) # Lognormal survival curve
    surv_pfs <- 1 - plnorm(time, meanlog = 0, sdlog = 0.7) # Lognormal survival curve
  } else {
    stop("Unknown distribution")
  }
  return(list(surv_os = surv_os, surv_pfs = surv_pfs))
}

# Health States in the Model
survival_data <- function(distribution) {
  data <- generate_survival_data(distribution)
  survival_data <- data.frame(
    time = time,
    surv_os = data$surv_os,
    surv_pfs = data$surv_pfs
  ) %>%
    mutate(
      progression_free = surv_pfs,
      progressed = surv_os - surv_pfs,
      dead = 1 - surv_os
    )
  return(survival_data)
}

# Cost-Effectiveness Inputs: Define costs and utilities for each health state
costs_intervention <- data.frame(
  state = c("Progression-Free", "Progressed", "Death"),
  cost = c(3000, 5000, 0) # CAR-T therapy cost per month
)

utilities_intervention <- data.frame(
  state = c("Progression-Free", "Progressed", "Death"),
  utility = c(0.8, 0.6, 0) # CAR-T therapy utility values
)

costs_comparator <- data.frame(
  state = c("Progression-Free", "Progressed", "Death"),
  cost = c(2500, 4500, 0) # Clofarabine cost per month
)

utilities_comparator <- data.frame(
  state = c("Progression-Free", "Progressed", "Death"),
  utility = c(0.75, 0.55, 0) # Clofarabine utility values
)

# Function to calculate PSA (Partitioned Survival Analysis)
calculate_psa <- function(survival_data, costs, utilities) {
  survival_data %>%
    rowwise() %>%
    mutate(
      cost = progression_free * costs$cost[1] +
        progressed * costs$cost[2] +
        dead * costs$cost[3],
      qalys = progression_free * utilities$utility[1] +
        progressed * utilities$utility[2] +
        dead * utilities$utility[3]
    ) %>%
    ungroup()
}

# Define UI
ui <- fluidPage(
  theme = shinytheme("cosmo"),
  titlePanel("Cost-Effectiveness Analysis: CAR-T vs Clofarabine"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("time_horizon", "Time Horizon (Months)", min = 1, max = 36, value = 12),
      selectInput("surv_dist", "Select Survival Distribution", 
                  choices = c("Exponential", "Weibull", "Lognormal")),
      numericInput("cost_pf_intervention", "Cost: Progression-Free (CAR-T)", value = 3000, min = 0),
      numericInput("cost_pd_intervention", "Cost: Progressed Disease (CAR-T)", value = 5000, min = 0),
      numericInput("utility_pf_intervention", "Utility: Progression-Free (CAR-T)", value = 0.8, min = 0, max = 1, step = 0.1),
      numericInput("utility_pd_intervention", "Utility: Progressed Disease (CAR-T)", value = 0.6, min = 0, max = 1, step = 0.1),
      numericInput("cost_pf_comparator", "Cost: Progression-Free (Clofarabine)", value = 2500, min = 0),
      numericInput("cost_pd_comparator", "Cost: Progressed Disease (Clofarabine)", value = 4500, min = 0),
      numericInput("utility_pf_comparator", "Utility: Progression-Free (Clofarabine)", value = 0.75, min = 0, max = 1, step = 0.1),
      numericInput("utility_pd_comparator", "Utility: Progressed Disease (Clofarabine)", value = 0.55, min = 0, max = 1, step = 0.1)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Survival Curves", plotOutput("surv_plot")),
        tabPanel("Results Table", DTOutput("results_table")),
        tabPanel("ICER Calculation", DTOutput("icer_table"))
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  # Reactive Survival Data based on selected distribution
  survival_data_reactive <- reactive({
    survival_data(tolower(input$surv_dist))
  })
  
  # Reactive Costs and Utilities for CAR-T
  costs_intervention <- reactive({
    data.frame(
      state = c("Progression-Free", "Progressed", "Death"),
      cost = c(input$cost_pf_intervention, input$cost_pd_intervention, 0)
    )
  })
  
  utilities_intervention <- reactive({
    data.frame(
      state = c("Progression-Free", "Progressed", "Death"),
      utility = c(input$utility_pf_intervention, input$utility_pd_intervention, 0)
    )
  })
  
  # Reactive Costs and Utilities for Clofarabine
  costs_comparator <- reactive({
    data.frame(
      state = c("Progression-Free", "Progressed", "Death"),
      cost = c(input$cost_pf_comparator, input$cost_pd_comparator, 0)
    )
  })
  
  utilities_comparator <- reactive({
    data.frame(
      state = c("Progression-Free", "Progressed", "Death"),
      utility = c(input$utility_pf_comparator, input$utility_pd_comparator, 0)
    )
  })
  
  # PSA Calculation for CAR-T and Clofarabine
  psa_intervention <- reactive({
    calculate_psa(survival_data_reactive(), costs_intervention(), utilities_intervention())
  })
  
  psa_comparator <- reactive({
    calculate_psa(survival_data_reactive(), costs_comparator(), utilities_comparator())
  })
  
  # Plot Survival Curves
  output$surv_plot <- renderPlot({
    combined_data <- bind_rows(
      mutate(psa_intervention(), treatment = "CAR-T"),
      mutate(psa_comparator(), treatment = "Clofarabine")
    )
    
    ggplot(combined_data, aes(x = time, color = treatment)) +
      geom_line(aes(y = progression_free, color = "Progression-Free")) +
      geom_line(aes(y = progressed, color = "Progressed")) +
      geom_line(aes(y = dead, color = "Death")) +
      labs(
        title = "Survival Curves for CAR-T and Clofarabine",
        x = "Time (Months)", y = "Proportion of Patients",
        color = "Health State"
      ) +
      theme_minimal()
  })
  
  # Results Table
  output$results_table <- renderDT({
    combined_results <- bind_rows(
      mutate(psa_intervention(), treatment = "CAR-T"),
      mutate(psa_comparator(), treatment = "Clofarabine")
    )
    
    combined_results %>%
      filter(time <= input$time_horizon) %>%
      select(time, treatment, progression_free, progressed, dead, cost, qalys) %>%
      datatable(
        options = list(pageLength = 10, scrollX = TRUE),
        caption = "Cost and QALY Results for CAR-T and Clofarabine"
      )
  })
  
  # ICER Calculation
  output$icer_table <- renderDT({
    intervention_results <- psa_intervention() %>%
      filter(time <= input$time_horizon) %>%
      summarize(
        total_cost = sum(cost),
        total_qalys = sum(qalys)
      )
    
    comparator_results <- psa_comparator() %>%
      filter(time <= input$time_horizon) %>%
      summarize(
        total_cost = sum(cost),
        total_qalys = sum(qalys)
      )
    
    # Calculate ICER (Incremental Cost-Effectiveness Ratio)
    incremental_cost <- intervention_results$total_cost - comparator_results$total_cost
    incremental_qalys <- intervention_results$total_qalys - comparator_results$total_qalys
    icers <- ifelse(incremental_qalys != 0, incremental_cost / incremental_qalys, NA)
    
    icers_df <- data.frame(
      Intervention = "CAR-T",
      Comparator = "Clofarabine",
      Incremental_Cost = incremental_cost,
      Incremental_QALYs = incremental_qalys,
      ICER = icers
    )
    
    datatable(
      icers_df,
      options = list(pageLength = 10, scrollX = TRUE),
      caption = "Incremental Cost-Effectiveness Ratio (ICER)"
    )
  })
}

# Run the Shiny App
shinyApp(ui = ui, server = server)
```

<div style="width: 100% ; height: 400px ; text-align: center; box-sizing: border-box; -moz-box-sizing: border-box; -webkit-box-sizing: border-box;" class="muted well">Shiny applications not supported in static R Markdown documents</div>

session information

``` r
sessionInfo()
```

    ## R version 4.3.1 (2023-06-16 ucrt)
    ## Platform: x86_64-w64-mingw32/x64 (64-bit)
    ## Running under: Windows 11 x64 (build 22631)
    ## 
    ## Matrix products: default
    ## 
    ## 
    ## locale:
    ## [1] LC_COLLATE=English_United Kingdom.utf8 
    ## [2] LC_CTYPE=English_United Kingdom.utf8   
    ## [3] LC_MONETARY=English_United Kingdom.utf8
    ## [4] LC_NUMERIC=C                           
    ## [5] LC_TIME=English_United Kingdom.utf8    
    ## 
    ## time zone: Europe/London
    ## tzcode source: internal
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ## [1] DT_0.33           writexl_1.5.1     shinythemes_1.2.0 shiny_1.9.1      
    ## [5] dplyr_1.1.4       ggplot2_3.5.1     survival_3.7-0   
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] Matrix_1.6-1.1    jsonlite_1.8.7    gtable_0.3.4      compiler_4.3.1   
    ##  [5] promises_1.2.1    tidyselect_1.2.0  Rcpp_1.0.11       jquerylib_0.1.4  
    ##  [9] later_1.3.2       splines_4.3.1     scales_1.3.0      yaml_2.3.7       
    ## [13] fastmap_1.2.0     mime_0.12         lattice_0.21-8    R6_2.5.1         
    ## [17] generics_0.1.3    knitr_1.44        htmlwidgets_1.6.2 tibble_3.2.1     
    ## [21] munsell_0.5.0     bslib_0.8.0       pillar_1.9.0      rlang_1.1.1      
    ## [25] utf8_1.2.3        cachem_1.1.0      httpuv_1.6.13     xfun_0.40        
    ## [29] sass_0.4.9        memoise_2.0.1     cli_3.6.1         withr_2.5.0      
    ## [33] magrittr_2.0.3    crosstalk_1.2.1   digest_0.6.33     grid_4.3.1       
    ## [37] rstudioapi_0.15.0 xtable_1.8-4      lifecycle_1.0.3   vctrs_0.6.5      
    ## [41] evaluate_0.21     glue_1.6.2        fansi_1.0.4       colorspace_2.1-0 
    ## [45] rmarkdown_2.25    tools_4.3.1       pkgconfig_2.0.3   htmltools_0.5.8.1
