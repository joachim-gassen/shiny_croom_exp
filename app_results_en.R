# --- Header -------------------------------------------------------------------
# (C) Joachim Gassen 2019, gassen@wiwi.hu-berlin.de
# License: MIT. See LICENSE file for details.
#
# English language shiny app communicating the results of the pricing experiment
# ------------------------------------------------------------------------------

library(DBI, quietly = TRUE)
library(shiny, quietly = TRUE)
library(DT, quietly = TRUE)
library(shinyjs, quietly = TRUE)
library(tidyverse)
library(kableExtra)

dbase_path <- "croom_exp_response.sqlite3"

ui <- fluidPage(
  titlePanel("A Pricing Task: Experimental Findings"),
  p("These are the findings of our classroom experiment."),
  br(),
  sidebarLayout(
    sidebarPanel(
      radioButtons("data_cutoff", "Do you want to limit the sample?",
                   c("All observations" = "none",
                     "Only observations with a price > $6" = "be6")),
      br(),
      sliderInput("exclude_below_time",
                  "Only observations with response time in seconds larger than...",
                  value = 0,
                  min = 0,
                  max = 60),
      downloadButton("download", "Download the experimental data")
      
    ),
    mainPanel(
      h3("Descriptive Statistics"),
      p("Below you will find your answering behavior, separated for both",
        "experimental groups. Price in measured in $ and response", 
        "time in seconds."),
      tableOutput("descriptive_table"),
      h3("Group Box Plots"),
      plotOutput("box_plots"),
      br(),
      h3("Test Statistics"),
      tableOutput("tests"),
      p(),
      p("The Chi-square est is based on the following contingency table."),
      tableOutput("cont_table"),
      br(),
      HTML("<p>Copyright Joachim Gassen, gassen@wiwi.hu-berlin.de, 2019.", 
        "See <a href='https://github.com/joachim-gassen/shiny_croom_exp'>",
        "GitHub repository</a> for license, code and details.")
    )
  )
)


server <- function(input, output, session) {

  read_experiment_data <- function(fname) {
    con <- dbConnect(RSQLite::SQLite(), fname)
    res <- dbSendQuery(con, "SELECT * FROM answers")
    df <- dbFetch(res)
    dbClearResult(res)
    dbDisconnect(con)
    df$full_cost <- ifelse (df$full_cost == 1, "Full cost", "Variable cost")
    df
  } 
  
  raw_df <- read_experiment_data(dbase_path)
  
  d <- reactive({
    if (input$data_cutoff == "be6") df <- raw_df %>% filter(price > 6)
    else df <- raw_df
    df %>% filter(time >= input$exclude_below_time)    
  })

  output$descriptive_table <- function() {
    df <- d() %>%  
      group_by(full_cost) %>%
      select(price, time) %>%
      gather(key = "var", value = "value", -full_cost) %>%
      group_by(full_cost, var) %>%
      summarise(N = n(),
                Mean = mean(value),
                'Standard deviation' = sd(value),
                Minimum = min(value),
                'First quartile' = quantile(value, 0.25),
                Median = median(value),
                'Third quartile' = quantile(value, 0.75),
                Maximum = max(value)) %>%
      rename('Full cost data' = full_cost)
    df <- as_tibble(cbind(nms = names(df), t(df)))
    print_df <- df[c(4:10),]
    print_df[, 2:5] <- lapply(print_df[, 2:5], as.numeric)
    names(print_df) <- c("", rep(c("price", "response time"), 2))
    group_header <- c(1, 2, 2)
    names(group_header) <- c(" ", 
                             sprintf("Full cost data (N = %d)", as.numeric(df[3, 2])),
                             sprintf("Variable cost data (N = %d)", as.numeric(df[3, 4])))
    knitr::kable(print_df, "html", digits = 2) %>%
      kable_styling("striped", full_width = F) %>%
      add_header_above(group_header)
  }

  output$box_plots <- renderPlot(
    ggplot(data = d(), aes(x = full_cost, y = price)) +
      geom_boxplot(outlier.color = NA, fill = "lightblue") +
      geom_jitter(width = 0.2, height = 0, size = 3, stroke = 0, shape = 16, alpha = 0.5) +
      theme_bw() + 
      labs(x = "Experimental group", 
           y = "Calculated price in $") 
  )
  
  output$tests <- function() {
    df <- d() 
    
    tt <- t.test(price ~ full_cost, data = df)
    rt <- wilcox.test(price ~ full_cost, data = df)
    
    df$high_price <- df$price > 12
    df$high_price[df$price == 12] <- NA
    
    ct <- chisq.test(df$high_price[!is.na(df$high_price)], 
                     df$full_cost[!is.na(df$high_price)])
    print_df <- rbind(c(tt$statistic, tt$p.value),
                      c(rt$statistic, rt$p.value),
                      c(ct$statistic, ct$p.value))
    
    colnames(print_df) <- c("Statistic", "P-value (two sided)")
    rownames(print_df) <- c("T-test for mean differences",
                            "Wilcoxon test for distribution differences",
                            "Chi-square test for group differences")
    
    knitr::kable(print_df, "html", digits = c(2, 4)) %>%
      kable_styling("striped", full_width = F)
  }
  
  output$cont_table <- function() {
    df <- d() 
    
    df$high_price <- df$price > 12
    df$high_price[df$price == 12] <- NA

    ct <- table(df$high_price, df$full_cost)
    rownames(ct) <- c("Price below $12", "Price above $12")
    
    knitr::kable(ct, "html") %>%
      kable_styling("striped", full_width = F)
  }
  
  output$download <- downloadHandler(
    filename <- function() {
      paste0('croom_exp_data-', Sys.Date(), '.csv')
    },
    content <- function(con) {
      write.csv(raw_df, con)
    }
  )
}


# Run the application 
shinyApp(ui = ui, server = server)

