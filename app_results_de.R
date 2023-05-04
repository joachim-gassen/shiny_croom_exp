# --- Header -------------------------------------------------------------------
# (C) Joachim Gassen 2021, gassen@wiwi.hu-berlin.de
# License: MIT. See LICENSE file for details.
#
# German language shiny app communicating the results of the pricing experiment
# ------------------------------------------------------------------------------


library(DBI, quietly = TRUE)
library(shiny, quietly = TRUE)
library(DT, quietly = TRUE)
library(shinyjs, quietly = TRUE)
library(tidyverse)
library(kableExtra)

# dbase_path <- "data_completed_exp/kore_sose22_croom_exp_done.sqlite3"
dbase_path <- "../kore_croom_exp/croom_exp.sqlite3"
end_experiment <- lubridate::as_datetime("2023-05-04 08:30:00", tz = "CEST")
DEBUG <- FALSE

if(Sys.time() < end_experiment & ! DEBUG) {
  ui <- fluidPage(p("Hier gibt es leider noch gar nichts zu sehen."))
} else {
  ui <- fluidPage(
    titlePanel("Preiskalkulation: Die Auswertung"),
    p("Hier finden Sie die Auswertung unseres Experiments."),
    br(),
    sidebarLayout(
      sidebarPanel(
        radioButtons("data_cutoff", "Wollen Sie das Sample beschränken?",
                     c("Alle Beobachtungen" = "none",
                       "Nur Beobachtungen mit Preis > 6 €" = "be6eur")),
        br(),
        sliderInput("exclude_below_time",
                    "Nur Beobachtungen mit Antwortzeit in Sekunden größer als...",
                    value = 0,
                    min = 0,
                    max = 60),
        downloadButton("download", "Download des Datensatzes")
        
      ),
      mainPanel(
        h3("Deskriptive Statistik"),
        p("Unten sehen Sie Ihr Antwortverhalten, getrennt für die beiden",
          "Experimentalgruppen. Der Preis in ist Euro und die Antwortzeit", 
          "ist in Sekunden angegeben."),
        tableOutput("descriptive_table"),
        h3("Gruppenboxplot"),
        plotOutput("box_plots"),
        br(),
        h3("Teststatistiken"),
        tableOutput("tests"),
        p(),
        p("Der Chi-square Test basiert auf der folgenden Kontingenztabelle."),
        tableOutput("cont_table"),
        br(),
        HTML("<p>Copyright Joachim Gassen, gassen@wiwi.hu-berlin.de, 2021.", 
             "Siehe <a href='https://github.com/joachim-gassen/shiny_croom_exp'>",
             "GitHub repository</a> für Lizenz, Code und Details.")
      )
    )
  )
}


server <- function(input, output, session) {

  read_experiment_data <- function(fname) {
    con <- dbConnect(RSQLite::SQLite(), fname)
    res <- dbSendQuery(con, "SELECT * FROM answers")
    df <- dbFetch(res)
    dbClearResult(res)
    dbDisconnect(con)
    df$full_cost <- ifelse (df$full_cost == 1, "Vollkostendaten", "Teilkostendaten")
    df
  } 
  
  raw_df <- read_experiment_data(dbase_path)
  
  d <- reactive({
    if (input$data_cutoff == "be6eur") df <- raw_df %>% filter(price > 6)
    else df <- raw_df
    df %>% filter(time >= input$exclude_below_time)    
  })
  
  output$descriptive_table <- function() {
    df <- d() %>%  
      group_by(full_cost) %>%
      select(full_cost, price, time) %>%
      gather(key = "var", value = "value", -full_cost) %>%
      group_by(full_cost, var) %>%
      summarise(N = n(),
                Mittelwert = mean(value),
                Standardabweichung = sd(value),
                Minimum = min(value),
                'Erstes Quartil' = quantile(value, 0.25),
                Median = median(value),
                'Drittes Quartil' = quantile(value, 0.75),
                Maximum = max(value),
                .groups = "drop") %>%
      rename(Vollkostendaten = full_cost)
    df <- as_tibble(cbind(nms = names(df), t(df)))
    print_df <- df[c(4:10),]
    print_df[, 2:5] <- lapply(print_df[, 2:5], as.numeric)
    names(print_df) <- c("", rep(c("Preis", "Antwortzeit"), 2))
    group_header <- c(1, 2, 2)
    names(group_header) <- c(" ", 
                             sprintf("Teilkostendaten (N = %d)", as.numeric(df[3, 2])),
                             sprintf("Vollkostendaten (N = %d)", as.numeric(df[3, 4])))
    knitr::kable(print_df, "html", digits = 2) %>%
      kable_styling("striped", full_width = F) %>%
      add_header_above(group_header)
  }

  output$box_plots <- renderPlot(
    ggplot(data = d(), aes(x = full_cost, y = price)) +
      geom_boxplot(outlier.color = NA, fill = "lightblue") +
      geom_jitter(width = 0.2, height = 0, size = 3, stroke = 0, shape = 16, alpha = 0.5) +
      theme_bw() + 
      labs(x = "Experimentalgruppe", 
           y = "Kalkulierter Preis in €") 
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
    
    colnames(print_df) <- c("Statistik", "P Wert (zweiseitig)")
    rownames(print_df) <- c("T-Test auf Mittelwertgleichheit",
                            "Wilcoxon Test auf Verteilungsgleichheit",
                            "Chi-square Test auf Gruppenunterschiede")
    
    knitr::kable(print_df, "html", digits = c(2, 4)) %>%
      kable_styling("striped", full_width = F)
  }
  
  output$cont_table <- function() {
    df <- d() 
    
    df$high_price <- df$price > 12
    df$high_price[df$price == 12] <- NA

    ct <- table(df$high_price, df$full_cost)
    rownames(ct) <- c("Preis unter 12 €", "Preis über 12 €")
    
    knitr::kable(ct, "html") %>%
      kable_styling("striped", full_width = F)
  }
  
  output$download <- downloadHandler(
    filename <- function() {
      paste0('croom_exp_data-', Sys.Date(), '.csv')
    },
    content <- function(con) {
      write.csv2(raw_df, con)
    }
  )
}


# Run the application 
shinyApp(ui = ui, server = server)

