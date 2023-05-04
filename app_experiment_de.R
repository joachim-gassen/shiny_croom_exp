# --- Header -------------------------------------------------------------------
# (C) Joachim Gassen 2021, gassen@wiwi.hu-berlin.de
# License: MIT. See LICENSE file for details.
#
# German language shiny app implementing a simple pricing experiment
# to assess the price distortion triggered by full cost allocation
#
# See https://calligross.de/post/using-cookie-based-authentication-with-shiny/
# for cookie implementation
# ------------------------------------------------------------------------------

library(DBI, quietly = TRUE)
library(shiny, quietly = TRUE)
library(DT, quietly = TRUE)
library(shinyjs, quietly = TRUE)

cost_info <- read.csv2("cost_info_de.csv")
dbase_path <- "croom_exp.sqlite3"
start_experiment <- lubridate::as_datetime("2023-05-04 08:30:00", tz = "CEST")
DEBUG <- FALSE

disableActionButton <- function(id, session) {
  session$sendCustomMessage(type="jsCode",
                            list(code= paste("$('#", id, "').prop('disabled',true)"
                                             , sep="")))
}

jsCode <- '
  shinyjs.getcookie = function(params) {
    var cookie = Cookies.get("id");
    if (typeof cookie !== "undefined") {
      Shiny.onInputChange("jscookie", cookie);
    } else {
      var cookie = "";
      Shiny.onInputChange("jscookie", cookie);
    }
  }

  shinyjs.setcookie = function(params) {
    Cookies.set("id", escape(params), { expires: 0.5 });  
    Shiny.onInputChange("jscookie", params);
  }

  shinyjs.rmcookie = function(params) {
    Cookies.remove("id");
    Shiny.onInputChange("jscookie", "");
  }
'

if(Sys.time() < start_experiment & ! DEBUG) {
  ui <- fluidPage(p("Hier gibt es leider noch gar nichts zu sehen."))
} else {
  ui <- fluidPage(
    tags$head(
      tags$script(src = "js.cookies.js")
    ),
    useShinyjs(), 
    extendShinyjs(
      text = jsCode, functions = c("getcookie", "setcookie", "rmcookie")
    ),
    titlePanel("Preiskalkulation"),
    uiOutput("greeting"),
    br(),
    uiOutput("cost_info"),
    br(),
    uiOutput("response")
  )
}


server <- function(input, output, session) {
  time_submitted <- NA
  submitted <- reactiveVal(FALSE)
  wrong_selection <- reactiveVal(FALSE)
  
  has_participated <- reactiveVal(FALSE)
  full_cost_tment <- reactiveVal()
  has_been_treated <- reactiveVal(FALSE)
  
  # check if a user already has participated cookie is present  
  observe({js$getcookie()})
  observeEvent(input$jscookie,{
    if (!DEBUG && !is.null(input$jscookie)) {
      if (input$jscookie == "HAS_PARTICIPATED_IN_CROOM_EXP") 
        has_participated(TRUE)
      if (!has_been_treated() && !has_participated() &&
          grepl("CROOM_EXP_TMENT_", input$jscookie, fixed = TRUE)) {
        has_been_treated(TRUE)
        full_cost_tment(as.logical(as.integer(substr(input$jscookie, 17, 17))))
      }
    }
    if (!has_participated() && !has_been_treated()) {
      con <- dbConnect(RSQLite::SQLite(), dbase_path)
      res <- dbSendQuery(con, "SELECT full_cost FROM answers")
      df <- dbFetch(res)
      dbClearResult(res)
      dbDisconnect(con)
      if (nrow(df) == 0) full_cost_tment(runif(1) > 0.5)   
      else full_cost_tment(!(mean(df$full_cost) > 0.5))
      has_been_treated(TRUE)
      js$setcookie(sprintf("CROOM_EXP_TMENT_%d", as.integer(full_cost_tment())))
    }
  })
  
  store_user_response <- function(tment, response, time) {
    con <- dbConnect(RSQLite::SQLite(), dbase_path)
    res <- dbSendQuery(con, sprintf("INSERT INTO answers VALUES ('%s', %d, %f, %f)",
                                    Sys.time(), tment, response, time))
    dbClearResult(res)
    dbDisconnect(con)   
  } 
  
  time_in <- Sys.time()
  
  observeEvent(input$submit, {
    if (input$price != "") {
      time_submitted <<- as.numeric(difftime(Sys.time(), time_in, units = "sec"))
      response <- as.numeric(substr(input$price, 1, 2))
      store_user_response(full_cost_tment(), response, time_submitted)
      disable("submit")
      submitted(TRUE)
      wrong_selection(FALSE)
      has_participated(TRUE)
      js$setcookie("HAS_PARTICIPATED_IN_CROOM_EXP") 
    } else {
      wrong_selection(TRUE)
    }
  })
  
  output$greeting <- renderUI(
    if(has_been_treated() && !has_participated()) {
      fluidRow(
        column(12, br(),
               p("In diesem kleinen Vorlesungsexperiment",
                 "geht es um ein Kalkulationsproblem. Unten finden Sie Informationen",
                 "zu der Kostenstruktur eines neuen Produkts. Ihre Aufgabe",
                 "ist, auf Basis der Kosten einen Absatzpreis zu wählen.",
                 "Die geplante Absatzmenge ist unsicher. Sie ist auf Basis eines",
                 "Preises von 12 € geschätzt und es ist davon auszugehen,",
                 "dass sie bei niedrigeren Preisen steigt und bei höheren",
                 "Preisen fällt."),
               p("Bitte arbeiten sie selbständig und geben Sie den",
                 "Preis in dem Auswahlfeld unten ein."),
               p("Herzlichen Dank!"),
               br()
               )
      )
    } else if (has_participated()) {
      fluidRow(
        column(12, align = "center",
               p(),
               p("Herzlichen Dank! Sie können das Fenster jetzt schließen."),
               p()
        ))      
    }
  )

  output$response <- renderUI(
    if (has_been_treated() && !has_participated() & !wrong_selection()) {
      fluidRow(
        column(12, align = "center",
               selectizeInput(
                 "price", "Ihr kalkulierter Absatzpreis", 
                 choices = sprintf("%d €", 1:20),
                 options = list(
                   placeholder = 'Preis',
                   onInitialize = I('function() { this.setValue(""); }')
                 )
               ),
               br(),
               p("Wenn Sie Ihren favorisierten Preis ausgewählt haben,",
                 "bestätigen Sie Ihre Auswahl."),
               actionButton("submit", "Auswahl bestätigen", class = "btn-primary"), 
               br()
        ))
    } else if (!has_participated() & wrong_selection()) {
      fluidRow(
        column(12, align = "center",
               selectizeInput(
                 "price", "Ihr kalkulierter Absatzpreis", 
                 choices = sprintf("%d €", 1:20),
                 options = list(
                   placeholder = 'Preis',
                   onInitialize = I('function() { this.setValue(""); }')
                 )
               ),
               br(),
               p("Wenn Sie Ihren favorisierten Preis ausgewählt haben,",
                 "bestätigen Sie Ihre Auswahl."),
               actionButton("submit", "Auswahl bestätigen", class = "btn-primary"),
               p(),
               p("Bitte wählen Sie einen Preis im Auswahlmenü!")
      )) 
    } 
  )
  
  output$full_cost_info <- renderDataTable(
    datatable(cost_info[cost_info$full_cost == 1, c("line_item", "value")], 
              class = "", 
              options = list(searching = FALSE,
                             paging = FALSE,
                             info = FALSE,
                             ordering = FALSE),
              rownames = FALSE,
              selection = "none",
              colnames = c("Position" = 1,
                           "Wert" = 2)) %>%
      formatStyle(
        1,
        target = "row",
        fontWeight = styleEqual("Vollkosten pro Stück", "bold")
      )
  )

  output$partial_cost_info <- renderDataTable(
    datatable(cost_info[cost_info$full_cost == 0, c("line_item", "value")], 
              class = "",
              options = list(searching = FALSE,
                             paging = FALSE,
                             info = FALSE,
                             ordering = FALSE),
              rownames = FALSE,
              selection = "none",
              colnames = c("Position" = 1,
                           "Wert" = 2)) %>%
      formatStyle(
        1,
        target = "row",
        fontWeight = styleEqual("Teilkosten pro Stück", "bold")
      )
  )

  output$cost_info <- renderUI(
      if (has_been_treated() && !has_participated() && full_cost_tment()) {
        fluidRow(
          column(12, tagList(
            p("Folgende Informationen liegen vor:"),
            dataTableOutput('full_cost_info'))
        ))
      } else if (has_been_treated() && !has_participated()) {
        fluidRow(
          column(12, tagList(
            p("Folgende Informationen liegen vor:"),
            dataTableOutput('partial_cost_info'))
          ))
      }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

