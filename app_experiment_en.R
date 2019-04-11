# --- Header -------------------------------------------------------------------
# (C) Joachim Gassen 2019, gassen@wiwi.hu-berlin.de
# License: MIT. See LICENSE file for details.
#
# English language shiny app implementing a simple pricing experiment
# to assess the price distortion triggered by full cost allocation
#
# See https://calligross.de/post/using-cookie-based-authentication-with-shiny/
# for cookie implementation
# ------------------------------------------------------------------------------

library(DBI, quietly = TRUE)
library(shiny, quietly = TRUE)
library(DT, quietly = TRUE)
library(shinyjs, quietly = TRUE)

cost_info <- read.csv2("cost_info_en.csv")

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

ui <- fluidPage(
  tags$head(
    tags$script(src = "js.cookies.js")
  ),
  useShinyjs(), 
  extendShinyjs(text = jsCode),
  titlePanel("A Pricing Task"),
  uiOutput("greeting"),
  br(),
  uiOutput("cost_info"),
  br(),
  uiOutput("response")
)


server <- function(input, output, session) {
  time_submitted <- NA
  submitted <- reactiveVal(FALSE)
  wrong_selection <- reactiveVal(FALSE)
  
  has_participated <- reactiveVal(FALSE)
  
  # check if a user already has participated cookie is present  
  observe({
    js$getcookie()
    if (!is.null(input$jscookie) && 
        input$jscookie == "HAS_PARTICIPATED_IN_CROOM_EXP") {
      has_participated(TRUE)
    }
  })

  store_user_response <- function(tment, response, time) {
    con <- dbConnect(RSQLite::SQLite(), "croom_exp.sqlite3")
    res <- dbSendQuery(con, sprintf("INSERT INTO answers VALUES ('%s', %d, %f, %f)",
                                    Sys.time(), tment, response, time))
    dbClearResult(res)
    dbDisconnect(con)   
  } 
  
  assign_treatment <- function() {
    con <- dbConnect(RSQLite::SQLite(), "croom_exp.sqlite3")
    res <- dbSendQuery(con, "SELECT full_cost FROM answers")
    df <- dbFetch(res)
    dbClearResult(res)
    dbDisconnect(con)
    if (nrow(df) == 0) runif(1) > 0.5   
    else !(mean(df$full_cost) > 0.5)
  } 
  
  full_cost_tment <- assign_treatment() 
  time_in <- Sys.time()
  
  observeEvent(input$submit, {
        if (input$price != "") {
          time_submitted <<- as.numeric(difftime(Sys.time(), time_in, units = "sec"))
          response <- as.numeric(substr(input$price, 2, 3))
          store_user_response(full_cost_tment, response, time_submitted)
          disable("submit")
          submitted(TRUE)
          wrong_selection(FALSE)
          js$setcookie("HAS_PARTICIPATED_IN_CROOM_EXP") 
        } else {
          wrong_selection(TRUE)
        }
  })
  
  output$greeting <- renderUI(
    if(!has_participated()) {
      fluidRow(
        column(12, br(),
               p("Welcome! This little classroom experiment presents a pricing",
                 "task. Below you will find information about the cost",
                 "structure of a new product. Your task is to decide on a",
                 "price based on the presented costs.",
                 "The predicted number of unit sales is uncertain. It is based",
                 "on a sales price of $12. You can assume that unit sales will",
                 "increase when you set a lower price while they will decrease", 
                 "when you set a higher price."),
               p("Please work on your own and enter your price in the",
                 "field below."),
               p("Thank you!"),
               br()
               )
      )
    } else {
      fluidRow(
        column(12, align = "center",
               p(),
               p("Thank you! you can now close this window."),
               p()
        ))      
    }
  )

  output$response <- renderUI(
    if (!has_participated() & !wrong_selection()) {
      fluidRow(
        column(12, align = "center",
               selectizeInput(
                 "price", "Your sales price", 
                 choices = sprintf("$%d", 1:20),
                 options = list(
                   placeholder = 'price',
                   onInitialize = I('function() { this.setValue(""); }')
                 )
               ),
               br(),
               p("When you selected your sales price,",
                 "click below to submit your answer."),
               actionButton("submit", "Submit answer", class = "btn-primary"), 
               br()
        ))
    } else if (!has_participated() & wrong_selection()) {
      fluidRow(
        column(12, align = "center",
               selectizeInput(
                 "price", "Your sales price", 
                 choices = sprintf("$%d", 1:20),
                 options = list(
                   placeholder = 'price',
                   onInitialize = I('function() { this.setValue(""); }')
                 )
               ),
               br(),
               p("When you selected your sales price,",
                 "click below to submit your answer."),
               actionButton("submit", "Submit answer", class = "btn-primary"), 
               p(),
               p("Please select a price in the menu above!")
      )) 
    } 
  )
  
  output$full_cost_info <- renderDataTable(
    if (!has_participated()) {
      datatable(cost_info[cost_info$full_cost == 1, c("line_item", "value")], 
                class = "", 
                options = list(searching = FALSE,
                               paging = FALSE,
                               info = FALSE,
                               ordering = FALSE),
                rownames = FALSE,
                selection = "none",
                colnames = c("Cost item" = 1,
                             "Value" = 2)) %>%
        formatStyle(
          1,
          target = "row",
          fontWeight = styleEqual("Full cost per unit", "bold")
        )
    }
  )

  output$partial_cost_info <- renderDataTable(
    if (!has_participated()) {
      datatable(cost_info[cost_info$full_cost == 0, c("line_item", "value")], 
                class = "",
                options = list(searching = FALSE,
                               paging = FALSE,
                               info = FALSE,
                               ordering = FALSE),
                rownames = FALSE,
                selection = "none",
                colnames = c("Cost item" = 1,
                             "Value" = 2)) %>%
        formatStyle(
          1,
          target = "row",
          fontWeight = styleEqual("Variable cost per unit", "bold")
        )
    }
  )

  output$cost_info <- renderUI(
      if (!has_participated() & full_cost_tment) {
        fluidRow(
          column(12, tagList(
            p("The following information is available:"),
            dataTableOutput('full_cost_info'))
        ))
      } else if (!has_participated()) {
        fluidRow(
          column(12, tagList(
            p("The following information is available:"),
            dataTableOutput('partial_cost_info'))
          ))
      }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

