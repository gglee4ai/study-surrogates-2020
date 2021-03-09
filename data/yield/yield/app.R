library(shiny)

## read latest files from drop box
usedrop <- FALSE
if(usedrop) {
  library(rdrop2)
  token <- readRDS("droptoken.rds")
  drop_acc(dtoken = token)
  data.files <- drop_search(".txt", path="yield")$path
  for(df in data.files) drop_get(df, overwrite=TRUE)
}

source("yield.R")
start.wk <- 37 ## 27 ## 21

# Define the fields we want to save from the form
fields <- c(formalArgs(yield), "Nx", "reps")

# Save a response
# ---- This is one of the two functions we will change for every storage type ----
saveData <- function(login, data) {
  table.file <- paste(login, ".txt", sep="")
  if(! file.exists(table.file)) invisible(NULL)
  data <- drop(t(data))
  reps <- data[8]
  Nx <- data[7]
  data <- data[-c(7,8)]
  X <- matrix(data, ncol=6)
  Y <- rep(NA, 10)
  ## wk <- as.numeric(strftime(Sys.time(),format="%W"))
  wk <- 50
  var <- 0.1+0.05*(cos(2*pi*(wk-start.wk)/10)+1)
  Y[1:reps] <- yield.fn(X) + rnorm(reps, sd=sqrt(var))
  new.yield.data <- data.frame(week=wk, X, Nx=Nx, matrix(Y, nrow=1))
  names(new.yield.data) <- c("week", formalArgs(yield), "Nx", paste("y", 1:10, sep=""))
  write.table(new.yield.data, file=table.file,
    quote=FALSE, row.names=FALSE, col.names=FALSE, append=TRUE)
  if(usedrop) drop_upload(table.file, dest="yield")
}


# Load all previous responses
# ---- This is one of the two functions we will change for every storage type ----
loadData <- function(login, reverse=TRUE) {
    table.file <- paste(login, ".txt", sep="")
    ## if(dbget) drop_get(paste(outputDir, "/", table.file, sep=""), overwrite=TRUE)
    if (file.exists(table.file)) {
      yield.data <- read.table(table.file, header=TRUE)
      if(reverse) yield.data <- yield.data[nrow(yield.data):1,]
      return(yield.data)
    } 
    invisible(NULL)
}

cost <- c(10,3,3,3,2,2,2,1,1,1)

spent <- function(login) {
  data <- loadData(login)
  if(is.null(data)) return(NULL)
  s <- 0
  for(i in 1:nrow(data)) {
    if(data[i,1] < start.wk) next;
    row <- data[i,-c(1:(length(formalArgs(yield))+2))]
    row[is.na(row)] <- 0
    s <- s + drop(cost %*% as.logical(row))
  }
  return(s)
}

budget <- function() {
  ## wk <- as.numeric(strftime(Sys.time(),format="%W"))
  wk <- 50
  (wk - start.wk + 1)*100
}

# Shiny app with 3 fields that the user can submit data for
shinyApp(
  ui = fluidPage(
    titlePanel("Yield Simulation"),
    h3(textOutput("hello")),
    tags$hr(),
    ## conditionalPanel(condition = "output.login == false", tags$hr()),
    conditionalPanel(condition = "output.budget > 0", 
      sidebarLayout(#fluid=FALSE,
        sidebarPanel(width=7, fluidPage(
      fluidRow(
        column(3, numericInput("N", "N", NA, min=0, width=150)),
        column(3, numericInput("P", "P", NA, min=0, width=150)),
        column(3, numericInput("K", "K", NA, min=0, width=150)),
        column(3, numericInput("Na", "Na", NA, min=0, width=150))
      ), fluidRow(
        column(3, numericInput("Ca", "Ca", NA, min=0, width=150)),
        column(3, numericInput("Mg", "Mg", NA, min=0, width=150)),
        column(3, numericInput("Nx", "Nx", NA, min=0, width=150)),
        column(3, sliderInput("reps", "Reps", min=1, max=10, value=1))#,
      ))),
        mainPanel(width=3,
          fluidRow(textOutput("check")),
          conditionalPanel(condition="output.check == 'Ready to run!'",
            fluidRow(p("Note: there is no option to undo.")),
            fluidRow(column(2, br(), actionButton("srun", "Run"))))
          ))
    ),
    DT::dataTableOutput("yield.data", width = 300),
    conditionalPanel(condition = "output.budget > -101",
      downloadButton("downloadData", "Download"))
  ),
  server = function(input, output, session) {
    
    output$check <- reactive({ 
      data <- sapply(fields, function(x) input[[x]])
      validate(
        need(all(is.finite(data)), "NA values not allowed."), 
        need(all(data[!is.na(data)] >= 0), "All inputs must be positive.")
      )
      "Ready to run!"
    })

    # Whenever a field is filled, aggregate all form data
    formData <- reactive({
      data <- sapply(fields, function(x) input[[x]])
      data
    })

    # When the Submit button is clicked, save the form data
    observeEvent(input$srun, {
      login <- substring(session$clientData$url_search, 2)
      saveData(login, formData())
    })

    output$hello <- renderPrint({
      input$srun 
      login <- substring(session$clientData$url_search, 2)
      table.file <- paste(login, ".txt", sep="")
      if (file.exists(table.file)) {
        spent.calc <- spent(login)
        budget.calc <- budget()
        greet <- paste("Hello [", login, "], you have spent ", spent.calc, sep="")
        greet <- paste(greet, " of ", budget.calc, " units", sep="")
        return(cat(greet))
      } else return(cat("User [", login, "] not found; please specify login via ?login in URL", sep="")) 
      ## else return(cat("Welcome, please log in"))
    })

    output$budget <- reactive({
      input$srun
      login <- substring(session$clientData$url_search, 2)
      table.file <- paste(login, ".txt", sep="")
      if (file.exists(table.file)) { 
        spent.calc <- spent(login)
        budget.calc <- budget()
        return(budget.calc - spent.calc)
      } else return(-101)
    })
    outputOptions(output, 'budget', suspendWhenHidden=FALSE)

    # Show the previous responses
    # (update with current response when Submit is clicked)
    output$yield.data <- DT::renderDataTable({
      input$srun
      login <- substring(session$clientData$url_search, 2)
      DT::datatable(loadData(login), options=list(searching=FALSE, pageLength=25))
    })

    output$downloadData <- downloadHandler(
      filename = function() { 
        login <- substring(session$clientData$url_search, 2)
        paste(login, '.txt', sep='') 
      },
      content = function(file) {
        login <- substring(session$clientData$url_search, 2)
        write.table(loadData(login, reverse=FALSE), file, col.names=TRUE, quote=FALSE)
      }
    )
  }
)
