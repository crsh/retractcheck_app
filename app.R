# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library("shiny")
library("retractcheck")
library("DT")

createTempDir <- function(input) {
  repeat {
    randomDir <- paste(sample(c(0:9, letters),32, replace=TRUE), collapse="")
    retDir <- paste(dirname(input$files$datapath), randomDir, sep = "/")
    if (!file.exists(retDir)) {
      break
    }
  }
  dir.create(retDir)
  return(retDir)
}

# Define UI for application that draws a histogram
ui <- fluidPage(
  h1("retractcheck"),
  br(),

  p("Upload a file containing references with DOI to check whether the ",
  a("Open retractions", href = "http://openretractions.com"),
  " database contains any correction or retraction notices. ",
  em("Note, to our knowledge no complete database of corrections and retractions exists. "),
  code("retractcheck"),
  ", therefore, only yields unambiguous results for references for which it finds corrections or retractions."),

  helpText("Powered by the R-package", code(a("retractcheck", href = "https://github.com/libscie/retractcheck"))),

  hr(),

  # Input in sidepanel
  fluidRow(
    column(
      6,

      # Input
      fileInput("files", "Upload files:", multiple = TRUE, accept = c("pdf/html/docx/rtf"), width = "100%"),
      helpText("Supported file types: PDF, HTML, DOCX, RTF"),

      br()

    ),


    conditionalPanel(
      condition = "output.results",
      tags$style(type="text/css", "button{float:right;}"),
      br(),
      br(),
      downloadButton("downloadData", "Download Results (csv)"),
      br()
    )


  ),

  hr(),

  # Plot in main
  mainPanel(
    tags$style(type = "text/css", "data table { font-size: 11pt;}"),
    htmlOutput("result_table"),
    br(),
    br(),
    br()
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  # File table
  output$filetable <- renderTable({
    if (is.null(input$files)) return(NULL)

    tab <- input$files[, "name", drop = FALSE]
    tab$name[nchar(tab$name) > 23] <- gsub("(?<=^.{30}).*"," (...)",  tab$name[nchar(tab$name) > 23], perl = TRUE)

    return(tab)
  })

  Results <- reactive({

    # Dir <- tempdir()
    Dir <- createTempDir(input)

    # Copy to the directory
    needCopy <- !file.exists(paste0(Dir, "/", input$files$name))
    file.copy(input$files$datapath[needCopy], paste0(Dir, "/", input$files$name[needCopy]))

    # Read in statcheck
    res <- retractcheck::retractcheck_dir(Dir)
    output$message <- renderText({resCap})

    # file.remove(paste0(Dir, "/", input$files$name[needCopy]))

    unlink(Dir, recursive = TRUE)

    return(res)

  })

  output$downloadData <- downloadHandler(
    filename = "retractcheck_report.csv",
    content = function(con) {
      if (is.null(input$files)) {
        # User has not uploaded a file yet
        return(NULL)
      }

      write.csv(Results(), con)
    })

  # Summary table
  # output$summary <- DT::renderTable({
  #   if (is.null(input$files)) return(NULL)
  #
  #   tabSummary <- Results()
  #
  #
  #   return(tabSummary)
  #
  # })

  # Detailed
  output$results <- DT::renderDataTable({
    if (is.null(input$files)) return(NULL)

    tabResults <- Results()

    # More consise names
    names(tabResults) <- c("Source", "DOI", "Type of update", "Retracted", "Update DOI", "Publisher", "Title", "Original date", "Update date", "Delay")

    tabResults$Source <- as.character(tabResults$Source)
    tabResults$Source[nchar(tabResults$Source) > 35] <- gsub("(?<=^.{30}).*"," (...)",  tabResults$Source[nchar(tabResults$Source) > 35], perl = TRUE)

    return(tabResults)
  })

  output$result_table <- renderUI({
    # DT::dataTableOutput("summary")
    div(DT::dataTableOutput("results"), style="font-size:80%; font-family: Helvetica Neue,Helvetica,Arial,sans-serif")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
