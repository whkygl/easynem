#' Nematode database search
#'
#' This function provides a visual interface for retrieving basic data of nematodes.
#' The database used is from <http://nemaplex.ucdavis.edu/Ecology/EcophysiologyParms/EcoParameterMenu.html>
#' @return A web interface
#' @usage nem_database()
#' @references <http://nemaplex.ucdavis.edu/Ecology/EcophysiologyParms/EcoParameterMenu.html>
#' @export
nem_database <- function(){
filter_data <- function(df, keywords){
  positions <- c()
  key <- paste0(keywords, collapse = "|")
  for(i in 1:nrow(df)){
    if(any(grepl(key, df[i, ], ignore.case = T))){
      positions <- c(positions, i)
    }
  }
  df[positions, ]
}

ui <- shinydashboard::dashboardPage(
  shinydashboard::dashboardHeader(title = "Nemdatabase"),
  shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(
      shinydashboard::menuItem("Basic", tabName = "basic", icon = shiny::icon("dashboard")),
      shinydashboard::menuItem("Family", tabName = "family", icon = shiny::icon("home")),
      shinydashboard::menuItem("Genus", tabName = "genus", icon = shiny::icon("th")),
      shinydashboard::menuItem("Species", tabName = "species", icon = shiny::icon("list")),
      shinydashboard::menuItem("Feeding", tabName = "feeding", icon = shiny::icon("bell")),
      shinydashboard::menuItem("Structure", tabName = "structure", icon = shiny::icon("gear")),
      shinydashboard::menuItem("Function", tabName = "function", icon = shiny::icon("female"))
    )
  ),
  shinydashboard::dashboardBody(
    shinydashboard::tabItems(
      # First tab content
      shinydashboard::tabItem(tabName = "basic",
              shiny::fluidRow(
                shinydashboard::box(
                  shiny::fileInput("file1", "Import keywords",
                            multiple = TRUE,
                            accept = c("text/csv",
                                       "text/comma-separated-values,text/plain",
                                       ".csv")),
                  shiny::actionButton("resetBasis", "Reset"),
                  shiny::downloadButton("downloadData1", "Download"),
                  width="50%"
                )
              ),
              shiny::fluidRow(
                shinydashboard::box(
                  DT::DTOutput("table1"),
                  shiny::tags$p("Database cited from the Nemaplex website (http://nemaplex.ucdavis.edu/)"),
                  width="100%"
                )
              )
      ),
      shinydashboard::tabItem(tabName = "family",
              shiny::fluidRow(
                shinydashboard::box(
                  shiny::fileInput("file2", "Import keywords",
                            multiple = TRUE,
                            accept = c("text/csv",
                                       "text/comma-separated-values,text/plain",
                                       ".csv")),
                  shiny::actionButton("resetFamily", "Reset"),
                  shiny::downloadButton("downloadData2", "Download"),
                  width="50%"
                )
              ),
              shiny::fluidRow(
                shinydashboard::box(
                  DT::DTOutput("table2"),
                  shiny::tags$p("Database cited from the Nemaplex website (http://nemaplex.ucdavis.edu/"),
                  width="100%"
                )
              )
      ),
      shinydashboard::tabItem(tabName = "genus",
              shiny::fluidRow(
                shinydashboard::box(
                  shiny::fileInput("file3", "Import keywords",
                            multiple = TRUE,
                            accept = c("text/csv",
                                       "text/comma-separated-values,text/plain",
                                       ".csv")),
                  shiny::actionButton("resetGenus", "Reset"),
                  shiny::downloadButton("downloadData3", "Download"),
                  width="50%"
                )
              ),
              shiny::fluidRow(
                shinydashboard::box(
                  DT::DTOutput("table3"),
                  shiny::tags$p("Database cited from the Nemaplex website (http://nemaplex.ucdavis.edu/"),
                  width="100%"
                )
              )
      ),
      shinydashboard::tabItem(tabName = "species",
              shiny::fluidRow(
                shinydashboard::box(
                  shiny::fileInput("file4", "Import keywords",
                            multiple = TRUE,
                            accept = c("text/csv",
                                       "text/comma-separated-values,text/plain",
                                       ".csv")),
                  shiny::actionButton("resetSpecies", "Reset"),
                  shiny::downloadButton("downloadData4", "Download"),
                  width="50%"
                )
              ),
              shiny::fluidRow(
                shinydashboard::box(
                  DT::DTOutput("table4"),
                  shiny::tags$p("Database cited from the Nemaplex website (http://nemaplex.ucdavis.edu/"),
                  width="100%"
                )
              )
      ),
      shinydashboard::tabItem(tabName = "feeding",
              shiny::fluidRow(
                shinydashboard::box(
                  shiny::fileInput("file5", "Import keywords",
                            multiple = TRUE,
                            accept = c("text/csv",
                                       "text/comma-separated-values,text/plain",
                                       ".csv")),
                  shiny::actionButton("resetFeeding", "Reset"),
                  shiny::downloadButton("downloadData5", "Download"),
                  width="50%"
                )
              ),
              shiny::fluidRow(
                shinydashboard::box(
                  DT::DTOutput("table5"),
                  shiny::tags$p("Database cited from the Nemaplex website (http://nemaplex.ucdavis.edu/"),
                  width="100%"
                )
              )
      ),
      shinydashboard::tabItem(tabName = "structure",
              shiny::fluidRow(
                shinydashboard::box(
                  shiny::fileInput("file6", "Import keywords",
                            multiple = TRUE,
                            accept = c("text/csv",
                                       "text/comma-separated-values,text/plain",
                                       ".csv")),

                  shiny::actionButton("resetStructure", "Reset"),
                  shiny::downloadButton("downloadData6", "Download"),
                  width="50%"
                )
              ),
              shiny::fluidRow(
                shinydashboard::box(
                  DT::DTOutput("table6"),
                  shiny::tags$p("Database cited from the Nemaplex website (http://nemaplex.ucdavis.edu/"),
                  width="100%"
                )
              )
      ),
      shinydashboard::tabItem(tabName = "function",
              shiny::fluidRow(
                shinydashboard::box(
                  shiny::fileInput("file7", "Import keywords",
                            multiple = TRUE,
                            accept = c("text/csv",
                                       "text/comma-separated-values,text/plain",
                                       ".csv")),
                  shiny::actionButton("resetFunction", "Reset"),
                  shiny::downloadButton("downloadData7", "Download"),
                  width="50%"
                )
              ),
              shiny::fluidRow(
                shinydashboard::box(
                  DT::DTOutput("table7"),
                  shiny::tags$p("Database cited from the Nemaplex website (http://nemaplex.ucdavis.edu/"),
                  width="100%"
                )
              )
      )
    )
  )
)

server <- function(input, output) {
  values <- shiny::reactiveValues(basis=basis1,
                           family= family1,
                           feeding = Feeding1,
                           df_function = function1,
                           genus = genus1,
                           species = species1,
                           structure = structure1
                           )


  shiny::observeEvent(input$file1, {
    df <- utils::read.csv(input$file1$datapath, header=F)
    keywords <- df$V1
    values$basis <- filter_data(values$basis, keywords)
  })

  shiny::observeEvent(input$file2, {
    df <- utils::read.csv(input$file2$datapath, header=F)
    keywords <- df$V1
    values$family <- filter_data(values$family, keywords)
  })

  shiny::observeEvent(input$file3, {
    df <- utils::read.csv(input$file3$datapath, header=F)
    keywords <- df$V1
    values$genus <- filter_data(values$genus, keywords)
  })

  shiny::observeEvent(input$file4, {
    df <- utils::read.csv(input$file4$datapath, header=F)
    keywords <- df$V1
    values$species <- filter_data(values$species, keywords)
  })

  shiny::observeEvent(input$file5, {
    df <- utils::read.csv(input$file5$datapath, header=F)
    keywords <- df$V1
    values$feeding <- filter_data(values$feeding, keywords)
  })

  shiny::observeEvent(input$file6, {
    df <- utils::read.csv(input$file6$datapath, header=F)
    keywords <- df$V1
    values$structure <- filter_data(values$structure, keywords)
  })

  shiny::observeEvent(input$file7, {
    df <- utils::read.csv(input$file7$datapath, header=F)
    keywords <- df$V1
    values$df_function <- filter_data(values$df_function, keywords)
  })

  shiny::observeEvent(input$resetBasis, {
    values$basis <- basis1
  })

  shiny::observeEvent(input$resetFamily, {
    values$family <- family1
  })

  shiny::observeEvent(input$resetGenus, {
    values$genus <- genus1
  })

  shiny::observeEvent(input$resetSpecies, {
    values$species <- species1
  })

  shiny::observeEvent(input$resetFeeding, {
    values$feeding <- Feeding1
  })

  shiny::observeEvent(input$resetStructure, {
    values$structure <- structure1
  })

  shiny::observeEvent(input$resetFunction, {
    values$df_function <- function1
  })



  output$table1 <- DT::renderDT({
      values$basis
  },  options = list(scrollX = T))


  output$table2 <- DT::renderDT({
    values$family
  },  options = list(scrollX = T))

  output$table3 <- DT::renderDT({
    values$genus
  },  options = list(scrollX = T))

  output$table4 <- DT::renderDT({
    values$species
  },  options = list(scrollX = T))

  output$table5 <- DT::renderDT({
    values$feeding
  },  options = list(scrollX = T))

  output$table6 <- DT::renderDT({
    values$structure
  },  options = list(scrollX = T))

  output$table7 <- DT::renderDT({
    values$df_function
  },  options = list(scrollX = T))

  output$downloadData1 <- shiny::downloadHandler(
    filename = function() {
      "basis_new.csv"
    },
    content = function(file) {
      utils::write.csv(values$basis, file, row.names = FALSE, quote=F, na = "")
    }
  )

  output$downloadData2 <- shiny::downloadHandler(
    filename = function() {
      "family_new.csv"
    },
    content = function(file) {
      utils::write.csv(values$family, file, row.names = FALSE, quote=F, na = "")
    }
  )

  output$downloadData3 <- shiny::downloadHandler(
    filename = function() {
      "genus_new.csv"
    },
    content = function(file) {
      utils::write.csv(values$genus, file, row.names = FALSE, quote=F, na = "")
    }
  )

  output$downloadData4 <- shiny::downloadHandler(
    filename = function() {
      "species_new.csv"
    },
    content = function(file) {
      utils::write.csv(values$species, file, row.names = FALSE, quote=F, na = "")
    }
  )

  output$downloadData5 <- shiny::downloadHandler(
    filename = function() {
      "feeding_new.csv"
    },
    content = function(file) {
      utils::write.csv(values$feeding, file, row.names = FALSE, quote=F, na = "")
    }
  )

  output$downloadData6 <- shiny::downloadHandler(
    filename = function() {
      "structure_new.csv"
    },
    content = function(file) {
      utils::write.csv(values$structure, file, row.names = FALSE, quote=F, na = "")
    }
  )

  output$downloadData7 <- shiny::downloadHandler(
    filename = function() {
      "function_new.csv"
    },
    content = function(file) {
      utils::write.csv(values$df_function, file, row.names = FALSE, quote=F, na = "")
    }
  )
}

shiny::shinyApp(ui, server)
}
