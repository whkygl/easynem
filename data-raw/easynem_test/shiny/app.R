## app.R ##
library(shinydashboard)
library(DT)
library(shiny)



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

ui <- dashboardPage(
  dashboardHeader(title = "Nemdatabase"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Basic", tabName = "basic", icon = icon("dashboard")),
      menuItem("Family", tabName = "family", icon = icon("home")),
      menuItem("Genus", tabName = "genus", icon = icon("th")),
      menuItem("Species", tabName = "species", icon = icon("list")),
      menuItem("Feeding", tabName = "feeding", icon = icon("bell")),
      menuItem("Structure", tabName = "structure", icon = icon("gear")),
      menuItem("Function", tabName = "function", icon = icon("female"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "basic",
              fluidRow(
                box(
                  fileInput("file1", "Import keywords",
                            multiple = TRUE,
                            accept = c("text/csv",
                                       "text/comma-separated-values,text/plain",
                                       ".csv")),
                  actionButton("resetBasis", "Reset"),
                  downloadButton("downloadData1", "Download"),
                  width="50%"
                )
              ),
              fluidRow(
                box(
                  DTOutput("table1"),
                  tags$p("Database cited from the Nemaplex website (http://nemaplex.ucdavis.edu/)"),
                  width="100%"
                )
              )
      ),
      tabItem(tabName = "family",
              fluidRow(
                box(
                  fileInput("file2", "Import keywords",
                            multiple = TRUE,
                            accept = c("text/csv",
                                       "text/comma-separated-values,text/plain",
                                       ".csv")),
                  actionButton("resetFamily", "Reset"),
                  downloadButton("downloadData2", "Download"),
                  width="50%"
                )
              ),
              fluidRow(
                box(
                  DTOutput("table2"),
                  tags$p("Database cited from the Nemaplex website (http://nemaplex.ucdavis.edu/"),
                  width="100%"
                )
              )
      ),
      tabItem(tabName = "genus",
              fluidRow(
                box(
                  fileInput("file3", "Import keywords",
                            multiple = TRUE,
                            accept = c("text/csv",
                                       "text/comma-separated-values,text/plain",
                                       ".csv")),
                  actionButton("resetGenus", "Reset"),
                  downloadButton("downloadData3", "Download"),
                  width="50%"
                )
              ),
              fluidRow(
                box(
                  DTOutput("table3"),
                  tags$p("Database cited from the Nemaplex website (http://nemaplex.ucdavis.edu/"),
                  width="100%"
                )
              )
      ),
      tabItem(tabName = "species",
              fluidRow(
                box(
                  fileInput("file4", "Import keywords",
                            multiple = TRUE,
                            accept = c("text/csv",
                                       "text/comma-separated-values,text/plain",
                                       ".csv")),
                  actionButton("resetSpecies", "Reset"),
                  downloadButton("downloadData4", "Download"),
                  width="50%"
                )
              ),
              fluidRow(
                box(
                  DTOutput("table4"),
                  tags$p("Database cited from the Nemaplex website (http://nemaplex.ucdavis.edu/"),
                  width="100%"
                )
              )
      ),
      tabItem(tabName = "feeding",
              fluidRow(
                box(
                  fileInput("file5", "Import keywords",
                            multiple = TRUE,
                            accept = c("text/csv",
                                       "text/comma-separated-values,text/plain",
                                       ".csv")),
                  actionButton("resetFeeding", "Reset"),
                  downloadButton("downloadData5", "Download"),
                  width="50%"
                )
              ),
              fluidRow(
                box(
                  DTOutput("table5"),
                  tags$p("Database cited from the Nemaplex website (http://nemaplex.ucdavis.edu/"),
                  width="100%"
                )
              )
      ),
      tabItem(tabName = "structure",
              fluidRow(
                box(
                  fileInput("file6", "Import keywords",
                            multiple = TRUE,
                            accept = c("text/csv",
                                       "text/comma-separated-values,text/plain",
                                       ".csv")),
                  
                  actionButton("resetStructure", "Reset"),
                  downloadButton("downloadData6", "Download"),
                  width="50%"
                )
              ),
              fluidRow(
                box(
                  DTOutput("table6"),
                  tags$p("Database cited from the Nemaplex website (http://nemaplex.ucdavis.edu/"),
                  width="100%"
                )
              )
      ),
      tabItem(tabName = "function",
              fluidRow(
                box(
                  fileInput("file7", "Import keywords",
                            multiple = TRUE,
                            accept = c("text/csv",
                                       "text/comma-separated-values,text/plain",
                                       ".csv")),
                  actionButton("resetFunction", "Reset"),
                  downloadButton("downloadData7", "Download"),
                  width="50%"
                )
              ),
              fluidRow(
                box(
                  DTOutput("table7"),
                  tags$p("Database cited from the Nemaplex website (http://nemaplex.ucdavis.edu/"),
                  width="100%"
                )
              )
      )
    )
  )
)

server <- function(input, output) {
  values <- reactiveValues(basis=read.csv("basis.csv"),
                           family= read.csv("family.csv"),
                           feeding = read.csv("Feeding.csv"),
                           df_function = read.csv("function.csv"),
                           genus = read.csv("genus.csv"),
                           species = read.csv("species.csv"),
                           structure = read.csv("structure.csv")
                           )
  
  
  observeEvent(input$file1, {
    df <- read.csv(input$file1$datapath, header=F)
    keywords <- df$V1
    values$basis <- filter_data(values$basis, keywords)
  })
  
  observeEvent(input$file2, {
    df <- read.csv(input$file2$datapath, header=F)
    keywords <- df$V1
    values$family <- filter_data(values$family, keywords)
  })
  
  observeEvent(input$file3, {
    df <- read.csv(input$file3$datapath, header=F)
    keywords <- df$V1
    values$genus <- filter_data(values$genus, keywords)
  })
  
  observeEvent(input$file4, {
    df <- read.csv(input$file4$datapath, header=F)
    keywords <- df$V1
    values$species <- filter_data(values$species, keywords)
  })
  
  observeEvent(input$file5, {
    df <- read.csv(input$file5$datapath, header=F)
    keywords <- df$V1
    values$feeding <- filter_data(values$feeding, keywords)
  })
  
  observeEvent(input$file6, {
    df <- read.csv(input$file6$datapath, header=F)
    keywords <- df$V1
    values$structure <- filter_data(values$structure, keywords)
  })
  
  observeEvent(input$file7, {
    df <- read.csv(input$file7$datapath, header=F)
    keywords <- df$V1
    values$df_function <- filter_data(values$df_function, keywords)
  })
  
  observeEvent(input$resetBasis, {
    values$basis <- read.csv("basis.csv")
  })
  
  observeEvent(input$resetFamily, {
    values$family <- read.csv("family.csv")
  })
  
  observeEvent(input$resetGenus, {
    values$genus <- read.csv("genus.csv")
  })
  
  observeEvent(input$resetSpecies, {
    values$species <- read.csv("species.csv")
  })
  
  observeEvent(input$resetFeeding, {
    values$feeding <- read.csv("Feeding.csv")
  })
  
  observeEvent(input$resetStructure, {
    values$structure <- read.csv("structure.csv")
  })
  
  observeEvent(input$resetFunction, {
    values$df_function <- read.csv("function.csv")
  })

  
  
  output$table1 <- renderDT({
      values$basis
  },  options = list(scrollX = T))
  
  
  output$table2 <- renderDT({
    values$family
  },  options = list(scrollX = T))
  
  output$table3 <- renderDT({
    values$genus
  },  options = list(scrollX = T))
  
  output$table4 <- renderDT({
    values$species
  },  options = list(scrollX = T))
  
  output$table5 <- renderDT({
    values$feeding
  },  options = list(scrollX = T))
  
  output$table6 <- renderDT({
    values$structure
  },  options = list(scrollX = T))
  
  output$table7 <- renderDT({
    values$df_function
  },  options = list(scrollX = T))
  
  output$downloadData1 <- downloadHandler(
    filename = function() {
      "basis_new.csv"
    },
    content = function(file) {
      write.csv(values$basis, file, row.names = FALSE, quote=F, na = "")
    }
  )
  
  output$downloadData2 <- downloadHandler(
    filename = function() {
      "family_new.csv"
    },
    content = function(file) {
      write.csv(values$family, file, row.names = FALSE, quote=F, na = "")
    }
  )
  
  output$downloadData3 <- downloadHandler(
    filename = function() {
      "genus_new.csv"
    },
    content = function(file) {
      write.csv(values$genus, file, row.names = FALSE, quote=F, na = "")
    }
  )
  
  output$downloadData4 <- downloadHandler(
    filename = function() {
      "species_new.csv"
    },
    content = function(file) {
      write.csv(values$species, file, row.names = FALSE, quote=F, na = "")
    }
  )
  
  output$downloadData5 <- downloadHandler(
    filename = function() {
      "feeding_new.csv"
    },
    content = function(file) {
      write.csv(values$feeding, file, row.names = FALSE, quote=F, na = "")
    }
  )
  
  output$downloadData6 <- downloadHandler(
    filename = function() {
      "structure_new.csv"
    },
    content = function(file) {
      write.csv(values$structure, file, row.names = FALSE, quote=F, na = "")
    }
  )
  
  output$downloadData7 <- downloadHandler(
    filename = function() {
      "function_new.csv"
    },
    content = function(file) {
      write.csv(values$df_function, file, row.names = FALSE, quote=F, na = "")
    }
  )
}

shinyApp(ui, server)
