# load libraries
library(shiny)
library(ggplot2)


# read model file names
model_dir <- "D:/PhD related/Shiny/models"

model_files <- list.files(
  model_dir,
  pattern = "\\.rds$",
  full.names = TRUE
)

print(model_files)

species_names <- gsub(
  "\\.rds$",
  "",
  basename(model_files)
)

print(species_names)

# User Interface (UI)
ui <- fluidPage(
  
  titlePanel(
    "Grasshopper Species Distribution Models"
  ),
  
  sidebarLayout(
    
    # sidebarPanel(
    #   
    #   selectInput(
    #     "species",
    #     "Select species:",
    #     choices = species_names
    #   )
    #   
    # ),
    sidebarPanel(
      
      selectInput(
        "species",
        "Select species:",
        choices = species_names
      ),
      
      uiOutput("stats"),
      
      hr(),
      
      h4("About this App"),
      
      p("This interactive application presents species distribution models",
        "for grasshoppers of Western Australia developed as part of a PhD research project."
        ),
      
      p(
        "Explore species-specific distribution maps, model performance statistics, predictor importance, and environmental response curves."
      ),
      
      p(
        "Further details can be found in the associated ",
        tags$a(
          "PhD thesis",
          href = "https://minerva-access.unimelb.edu.au/items/d6602761-b9e9-4dc1-b52d-e0ceb9bae69a",
          target = "_blank"
        )
      ),
      
      p(
        tags$a(
          "Hosted on GitHub Repository",
          href = "https://github.com/anwar79melb",
          target = "_blank"
        )
      ),
      
      hr(),
      
      strong("Author:"),
      p("Author: Md Anwar Hossain"),
      
      strong("Institution:"),
      p("The University of Melbourne"),
      
      p(
        a(
          "Contact",
          href = "mailto:anwar.wildlife.du3@gmail.com"
        )
      )
      
    ),
    
    mainPanel(
      
      fluidRow(
        
        column(
          6,
          
          div(
            style = "margin-bottom:20px;",
            
            plotOutput(
              "sdm_plot",
              click = "sdm_click",
              height = "450px"
            ),
            
            p(
              "Click plot to enlarge",
              style = "font-size:12px; color:grey;"
            ),
            
            downloadButton(
              "download_sdm",
              "Download SDM Map",
              width = "100%"
            )
          )
        ),
        
        column(
          6,
          
          div(
            style = "margin-bottom:20px;",
            
            plotOutput(
              "roc_plot",
              click = "roc_click",
              height = "450px"
            ),
            
            p(
              "Click plot to enlarge",
              style = "font-size:12px; color:grey;"
            ),
            
            downloadButton(
              "download_roc",
              "Download ROC Curve",
              width = "100%"
            )
          )
        )
        
      ),
      
      fluidRow(
        
        column(
          6,
          
          div(
            style = "margin-bottom:20px;",
            
            plotOutput(
              "importance_plot",
              click = "importance_click",
              height = "450px"
            ),
            
            p(
              "Click plot to enlarge",
              style = "font-size:12px; color:grey;"
            ),
            
            downloadButton(
              "download_importance",
              "Download Importance Plot",
              width = "100%"
            )
          )
        ),
        
        column(
          6,
          
          div(
            style = "margin-bottom:20px;",
            
            plotOutput(
              "response_plot",
              click = "response_click",
              height = "450px"
            ),
            
            p(
              "Click plot to enlarge",
              style = "font-size:12px; color:grey;"
            ),
            
            downloadButton(
              "download_response",
              "Download Response Curves",
              width = "100%"
            )
          )
        )
        
      ),
      
    )
    
  )
  
)


# server
server <- function(input, output, session){
  
  current_species <- reactive({
    
    readRDS(
      file.path(
        model_dir,
        paste0(input$species, ".rds")
      )
    )
    
  })
  
  output$sdm_plot <- renderPlot({
    
    current_species()$predWA_plot +
      labs(
        subtitle = paste0(
          current_species()$species,
          " (sites = ",
          current_species()$occurrences,
          ", AUC-ROC = ",
          round(current_species()$auc_roc, 3),
          ")"
        )
      )
    
  })
  # zoom when clicked
  observeEvent(input$sdm_click, {
    
    showModal(
      
      modalDialog(
        
        size = "l",
        
        plotOutput(
          "sdm_plot_large",
          height = "700px"
        ),
        
        easyClose = TRUE
        
      )
      
    )
    
  })
  output$sdm_plot_large <- renderPlot({
    
    current_species()$predWA_plot
    
  })
  
  # add download handler
  output$download_sdm <- downloadHandler(
    
    filename = function() {
      
      paste0(
        current_species()$species,
        "_SDM_Map.png"
      )
      
    },
    
    content = function(file) {
      
      ggsave(
        file,
        plot = current_species()$predWA_plot,
        width = 8,
        height = 6,
        dpi = 300
      )
      
    }
  )
  
  output$roc_plot <- renderPlot({
    
    current_species()$roc_plot +
      labs(
        subtitle = paste0(
          "ROC-AUC = ",
          round(current_species()$auc_roc, 3)
        )
      )
    
  })
  
  # zoom when clicked
  observeEvent(input$roc_click, {
    
    showModal(
      
      modalDialog(
        
        size = "l",
        
        plotOutput(
          "roc_plot_large",
          height = "700px"
        ),
        
        easyClose = TRUE
        
      )
      
    )
    
  })
  
  output$roc_plot_large <- renderPlot({
    
    current_species()$roc_plot
    
  })
  
  # download handler
  output$download_roc <- downloadHandler(
    
    filename = function() {
      
      paste0(
        current_species()$species,
        "_ROC.png"
      )
      
    },
    
    content = function(file) {
      
      ggsave(
        file,
        plot = current_species()$roc_plot,
        width = 8,
        height = 6,
        dpi = 300
      )
      
    }
  )
  
  output$importance_plot <- renderPlot({
    
    current_species()$importance_plot
    
  })
  # zoom when clicked
  observeEvent(input$importance_click, {
    
    showModal(
      
      modalDialog(
        
        size = "l",
        
        plotOutput(
          "importance_plot_large",
          height = "700px"
        ),
        
        easyClose = TRUE
        
      )
      
    )
    
  })
  
  output$importance_plot_large <- renderPlot({
    
    current_species()$importance_plot
    
  })
  
  # download handler
  output$download_importance <- downloadHandler(
    
    filename = function() {
      
      paste0(
        current_species()$species,
        "_Importance.png"
      )
      
    },
    
    content = function(file) {
      
      ggsave(
        file,
        plot = current_species()$importance_plot,
        width = 8,
        height = 6,
        dpi = 300
      )
      
    }
  )
  
  
  output$response_plot <- renderPlot({
    
    current_species()$response_plot_facets
    
  })
  
  # zoom when clicked
  observeEvent(input$response_click, {
    
    showModal(
      
      modalDialog(
        
        size = "l",
        
        plotOutput(
          "response_plot_large",
          height = "700px"
        ),
        
        easyClose = TRUE
        
      )
      
    )
    
  })
  
  output$response_plot_large <- renderPlot({
    
    current_species()$response_plot_facets
    
  })
  
  
  
  # download handler
  output$download_response <- downloadHandler(
    
    filename = function() {
      
      paste0(
        current_species()$species,
        "_ResponseCurves.png"
      )
      
    },
    
    content = function(file) {
      
      ggsave(
        file,
        plot = current_species()$response_plot_facets,
        width = 10,
        height = 8,
        dpi = 300
      )
      
    }
  )
  
  output$stats <- renderUI({
    
    req(current_species())
    
    x <- current_species()
    
    div(
      
      style = "
      background:#eef5ff;
      border:1px solid #d6e4ff;
      border-radius:8px;
      padding:8px;
      margin-bottom:5px;
      font-size:14px;
      line-height:1.3;
    ",
      
      h4(
        "Species Summary",
        style = "margin-top:0px; margin-bottom:8px;"
      ),
      
      HTML(
        paste0(
          "<b>Species:</b> ", x$species, "<br>",
          "<b>Occurrences:</b> ", x$occurrences, "<br>",
          "<b>ROC-AUC:</b> ", round(x$auc_roc, 3), "<br>",
          "<b>PR-AUC:</b> ", round(x$auc_pr, 3)
        )
      )
      
    )
    
  })
  
}


# run app
shinyApp(ui, server)
