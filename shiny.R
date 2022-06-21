#############################
############ UI #############
#############################

#Library Dataset
library(dplyr)
library(skimr)
library(visdat)
#Library Naive Bayes
library(leaflet)
library(tidyverse)
library(DT)
library(plotly)
library(caret)
library(stargazer)
library(pROC)
#Library PCA
library(factoextra)
library(corrplot)
library(ggplot2)

ui_shiny <- function(id)
{
  ID <- NS(id)
  fluidPage(
    navlistPanel(
      #Tab Panel Upload Data
      tabPanel("Upload",
               h3("Analisis data menggunakan metode PCA", 
                  style="
                  font-family:'arial'
                  color:black;
                  text-align:center"
               ),
               br(),
               sidebarPanel(
                 fileInput(ID("upload_data"), "Choose .txt/.csv file",
                           accept = c("text/csv",
                                      "text/comma-separated-values",
                                      ".csv"))
               ),
      ),
      #Tab Panel Dataset
      tabPanel("Dataset",
               radioButtons(ID("pemisah_variabel"), "Separator",
                            choices = c(Comma = ",",
                                        Semicolon = ";",
                                        Tab = "\t"),
                            selected = ",", inline = TRUE),
               h3("Tampilan Dataset", style=
                    "text-align:center"),
               DT::DTOutput(ID("tampilan_dataset")),
               hr(),
               h3("Profil Dataset", style=
                    "text-align:center"),
               verbatimTextOutput(ID("profil_dataset")),
               hr(),
               h3("Summary Dataset", style=
                    "text-align:center"),
               verbatimTextOutput(ID("summary_dataset")),
               hr(),
               h3("Histogram Dataset", style=
                    "text-align:center"),
               plotOutput(ID("hist_dataset"))
      ),
      #Tab Panel PCA
      tabPanel("PCA",
               h3("Eigenvalue", style=
                    "text-align:center"),
               verbatimTextOutput(ID("eigenvalue_PCA")),
               h5("tes",
                  style= "text-align:center"),
               hr(),
               h3("Coordinates", style=
                    "text-align:center"),
               verbatimTextOutput(ID("coord_PCA")),
               h5("tes",
                  style= "text-align:center"),
               hr(),
               h3("Cos 2", style=
                    "text-align:center"),
               verbatimTextOutput(ID("cos2_PCA")),
               h5("tes",
                  style= "text-align:center"),
               hr(),
               h3("Contribution", style=
                    "text-align:center"),
               verbatimTextOutput(ID("contrib_PCA")),
               h5("tes",
                  style= "text-align:center"),
               hr(),
               h3("Summary", style=
                    "text-align:center"),
               verbatimTextOutput(ID("hasil_summaryPCA")),
               h5("tes",
                  style= "text-align:center"),
               hr(),
               h3("Model", style=
                    "text-align:center"),
               DT::DTOutput(ID("hasil_modelPCA")),
               h5("tes",
                  style= "text-align:center"),
               hr(),
               h3("Predict", style=
                    "text-align:center"),
               DT::DTOutput(ID("hasil_predictPCA")),
               h5("tes",
                  style= "text-align:center"),
      ),
      #Tab Panel Visualisasi PCA
      tabPanel("Visualisasi",
               h3("Scree Plot", style=
                    "text-align:center"),
               plotOutput(ID("screeplot_PCA")),
               h5("Menggambarkan nilai dari Eigen Values tiap variabel ke dalam bentuk dimensi plot",
                  style= "text-align:center"),
               hr(),
               h3("Cos2 Plot", style=
                    "text-align:center"),
               plotOutput(ID("cos2plot_PCA")),
               h3("Individuals Plot", style=
                    "text-align:center"),
               plotOutput(ID("individuals_PCA")),
               h5("Menggambarkan nilai dari cos2 ke tiap dimensi plot",
                  style= "text-align:center"),
               hr(),
               h3("Contrib Plot", style=
                    "text-align:center"),
               plotOutput(ID("contribplot_PCA")),
               h3("Variables Plot", style=
                    "text-align:center"),
               plotOutput(ID("variables_PCA")),
               h5("Menggambarkan nilai dari contrib sebagai kontribusi PCA ke tiap dimensi plot",
                  style= "text-align:center"),
               hr(),
               h3("Biplot", style=
                    "text-align:center"),
               plotOutput(ID("biplot_PCA")),

               h5("Menggambarkan gabungan dari Individuals Plot dan Variables Plot",
                  style= "text-align:center"),
      )
    ) #navlistPanel
  ) #fluidPage
} #function

server_shiny <- function(input, output, session)
{
#############################
######## Naive Bayes ########
#############################
  myData <- reactive(
    {
      take_file <- input$take_file
      
      if (is.null(take_file))
        return(NULL)
      
      d <- read.csv(take_file$datapath, sep = input$pemisah_variabel)
      return(d)
    }) 
  
  output$tampilan_dataset2 <- DT::renderDT(
    {
      import_data <- myData()
      
      DT::datatable(import_data)
    })
  
  output$profil_dataset2 <- renderPrint(
    {
      profil_dataset2 <- myData()
      
      glimpse(profil_dataset2)
    })
  
  output$summary_dataset2 <- renderPrint(
    {
      summary_dataset2 <- myData()
      
      summary(summary_dataset2)
      skim(summary_dataset2)
    })

#############################
########### PCA #############
#############################
  dataset <- reactive(
    {
      upload_data <- input$upload_data
      
      if(is.null(upload_data))
        return(NULL)
      
      data = read.csv(upload_data$datapath, sep = input$pemisah_variabel, 
                      row.names = 1)
      return(data)
    })
  
  output$tampilan_dataset <- DT::renderDT(
    {
      data_terpilih <- dataset()
      
      DT::datatable(data_terpilih)
    })
  
  output$profil_dataset <- renderPrint(
    {
      profil_dataset <- dataset()
      
      glimpse(profil_dataset)
    })
  
  output$summary_dataset <- renderPrint(
    {
      summary_dataset <- dataset()
      
      summary(summary_dataset)
      skim(summary_dataset)
    })
  
  output$hist_dataset <- renderPlot(
    {
      hist_dataset <- dataset()
      
      vis_miss(hist_dataset)
      vis_dat(hist_dataset)
      
      par(mfrow=c(3,4))
      for(i in 1:ncol(hist_dataset)) {
        hist(hist_dataset[, i], main = paste(colnames(hist_dataset[i])), xlab = "")
      }
    })
  
  output$hasil_modelPCA <- DT::renderDT(
    {
      hasil_modelPCA <- dataset()
      
      pcaModel <- prcomp(hasil_modelPCA, scale. = TRUE, center = TRUE)
      pcaModel$rotation
    })
  
  output$hasil_summaryPCA <- renderPrint(
    {
      hasil_summaryPCA <- dataset()
      
      pcaModel <- prcomp(hasil_summaryPCA, scale. = TRUE, center = TRUE)
      pcaModel$rotation
      
      summary(pcaModel)
    })
  
  output$hasil_predictPCA <- DT::renderDT(
    {
      hasil_predictPCA <- dataset()
      
      pcaModel <- prcomp(hasil_predictPCA, scale. = TRUE, center = TRUE)
      pcaModel$rotation
      
      predict_PCA=predict(pcaModel, newdata=hasil_predictPCA)
    })
  
  output$eigenvalue_PCA <- renderPrint(
    {
      eigenvalue_PCA <- dataset()
      
      modelPCA <- prcomp(eigenvalue_PCA, scale. = TRUE, center = TRUE)
      modelPCA$rotation
      
      get_eigenvalue(modelPCA)
    })
  
  output$coord_PCA <- renderPrint(
    {
      coord_PCA <- dataset()
      
      modelPCA <- prcomp(coord_PCA, scale. = TRUE, center = TRUE)
      modelPCA$rotation
      
      var <- get_pca_var(modelPCA)
      var
      
      head(var$coord)
    })
  
  output$cos2_PCA <- renderPrint(
    {
      cos2_PCA <- dataset()
      
      modelPCA <- prcomp(cos2_PCA, scale. = TRUE, center = TRUE)
      modelPCA$rotation
      
      var <- get_pca_var(modelPCA)
      var
      
      head(var$cos2)
    })
  
  output$contrib_PCA <- renderPrint(
    {
      contrib_PCA <- dataset()
      
      modelPCA <- prcomp(contrib_PCA, scale. = TRUE, center = TRUE)
      modelPCA$rotation
      
      var <- get_pca_var(modelPCA)
      var
      
      head(var$contrib)
    })
  
  output$screeplot_PCA <- renderPlot(
    {
      screeplot_PCA <- dataset()
      
      modelPCA <- prcomp(screeplot_PCA, scale. = TRUE, center = TRUE)
      modelPCA$rotation
      
      fviz_eig(modelPCA)
    })
  
  output$cos2plot_PCA <- renderPlot(
    {
      cos2plot_PCA <- dataset()
      
      modelPCA <- prcomp(cos2plot_PCA, scale. = TRUE, center = TRUE)
      modelPCA$rotation
      
      fviz_cos2(modelPCA, choice = "var", axes = 1:2)
    })
  
  output$individuals_PCA <- renderPlot(
    {
      biplot_PCA <- dataset()
      
      modelPCA <- prcomp(biplot_PCA, scale. = TRUE, center = TRUE)
      modelPCA$rotation
      
      fviz_eig(modelPCA)
      
      fviz_pca_ind(modelPCA, repel = TRUE,
                   col.ind = "cos2",
                   gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
      )
    })
  
  output$contribplot_PCA <- renderPlot(
    {
      contribplot_PCA <- dataset()
      
      modelPCA <- prcomp(contribplot_PCA, scale. = TRUE, center = TRUE)
      modelPCA$rotation
      
      fviz_contrib(modelPCA, choice = "var", axes = 1:2, top = 10)
    })
  
  output$variables_PCA <- renderPlot(
    {
      variables_PCA <- dataset()
      
      modelPCA <- prcomp(variables_PCA, scale. = TRUE, center = TRUE)
      modelPCA$rotation
      
      fviz_eig(modelPCA)
      
      fviz_pca_var(modelPCA, repel = TRUE,
                   col.var = "contrib",
                   gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
      )
    })
  
  output$biplot_PCA <- renderPlot(
    {
      biplot_PCA <- dataset()
      
      modelPCA <- prcomp(biplot_PCA, scale. = TRUE, center = TRUE)
      modelPCA$rotation
      
      fviz_eig(modelPCA)
      
      fviz_pca_biplot(modelPCA, repel = TRUE,
                      col.var = "blue",
                      col.ind = "#000000")
    })

} #function