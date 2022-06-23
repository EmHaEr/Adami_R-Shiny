#Library Shiny
library(shiny)
library(shinyAce)
library(shinyWidgets)
library(shinydashboard)
library(shinycssloaders)
library(shinythemes)
library(caTools)
library(maps)
library(leaflet)
library(rio)
library(DT)
library(stargazer)
library(bslib)
library(rsconnect)
#Library Dataset
library(dplyr)
library(skimr)
library(visdat)
library(datadigest)
#Library PCA
library(factoextra)
library(corrplot)
library(ggplot2)


ui <- fluidPage(
  shinythemes::themeSelector(),
  tags$style(HTML(
    ".hbar {width:100%;overflow-x: scroll;overflow-y: scroll;}",
    )
  ),

    #Output
    h2("Input Data"),
    sidebarLayout(
        sidebarPanel(
            fileInput("upload_data", "Choose .txt/.csv/.xlsx File",
                      accept = c(
                          "text/csv",
                          "text/comma-separated-values,text/plain",
                          ".csv")
            ),
            hr(),
            h6("Belum memiliki dataset ? Cari di bawah ini"),
            a(href="https://www.kaggle.com/", "Web Dataset 1"),
            a(href="https://data.world/", "Web Dataset 2"),
            a(href="", "Web Dataset 3"),
            hr(),
            radioButtons("pemisah_variabel", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ",", inline = TRUE)
        ),
        mainPanel(
          div(class = "hbar", withSpinner(DT::DTOutput("data2")))
        )
    ),
    hr(),
    
    h2("Data Pre-Processing (Pemilihan Variabel)"),
    sidebarLayout(
        sidebarPanel(
            uiOutput("variable_selectizeinput"),
            hr(),
            h6("Variabel pertama akan menjadi tabel utama pada analisis nanti"),
            hr(),
            downloadButton("download_data", "Download Data")
        ),
        mainPanel(
            div(class = "hbar", withSpinner(DTOutput("data_selectizeinput")))
        )
    ),
    hr(),
    
    h2("PCA"),
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
                         fileInput("upload_dataset", "Choose .txt/.csv file",
                                   accept = c("text/csv",
                                              "text/comma-separated-values",
                                              ".csv"))
                     ),
            ),
            #Tab Panel Dataset
            tabPanel("Dataset",
                     radioButtons("pemisah_variabel2", "Separator",
                                  choices = c(Comma = ",",
                                              Semicolon = ";",
                                              Tab = "\t"),
                                  selected = ",", inline = TRUE),
                     h3("Tampilan Dataset", style=
                            "text-align:center"),
                     div(class = "hbar", withSpinner(DT::DTOutput("tampilan_dataset"))),
                     hr(),
                     h3("Profil Dataset", style=
                            "text-align:center"),
                     verbatimTextOutput("profil_dataset"),
                     hr(),
                     h3("Summary Dataset", style=
                            "text-align:center"),
                     div(withSpinner(verbatimTextOutput("summary_dataset"))),
                     hr(),
                     h3("Histogram Dataset", style=
                            "text-align:center"),
                     div(withSpinner(plotOutput("hist_dataset")))
            ),
            #Tab Panel PCA
            tabPanel("PCA",
                     h3("Eigenvalue", style=
                            "text-align:center"),
                     div(withSpinner(verbatimTextOutput("eigenvalue_PCA"))),
                     h5("tes",
                        style= "text-align:center"),
                     hr(),
                     h3("Coordinates", style=
                            "text-align:center"),
                     div(withSpinner(verbatimTextOutput("coord_PCA"))),
                     h5("tes",
                        style= "text-align:center"),
                     hr(),
                     h3("Cos 2", style=
                            "text-align:center"),
                     div(withSpinner(verbatimTextOutput("cos2_PCA"))),
                     h5("tes",
                        style= "text-align:center"),
                     hr(),
                     h3("Contribution", style=
                            "text-align:center"),
                     div(withSpinner(verbatimTextOutput("contrib_PCA"))),
                     h5("tes",
                        style= "text-align:center"),
                     hr(),
                     h3("Summary", style=
                            "text-align:center"),
                     div(withSpinner(verbatimTextOutput("hasil_summaryPCA"))),
                     h5("tes",
                        style= "text-align:center"),
                     hr(),
                     h3("Model", style=
                            "text-align:center"),
                     div(class = "hbar", withSpinner(DT::DTOutput("hasil_modelPCA"))),
                     h5("tes",
                        style= "text-align:center"),
                     hr(),
                     h3("Predict", style=
                            "text-align:center"),
                     div(class = "hbar", withSpinner(DT::DTOutput("hasil_predictPCA"))),
                     h5("tes",
                        style= "text-align:center"),
            ),
            #Tab Panel Visualisasi PCA
            tabPanel("Visualisasi",
                     h3("Scree Plot", style=
                            "text-align:center"),
                     div(withSpinner(plotOutput("screeplot_PCA"))),
                     h5("Menggambarkan nilai dari Eigen Values tiap variabel ke dalam bentuk dimensi plot",
                        style= "text-align:center"),
                     hr(),
                     h3("Cos2 Plot", style=
                            "text-align:center"),
                     div(withSpinner(plotOutput("cos2plot_PCA"))),
                     h3("Individuals Plot", style=
                            "text-align:center"),
                     div(withSpinner(plotOutput("individuals_PCA"))),
                     h5("Menggambarkan nilai dari cos2 ke tiap dimensi plot",
                        style= "text-align:center"),
                     hr(),
                     h3("Contrib Plot", style=
                            "text-align:center"),
                     div(withSpinner(plotOutput("contribplot_PCA"))),
                     h3("Variables Plot", style=
                            "text-align:center"),
                     div(withSpinner(plotOutput("variables_PCA"))),
                     h5("Menggambarkan nilai dari contrib sebagai kontribusi PCA ke tiap dimensi plot",
                        style= "text-align:center"),
                     hr(),
                     h3("Biplot", style=
                            "text-align:center"),
                     div(withSpinner(plotOutput("biplot_PCA"))),
                     h5("Menggambarkan gabungan dari Individuals Plot dan Variables Plot",
                        style= "text-align:center"),
            )
        ) #navlistPanel
    ), #fluidPage
    hr()
)