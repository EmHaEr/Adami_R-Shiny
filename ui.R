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
            fileInput("upload_data", "Choose .txt/.csv file",
                      accept = c(
                          "text/csv",
                          "text/comma-separated-values,text/plain",
                          ".csv")
            ),
            hr(),
            h4("Belum memiliki dataset ?"),
            a(href="https://www.kaggle.com/", "Kaggle"),
            a(href="https://data.world/", "Data World"),
            a(href="https://drive.google.com/file/d/1EoPQtb7iq3ZodsZqZJ1lpJxYCmfvZ6mh/view?usp=sharing", "Factoextra"),
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
          uiOutput("variabel"),
          div(checkboxInput('all', 'Pilih Semua Variabel', value = FALSE), style = "text-align:center"),
          hr(),
          div(style = "text-align:center", downloadButton("download_data", "Download Data"))
        ),
        mainPanel(
          h3("Tampilan Dataset", style=
               "text-align:center"),
          div(class = "hbar", withSpinner(DTOutput("data_selectizeinput")))
        )
    ),
    hr(),
    
    h2("PCA"),
    fluidPage(
        navlistPanel(
            #Tab Panel Dataset
            tabPanel("Dataset",
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
                     h5("Nilai faktor variabel yang dijelaskan oleh masing-masing
                        principal component ke tiap dimensi",
                        style= "text-align:center"),
                     hr(),
                     h3("Coordinates", style=
                            "text-align:center"),
                     div(withSpinner(verbatimTextOutput("coord_PCA"))),
                     h5("Nilai komponen yang menggambarkan titik asal (koordinat) pada tiap variabel 
                        pada principal component ke tiap dimensi",
                        style= "text-align:center"),
                     hr(),
                     h3("Cos2", style=
                            "text-align:center"),
                     div(withSpinner(verbatimTextOutput("cos2_PCA"))),
                     h5("Nilai komponen terhadap kuadrat jarak observasi (kurva cosinus) ke coordinates
                        pada principal component ke tiap dimensi",
                        style= "text-align:center"),
                     hr(),
                     h3("Contribution", style=
                            "text-align:center"),
                     div(withSpinner(verbatimTextOutput("contrib_PCA"))),
                     h5("Nilai komponen yang diperoleh dari perbandingan nilai komponen cos2 dengan eigenvalue
                        pada principal component ke tiap dimensi",
                        style= "text-align:center"),
                     hr(),
                     h3("Summary", style=
                            "text-align:center"),
                     div(withSpinner(verbatimTextOutput("hasil_summaryPCA"))),
                     h5("Rangkuman nilai statistik PCA pada tiap dimensi",
                        style= "text-align:center"),
                     hr(),
                     h3("Model", style=
                            "text-align:center"),
                     div(class = "hbar", withSpinner(DT::DTOutput("hasil_modelPCA"))),
                     h5("",
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
                     h5("Menggambarkan nilai dari eigenvalue tiap variabel ke dalam bentuk dimensi plot",
                        style= "text-align:center"),
                     hr(),
                     h3("Cos2 Plot", style=
                            "text-align:center"),
                     div(withSpinner(plotOutput("cos2plot_PCA"))),
                     h3("Individuals Plot", style=
                            "text-align:center"),
                     div(withSpinner(plotOutput("individuals_PCA"))),
                     h5("Menggambarkan nilai dari cos2 ke tiap instance",
                        style= "text-align:center"),
                     hr(),
                     h3("Contrib Plot", style=
                            "text-align:center"),
                     div(withSpinner(plotOutput("contribplot_PCA"))),
                     h3("Variables Plot", style=
                            "text-align:center"),
                     div(withSpinner(plotOutput("variables_PCA"))),
                     h5("Menggambarkan nilai dari contrib sebagai kontribusi PCA ke tiap variabel",
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