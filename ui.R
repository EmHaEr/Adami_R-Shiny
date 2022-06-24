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
  
  dashboardPage(
    dashboardHeader(
      title = "PCA", titleWidth = 300
      
    ), #Header
    
    dashboardSidebar(width = 300,
      sidebarMenu(
        menuItem("Dataset", tabName = "Dataset"),
        menuItem("Pre-Processing", tabName = "Variabel"),
        menuItem("PCA", tabName = "PCA")
      ) #Menu
    ), #Sidebar
    
    dashboardBody(
      tabItems(
        tabItem(tabName = "Dataset",
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
                    tags$b("Belum memiliki dataset ?"),
                    br(),
                    a(href="https://www.kaggle.com/", "Kaggle"),
                    br(),
                    a(href="https://data.world/", "Data World"),
                    br(),
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
                  ),
                ),
        ), #tabname Dataset
        
        tabItem(tabName = "Variabel",
                h2("Data Pre-Processing (Pemilihan Variabel)"),
                sidebarLayout(
                  sidebarPanel(
                    uiOutput("variabel"),
                    tags$b("Nilai pada seluruh variabel data harus bernilai numerik", style=
                             "text-align:center"),
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
        ), #tabname Pre-Processing
        
        tabItem(tabName = "PCA",
                h2("PCA"),
                fluidPage(
                  navlistPanel(
                    #Tab Panel Dataset
                    tabPanel("Dataset",
                             h3("Profil Dataset", style=
                                  "text-align:center"),
                             div(withSpinner(verbatimTextOutput("profil_dataset"))),
                             div(actionButton('cetak_gambar1','Download', icon = icon('download'), style="display: block; margin-left: auto; margin-right: auto;")),
                             hr(),
                             h3("Summary Dataset", style=
                                  "text-align:center"),
                             div(withSpinner(verbatimTextOutput("summary_dataset"))),
                             div(actionButton('cetak_gambar2','Download', icon = icon('download'), style="display: block; margin-left: auto; margin-right: auto;")),
                             hr(),
                             h3("Histogram Dataset", style=
                                  "text-align:center"),
                             div(withSpinner(plotOutput("hist_dataset"))),
                             div(actionButton('cetak_gambar3','Download', icon = icon('download'), style="display: block; margin-left: auto; margin-right: auto;")),
                    ),
                    #Tab Panel PCA
                    tabPanel("PCA",
                             h3("Eigenvalue", style=
                                  "text-align:center"),
                             div(withSpinner(verbatimTextOutput("eigenvalue_PCA"))),
                             h5("Nilai faktor variabel yang dijelaskan oleh masing-masing
                        principal component ke tiap dimensi PC",
                                style= "text-align:center"),
                             div(actionButton('cetak_gambar4','Download', icon = icon('download'), style="display: block; margin-left: auto; margin-right: auto;")),
                             hr(),
                             h3("Coordinates", style=
                                  "text-align:center"),
                             div(withSpinner(verbatimTextOutput("coord_PCA"))),
                             h5("Nilai komponen yang menggambarkan titik asal (koordinat) pada tiap variabel 
                        pada principal component ke tiap dimensi PC",
                                style= "text-align:center"),
                             div(actionButton('cetak_gambar5','Download', icon = icon('download'), style="display: block; margin-left: auto; margin-right: auto;")),
                             hr(),
                             h3("Cos2", style=
                                  "text-align:center"),
                             div(withSpinner(verbatimTextOutput("cos2_PCA"))),
                             h5("Nilai komponen terhadap kuadrat jarak observasi (kurva cosinus) ke coordinates
                        pada principal component ke tiap dimensi PC",
                                style= "text-align:center"),
                             div(actionButton('cetak_gambar6','Download', icon = icon('download'), style="display: block; margin-left: auto; margin-right: auto;")),
                             hr(),
                             h3("Contribution", style=
                                  "text-align:center"),
                             div(withSpinner(verbatimTextOutput("contrib_PCA"))),
                             h5("Nilai komponen yang diperoleh dari perbandingan nilai komponen cos2 dengan eigenvalue
                        pada principal component ke tiap dimensi PC",
                                style= "text-align:center"),
                             div(actionButton('cetak_gambar7','Download', icon = icon('download'), style="display: block; margin-left: auto; margin-right: auto;")),
                             hr(),
                             h3("Summary", style=
                                  "text-align:center"),
                             div(withSpinner(verbatimTextOutput("hasil_summaryPCA"))),
                             h5("Rangkuman nilai statistik PCA pada tiap dimensi PC",
                                style= "text-align:center"),
                             hr(),
                             h3("Model", style=
                                  "text-align:center"),
                             div(class = "hbar", withSpinner(DT::DTOutput("hasil_modelPCA"))),
                             h5("tes",
                                style= "text-align:center"),
                             div(style = "text-align:center", downloadButton("download_model", "Download Data")),
                             hr(),
                             h3("Predict", style=
                                  "text-align:center"),
                             div(class = "hbar", withSpinner(DT::DTOutput("hasil_predictPCA"))),
                             h5("tes",
                                style= "text-align:center"),
                             div(style = "text-align:center", downloadButton("download_predict", "Download Data"))
                    ),
                    #Tab Panel Visualisasi PCA
                    tabPanel("Visualisasi", id = "Visualisasi",
                             h3("Scree Plot", style=
                                  "text-align:center"),
                             div(withSpinner(plotOutput("screeplot_PCA"))),
                             h5("Menggambarkan nilai dari eigenvalue tiap variabel ke dalam bentuk dimensi plot PC",
                                style= "text-align:center"),
                             hr(),
                             h3("Cos2 Plot", style=
                                  "text-align:center"),
                             div(withSpinner(plotOutput("cos2plot_PCA"))),
                             h3("Individuals Plot", style=
                                  "text-align:center"),
                             div(withSpinner(plotOutput("individuals_PCA"))),
                             h5("Menggambarkan nilai dari cos2 ke tiap instance PC",
                                style= "text-align:center"),
                             hr(),
                             h3("Contrib Plot", style=
                                  "text-align:center"),
                             div(withSpinner(plotOutput("contribplot_PCA"))),
                             h3("Variables Plot", style=
                                  "text-align:center"),
                             div(withSpinner(plotOutput("variables_PCA"))),
                             h5("Menggambarkan nilai dari contrib sebagai kontribusi ke tiap variabel PC",
                                style= "text-align:center"),
                             hr(),
                             h3("Biplot", style=
                                  "text-align:center"),
                             div(withSpinner(plotOutput("biplot_PCA"))),
                             h5("Menggambarkan gabungan dari Individuals Plot dan Variables Plot",
                                style= "text-align:center"),
                             div(actionButton('cetak_gambar8','Download', icon = icon('download'), style="display: block; margin-left: auto; margin-right: auto;")),
                    )
                  ) #navlistPanel
                ), #fluidPage
        ) #tabname PCA
      )

    ) #Body
    
  ), #Page

)