#Library Shiny
library(shiny)
library(shinyAce)
library(shinyWidgets)
library(shinydashboard)
library(shinycssloaders)
library(shinyscreenshot)
library(shinythemes)
library(rsconnect)
#Library Dataset
library(kableExtra)
library(readr)
library(DT)
library(dplyr)
library(skimr)
library(visdat)
#Library Markdown
library(rmarkdown)
library(tinytex)
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
        menuItem("Tutorial", icon = icon("youtube"), href ="https://youtu.be/sq0bUkDNBsA"),
        menuItem("Dataset", tabName = "Dataset"),
        menuItem("Pre-Processing", tabName = "Variabel"),
        menuItem("PCA", dropdownButton = TRUE,
                 menuSubItem("Data PCA", tabName = "Data2"),
                 menuSubItem("Komputasi PCA", tabName = "Komputasi"), 
                 menuSubItem("Visualisasi PCA", tabName = "Visualisasi")
        ),
        menuItem("Data Report", tabName = "DataReport"),
        menuItem("Source Code", icon = icon("github"), href = "https://github.com/EmHaEr/Adami_R-Shiny")
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
                    h3("Tampilan Dataset", style=
                         "text-align:center"),
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
        
        tabItem(tabName = "Data2",
                fluidRow(
                  h2("Dataset", style = "text-align:center"),
                  box(
                    title = "Profil Dataset", background = "blue", solidHeader = TRUE,
                    collapsible = TRUE, width = 12,
                    div(withSpinner(verbatimTextOutput("profil_dataset"))),
                    div(actionButton('cetak_gambar1','Download', icon = icon('download'), style="display: block; margin-left: auto; margin-right: auto;")),
                  ),
                  box(
                    title = "Summary Dataset", background = "blue", solidHeader = TRUE,
                    collapsible = TRUE, width = 12,
                    div(withSpinner(verbatimTextOutput("summary_dataset"))),
                    div(actionButton('cetak_gambar2','Download', icon = icon('download'), style="display: block; margin-left: auto; margin-right: auto;")),
                  ),
                  box(
                    title = "Histogram Dataset", background = "blue", solidHeader = TRUE,
                    collapsible = TRUE, width = 12,
                    div(withSpinner(plotOutput("hist_dataset"))),
                    div(actionButton('cetak_gambar3','Download', icon = icon('download'), style="display: block; margin-left: auto; margin-right: auto;")),
                  )
                )
        ), #tabname Data PCA
      
        tabItem(tabName = "Komputasi",
                fluidRow(
                  h2("Komputasi", style = "text-align:center"),
                  box(
                    title = "Eigenvalue", background = "blue", solidHeader = TRUE,
                    collapsible = TRUE, width = 12,
                    div(withSpinner(verbatimTextOutput("eigenvalue_PCA"))),
                    h5("Nilai faktor variabel yang dijelaskan oleh masing-masing PC",
                       style= "text-align:center"),
                    div(actionButton('cetak_gambar4','Download', icon = icon('download'), style="display: block; margin-left: auto; margin-right: auto;")),
                  ),
                  box(
                    title = "Coordinates PCA", background = "blue", solidHeader = TRUE,
                    collapsible = TRUE, width = 12,
                    div(withSpinner(verbatimTextOutput("coord_PCA"))),
                    h5("Nilai komponen yang menggambarkan titik asal (koordinat) pada tiap variabel pada PC",
                       style= "text-align:center"),
                    div(actionButton('cetak_gambar5','Download', icon = icon('download'), style="display: block; margin-left: auto; margin-right: auto;")),
                  ),
                  box(
                    title = "Squared Cosine PCA", background = "blue", solidHeader = TRUE,
                    collapsible = TRUE, width = 12,
                    div(withSpinner(verbatimTextOutput("cos2_PCA"))),
                    h5("Nilai komponen terhadap kuadrat jarak observasi (kurva cosinus) ke coordinates pada PC",
                       style= "text-align:center"),
                    div(actionButton('cetak_gambar6','Download', icon = icon('download'), style="display: block; margin-left: auto; margin-right: auto;")),
                  ),
                  box(
                    title = "Contribution PCA", background = "blue", solidHeader = TRUE,
                    collapsible = TRUE, width = 12,
                    div(withSpinner(verbatimTextOutput("contrib_PCA"))),
                    h5("Nilai komponen yang diperoleh dari perbandingan nilai komponen cos2 dengan eigenvalue pada PC",
                       style= "text-align:center"),
                    div(actionButton('cetak_gambar7','Download', icon = icon('download'), style="display: block; margin-left: auto; margin-right: auto;")),
                  ),
                  box(
                    title = "Summary PCA", background = "blue", solidHeader = TRUE,
                    collapsible = TRUE, width = 12,
                    div(withSpinner(verbatimTextOutput("hasil_summaryPCA"))),
                    h5("Rangkuman nilai statistik PCA pada tiap dimensi PC",
                       style= "text-align:center"),
                    div(actionButton('cetak_gambar12','Download', icon = icon('download'), style="display: block; margin-left: auto; margin-right: auto;")),
                  ),
                  box(
                    title = "Model PCA", background = "blue", solidHeader = TRUE,
                    collapsible = TRUE, width = 12,
                    div(class = "hbar", withSpinner(DT::DTOutput("hasil_modelPCA"))),
                    h5("Sebaran data yang telah terstandarisasi pada tiap dimensi PC",
                       style= "text-align:center"),
                    div(style = "text-align:center", downloadButton("download_model", "Download Data")),
                  ),
                  box(
                    title = "Predict PCA", background = "blue", solidHeader = TRUE,
                    collapsible = TRUE, width = 12,
                    div(class = "hbar", withSpinner(DT::DTOutput("hasil_predictPCA"))),
                    h5("Sebaran data baru yang telah dianalisis dari hasil tiap dimensi PC",
                       style= "text-align:center"),
                    div(style = "text-align:center", downloadButton("download_predict", "Download Data")),
                  ),
                )
        ), #tabname Komputasi
        
        tabItem(tabName = "Visualisasi",
                fluidRow(
                    h2("Visualisasi", style = "text-align:center"),
                    box(
                      title = "Scree Plot", background = "blue", solidHeader = TRUE,
                      collapsible = TRUE, width = 6,
                      div(withSpinner(plotOutput("screeplot_PCA"))),
                      h5("Menggambarkan nilai dari eigenvalue tiap variabel ke dalam bentuk dimensi plot PC",
                         style= "text-align:center"),
                      div(actionButton('cetak_gambar8','Download', icon = icon('download'), style="display: block; margin-left: auto; margin-right: auto;")),
                    ),
                    box(
                      title = "Squared Cosine Plot", background = "blue", solidHeader = TRUE,
                      collapsible = TRUE, width = 6,
                      div(withSpinner(plotOutput("cos2plot_PCA"))),
                    ),
                    box(
                      title = "Contrib Plot", background = "blue", solidHeader = TRUE,
                      collapsible = TRUE, width = 6,
                      div(withSpinner(plotOutput("contribplot_PCA"))),
                    ),
                    box(
                      title = "Individuals Plot", background = "blue", solidHeader = TRUE,
                      collapsible = TRUE, width = 12,
                      div(withSpinner(plotOutput("individuals_PCA"))),
                      h5("Menggambarkan nilai dari cos2 ke tiap instance PC",
                         style= "text-align:center"),
                      div(actionButton('cetak_gambar9','Download', icon = icon('download'), style="display: block; margin-left: auto; margin-right: auto;")),
                    ),
                    box(
                      title = "Variables Plot", background = "blue", solidHeader = TRUE,
                      collapsible = TRUE, width = 6,
                      div(withSpinner(plotOutput("variables_PCA"))),
                      h5("Menggambarkan nilai dari contrib sebagai kontribusi ke tiap variabel PC",
                         style= "text-align:center"),
                      div(actionButton('cetak_gambar10','Download', icon = icon('download'), style="display: block; margin-left: auto; margin-right: auto;")),
                    ),
                    box(
                      title = "Biplot", background = "blue", solidHeader = TRUE,
                      collapsible = TRUE, width = 12,
                      div(withSpinner(plotOutput("biplot_PCA"))),
                      h5("Menggambarkan gabungan dari Individuals Plot dan Variables Plot",
                         style= "text-align:center"),
                      div(actionButton('cetak_gambar11','Download', icon = icon('download'), style="display: block; margin-left: auto; margin-right: auto;")),
                    ),
                  )
        ), #tabname Visualisasi
        
        tabItem(tabName = "DataReport",
                h2("Data Report", style = "text-align:center"),
                box(
                  title = "Data Report", background = "blue", solidHeader = TRUE,
                  radioButtons("reportData", "Select Output", c('HTML'), inline = TRUE),
                  div(style = "text-align:center", downloadButton("downloadData", "Download Data Report"))
                )
        ) #tabName Data Report
        
      ) #tabitems

    ) #Body
    
  ), #Page

)