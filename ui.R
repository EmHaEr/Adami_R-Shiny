library(shiny)
library(shinyAce)
library(shinyWidgets)
library(shinydashboard)
library(shinycssloaders)
library(shinythemes)
library(e1071)
library(caret)
library(caTools)
library(maps)
library(leaflet)
library(datadigest)
library(rio)
library(DT)
library(stargazer)
library(bslib)
library(rsconnect)

thematic::thematic_shiny(font = "auto")

ui <- fluidPage(
    theme = bs_theme(),
    #Judul Aplikasi
    h1("Aplikasi Analisis Data Menggunakan Metode PCA & Klasifikasi Data Menggunakan Metode Naive Bayes", style="
    font-family: 'cursive';
    font-weight: bold;
    font-size:50px;
    text-align:center
    "),
    
    #R Shiny
    HTML('<center><img src="rshiny.png" width="200"></center>'),
    br(),
    
    #Logo UII
    HTML('<center><img src="UII.png" width="100" height="150"></center>'),
    br(),

    #Credit
    h2("Created by : Muhd Humam Rhamadhani & Muhammad Daffa Muafa", style="
    font-family: 'cursive';
    font-size:20px;
    text-align:center
    "),
    
    #Output
    h2("Input Data"),
    sidebarLayout(
        sidebarPanel(
            fileInput("upload_data", "Choose .txt/.csv/.xlsx File",
                      accept = c(
                          "text/csv",
                          "text/comma-separated-values,text/plain",
                          ".csv",
                          ".xlsx")
            ),
            hr(),
            h6("Belum memiliki dataset ? Cari di bawah ini"),
            a(href="https://www.kaggle.com/", "Web Dataset 1"),
            a(href="https://data.world/", "Web Dataset 2"),
            hr(),
            radioButtons("pemisah_variabel", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t",
                                     xlsx = "xlsx"),
                         selected = ",", inline = TRUE)
        ),
        mainPanel(
            DT::DTOutput("data2")
        )
    ),
    h2("Data Processing (Pemilihan Variabel)"),
    sidebarLayout(
        sidebarPanel(
            uiOutput("variable_selectizeinput"),
            hr(),
            h6("Variabel pertama akan menjadi tabel utama pada analisis nanti"),
            hr(),
            downloadButton("download_data", "Download Data")
        ),
        mainPanel(
            DT::DTOutput("data_selectizeinput"),
        )
    ),
    br(),
    h2("PCA"),
    uiOutput("shiny"),
    br(),
    h2("Naive Bayes"),
    br()
)