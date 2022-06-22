# unused libraries is removed for deployer
library(shiny)
library(shinydashboard)
library(caret)
library(shinycssloaders)
library(DT)
library(skimr)
library(dplyr)
library(knitr)
library(shinythemes)
library(shinyscreenshot)
library(shinyanimate)

title <-  tags$a(href='https://scholar.google.com/scholar?hl=en&as_sdt=0%2C5&q=naive+bayes&btnG=',icon("code"),'Naive Bayes',style="color:white;")
title2 <- tags$a(href='https://scholar.google.com/scholar?hl=en&as_sdt=0%2C5&q=decision+tree&btnG=',icon("code"),'Decision Tree',style="color:white;")
title3 <- tags$a(href='https://scholar.google.com/scholar?hl=en&as_sdt=0%2C5&q=Support+Vector+Machine&btnG=',icon("code"),'Support Vector Machine',style="color:white;")
title4 <- tags$a(href='https://scholar.google.com/scholar?hl=en&as_sdt=0,5&q=K-nearest+neighbors',icon("code"),'K-Nearast Neighbors',style="color:white;")
title5 <- tags$a(href='https://scholar.google.com/scholar?hl=en&as_sdt=0%2C5&q=random+forest&btnG=',icon("code"),'Random Forest',style="color:white;")
title6 <- tags$a(href='https://scholar.google.com/scholar?hl=en&as_sdt=0%2C5&q=logistic+regression&btnG=',icon("code"),'Logistic Regression',style="color:white;")

ui <- fluidPage(
  
  
  themeSelector(),
  tags$style(HTML(
      "
      .box1 {width:100%;max-height:350px;overflow-x: scroll;overflow-y: scroll;}
      .box2 {width:100%;overflow-x: scroll;overflow-y: scroll;}
      .box3 {text-align:center;padding: 10px 0}
      .box4 {padding:0 10px;text-align:center}
      .nav-tabs-custom .nav-tabs li.active {border-top-color: #d73925;
      ",
    )
  ),
  
  
################################## Halo Nama ! #####################################################################
          div(id="shiny16",style="display: block; margin-left: auto; margin-right: auto;",h1( textOutput("txtOutput"),style="
              text-align:center; 
              text-shadow: 0 0 0.2em #F7EC09, 0 0 0.2em #F7EC09; 
              font-family:Coolvetica; "
             )
          ),
  
################################## Selamat Datang #####################################################################
        div(id="shiny17", h1("Selamat Datang di Aplikasi", style="
            text-align:center; 
            text-shadow: 0 0 0.2em #F7EC09, 0 0 0.2em #F7EC09; 
            font-family:Coolvetica
          ")
        ),
        withAnim(),
      div(id='shiny1',img(src = 'logoShadoww.png', style="
                          display: block; 
                          margin-left: auto; 
                          margin-right: auto;",
                          height=220,
                          width=325)),

  
      tags$hr(),
      
  
      navbarPage("Classification",
                 selected = "Masukkan Data",
                 inverse = T,
                 
                 
           tabPanel("Tentang", 
                    fluidPage(
                      img(src='tentangg.png', width=1600, height=550),
                      div(h4("Aplikasi ADAMI adalah aplikasi data mining berbasis WEB yang dapat membantu penggunanya dalam mengolah data yang dimiliki secara mudah.
                                 Pengolahan data pada aplikasi ADAMI menggunakan Algoritma Naive Bayes Classifier. 
                                 Aplikasi ini dirancang agar user friendly dan akan memudahkan pengguna dalam memahami setiap elemen dalam User Interface. 
                                 Aplikasi ADAMI memiliki fitur yaitu menampilkan hasil klasifikasi pada data,  serta dapat memberikan hasil prediksi yang dapat diunduh", 
                               style="
                               box-shadow:5px 5px 8px yellow, 10px 10px 8px LightGrey, 15px 15px 8px yellow; 
                               border-left: 16px solid grey; 
                               border-radius: 20px; 
                               border-style: outset; 
                               margin:auto; padding:45px; 
                               width:100%; 
                               background-color:#999999; 
                               font-size:24px;
                               font-family:Coolvetica")),
                         br(),
                         br(),
                         br(),
                         br(),
                        actionButton('help','Bantuan', icon = icon('help'),
                                     onclick = "window.open ('https://youtube.com', '_blank')",
                                      style="display: block; margin-left: auto; margin-right: auto; 
                                     background-color:white; border: 2px solid yellow; font-size:20px; padding: 14px 40px, border-radius: 12px; 
                                     box-shadow: 0 8px 16px 0 rgba(0,0,0,0.2), 0 6px 20px 0 rgba(0,0,0,0.19); width:100%;font-family:Coolvetica"),
                         br(),
                         br(),
                      
                      div(id='shiny14',h1("Filosofi Logo", style='padding-top:20px; text-align:center; text-shadow: 0 0 0.2em #F7EC09, 0 0 0.2em #F7EC09; 
                                                   font-family:Coolvetica; font-size:50px')),
                      
                      hr(),
                      
                      fluidRow(
                        column(4,
                               div(id='shiny4',img(src = 'logo1.png', style="display: block; margin-left: auto; margin-right: auto;", height=245, width=235)),
                               
                        ),
                        column(4,
                               div(id='shiny5',h1("Melambangkan Logo Shiny", style='padding-top:20px; text-align:center; text-shadow: 0 0 0.2em #F7EC09, 0 0 0.2em #F7EC09; 
                                                   font-family:Coolvetica;')),
                              
                        ),
                        column(4,
                               div(id='shiny3',img(src = 'shiny.png', style="display: block; margin-left: auto; margin-right: auto;", height=245, width=235)),
                        ),
                        
                      ),
                      
                      
                  
                      br(),
                      br(),
                      br(),
                      
                      hr(),
                      
                      fluidRow(
                        column(4,
                               div(id='shiny9',img(src = 'logo3.png', style="display: block; margin-left: auto; margin-right: auto;", height=235, width=99)),
                               
                        ),
                        column(4,
                               div(id='shiny8',h1("Melambangkan Database", style='padding-top:20px; text-align:center; text-shadow: 0 0 0.2em #F7EC09, 0 0 0.2em #F7EC09; 
                                                    font-family:Coolvetica;')),
                               
                        ),
                        column(4,
                               div(id='shiny6',img(src = 'logo4.png', style="display: block; margin-left: auto; margin-right: auto;", height=235, width=235)),
                               
                        ),
                      ),
                      br(),
                    br(),
                    br(),
                    hr(),
                      
                      fluidRow(
                        column(4,
                               div(id='shiny11',img(src = 'logo5.png', style="display: block; margin-left: auto; margin-right: auto;", height=150, width=300)),
                               
                        ),
                        column(4,
                               div(id='shiny12',h1("Melambangkan alat mining", style='padding-top:20px; text-align:center; text-shadow: 0 0 0.2em #F7EC09, 0 0 0.2em #F7EC09; 
                                                     font-family:Coolvetica;')),
                               
                        ),
                        column(4,
                               div(id='shiny13',img(src = 'logo6.png', style="display: block; margin-left: auto; margin-right: auto;", height=235, width=235)),
                        ),
                        
                      ),
                      
                    hr(),
                    
                    div(id='shiny18',h1("ADAMI Merupakan Aplikasi Data Mining yang Dikembangkan Menggunakan Shiny Framework", style='padding-top:20px; text-align:center; text-shadow: 0 0 0.2em #F7EC09, 0 0 0.2em #F7EC09; 
                                                   font-family:Coolvetica;')),
                    
                      
                  
                      
                         hr(),
                          
                      ),
                    
           ), #Akhir Tab Panel Tentang
           
           
                 
                 
          tabPanel("Masukkan Data", style="font-family:Coolvetica",
        sidebarPanel(
            textInput("txtInput","Masukkan Nama Anda (Tidak Wajib)", ),
            helpText("Masukkan Dataset Anda (Ukuran maksimal 5MB)"),
            fileInput("take_file", "Choose .txt/.csv/ File",
            accept = c(
            "text/csv",
            "text/comma-separated-values,text/plain",
            ".csv",
            ".xlsx")
            
            
            ),
            
            radioButtons("pemisah_variabel", "Separator",
              choices = c(Comma = ",",
              Semicolon = ";",
              Tab = "\t"
              ),
              selected = ",", inline = TRUE
              ),
            h6("Belum memiliki dataset?"),
            a(href="https://www.kaggle.com", "[Web Dataset 1]", style='color:red'),
            a(href="https://archive.ics.uci.edu/ml/index.php", "[Web Dataset 2]",style='color:red'),
            a(href="https://data.world/", "[Web Dataset 3]",style='color:red'),
            
            br(),
            br(),
            br(),
            
            helpText("Didukung oleh :", a("Shiny", href = "http://shiny.rstudio.com")),
            br(),
            tags$img(id = 'shiny2',src = 'shiny.png', height=140, width=126),
            
       ), #Akhir sidebarpanel (tentang)
          
        
            
            div(id="shiny15",h3("Berikut merupakan data Anda", style="
            color: black;
            text-align:center;
            font-family:Coolvetica;
            text-shadow: 0 0 0.2em #F7EC09, 0 0 0.2em #F7EC09;"
               
            )
            ),
            
            mainPanel(
              box(div(class="box2",DT::DTOutput("show_data")),
                  width = 12,
                  height = "800")
            ),
        ), #Akhir Tab Panel Masukkan Data
       
       
        tabPanel("Naive Bayes", style="font-family:Coolvetica", 
         dashboardPage( skin = "yellow",
           dashboardHeader(title = title, titleWidth = 500,
                           
                           # tags$li(class="dropdown", tags$a(href="https://www.youtube.com",icon("calender"), "Bantuan", target="_blank"),
 
               dropdownMenu(type = "message",
                        messageItem(from = "Developer",message = "Happy Classify!")
                      ),
            
        
               dropdownMenuOutput("msgOutput")
          
          ), #Akhir dashboard header
          
          
           dashboardSidebar(
             tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: Grey}")),
             sliderInput(
               "Slider1",
               label = h3("Train/Test Split %"),
               min = 0,
               max = 100,
               value = 75
             ),
             
             
             div(class="box4",textOutput("cntTrain")),
             div(class="box4",textOutput("cntTest")),
             br(),
             br(),
             br(),
             div(class="box4",id="shiny19",a(href="https://youtube.com/", " Video Panduan",style='color:red; font-size:24px;')),
             div(class="box4", helpText("tekan text merah")),
             br()
             
             
             
           ),
           dashboardBody(
             fluidRow(
                 div(helpText("Jangan Input Variabel Target (Y) kedalam Variabel Predictor (X)"), style = "text-align:center"),
                 uiOutput("SelectX"),
                 div(checkboxInput('all', 'Pilih Semua Predictor(X)', value = FALSE), style = "text-align:center"),
                 
                 uiOutput("SelectY"),
             ),
             
             
             fluidPage(  
               tabBox(
                 id = "tabset1",
                
                 height = "1600",
                 width = 12,
                 tabPanel("Data",
                          div(class="box2",withSpinner(DTOutput("Data"))),
                          div(downloadButton('downloadDataBaru2',label="Data Filter"))),
                 
                 
                 tabPanel(
                   "Dataset Information",
                   box(
                     width = 12,
                     title = "Overall Data Summary",
                     status = "warning",
                     solidHeader = T,
                     collapsible = T,
                     div(class="box1",withSpinner(verbatimTextOutput("summary_dataset"))),
                     actionButton('cetak_gambar1','Download', icon = icon('download'), style="float:right" ),
                   ),
              
                   box(
                     title = "Attribute Summary",
                     status = "warning",
                     solidHeader = T,
                     collapsible = T,
                     div(class="box1",withSpinner(verbatimTextOutput("summary"))),
                     width = 12,
                     actionButton('cetak_gambar10','Download', icon = icon('download'), style="float:right" ),
                   ),
                   
                   box(
                     title = "Profile Dataset",
                     status = "warning",
                     solidHeader = T,
                     collapsible = T,
                     div(class="box1",withSpinner(verbatimTextOutput("profil_dataset"))),
                     width = 12,
                     actionButton('cetak_gambar2','Download', icon = icon('download'), style="float:right" ),
                   ),
                   
                 ),#Akhir Tab panel Dataset Information
                 
                 
                 tabPanel(
                   "Model",
                   box(
                     div(class ="box1",
                         withSpinner(verbatimTextOutput("Model"))),
                     width = 12,
                     title = "Model",
                     status = "warning",
                     solidHeader = T,
                     collapsible = T,
                     actionButton('cetak_gambar3','Download', icon = icon('download'), style="float:right" ),
                   ),
                   box(
                     title = "Summary",width = 12,
                     status = "warning",
                     solidHeader = T,
                     collapsible = T,
                     div(class="box1",withSpinner(verbatimTextOutput("Model_summary"))),
                     actionButton('cetak_gambar4','Download', icon = icon('download'), style="float:right" )
                   )
                   
                 ),#Akhir tab Panel Model
                 
                 
                 tabPanel(
                   "Model Assessment",
                   box(
                     div(class="box1",withSpinner(verbatimTextOutput("crossT"))),
                     width = 12, 
                     title = "Confusion Matrix (Cross Tabulasi)",
                     status = "warning",
                     solidHeader = T,
                     collapsible = T,
                     actionButton('cetak_gambar5','Download', icon = icon('download'), style="float:right" ),
                   ),
                   div(class="box3",paste("Beberapa informasi dapat tidak tersedia karena keterbasatan library")),
                   box(
                     width = 12,
                     status = "warning",
                     solidHeader = T,
                     div(class="box1",paste("Overall Statistics"), withSpinner(verbatimTextOutput("Test")),
                         paste("Class Statistics"), withSpinner(tableOutput("Test2"))),
                     actionButton('cetak_gambar7','Download(class)', icon = icon('download'), style="float:right"),
                     actionButton('cetak_gambar6','Download(overall)', icon = icon('download'), style="float:right")
                   ),
                   box(
                     width = 12,
                     status = "warning",
                     solidHeader = T,
                     div(class="box1",paste("Table"),withSpinner(verbatimTextOutput("Test3")),
                         paste("Comparison"),withSpinner(verbatimTextOutput("Test4"))),
                     actionButton('cetak_gambar8','Download(table)', icon = icon('download'), style="float:right"),
                     actionButton('cetak_gambar9','Download(comp)', icon = icon('download'), style="float:right"),
                   )
                   
                 ),#Akhir Tab Panel Model Assessment
                 
                 
                 tabPanel(
                   "Predict",
                   box(
                     status = "warning",
                     solidHeader = T,
                     div(class="box1",fileInput("take_test", "Choose .txt/.csv/ File",
                                  accept = c(
                                    "text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv")),width = 6, title = "Input data yang akan diprediksi:")),
                   box(
                     status = "warning",
                     solidHeader = T,
                     div(class="box1",radioButtons("pemisah_variabel2", "Separator",
                                choices = c(Comma = ",",
                                            Semicolon = ";",
                                            Tab = "\t"
                                            
                                            ),
                                selected = ",", inline = TRUE
                   )),width = 6, title = "Pemisah data"),
                   box(div(class="box1",withSpinner(DT::DTOutput("show_myTest"))),width=12),
                   box(
                     title = "Hasil Prediksi", width = 12,
                     status = "warning",
                     solidHeader = T,
                     div(class="box1",withSpinner(verbatimTextOutput("predict_myTest")))
                   ),
                   
                   box(
                     title = "Download Data Baru CSV",
                     status = "warning",
                     solidHeader = T,
                     div(downloadButton('downloadDataBaru',label="Predictions Results")),width=12,
                     style="text-align:center;",
                   ),
                   
                 ), #Akhir Tab Panel Predict
                 
                 tabPanel(
                   "Report",
                   
                   box(
                     div(
                       radioButtons('report123', 'Download Report Aplikasi ADAMI:', c('HTML', 'Word'), inline = TRUE),
                       downloadButton('download123',label="Report Data ADAMI")),width=12,
                     style ="text-align:center; font-size: 24px; background-color:lightGrey; box-shadow: 0px"
                   ),
                 ) #Akhir dar tab Panel Report
                 
               )# Akhir tab Box Bagian Bawah
               
               
             ) #fluidpage
             
           ) #dashboardbody
          
         )#dashboardpage
         
       ), # Akhir Tab Panel Naive Bayesmodelling 
       
       
       
                  # tabPanel("Decision Tree",
                  #     dashboardPage(
                  #                     dashboardHeader(title = title2, titleWidth = 500,
                  # 
                  #                                     dropdownMenu(type = "message",
                  #                                                  messageItem(from = "Developer",message = "Coming Soon!")
                  #                                     ),
                  # 
                  #                                     dropdownMenuOutput("msgOutput")),
                  #                     dashboardSidebar(
                  # 
                  # 
                  # 
                  #                     ),
                  #                     dashboardBody(
                  # 
                  #                       fluidRow(
                  #                         valueBox("Coming Soon","Decsion Tree", icon=icon("car-side"))
                  #                       ),
                  # 
                  # 
                  #                     ) #dashboardbody
                  #                  )
                  #   )
                  #          ,
                  # tabPanel("Support Vector Machine",
                  #          dashboardPage(
                  #                         dashboardHeader(title = title3, titleWidth = 500,
                  # 
                  #                                         dropdownMenu(type = "message",
                  #                                                      messageItem(from = "Developer",message = "Coming Soon!")
                  #                                         ),
                  # 
                  #                                         dropdownMenuOutput("msgOutput")),
                  #                         dashboardSidebar(
                  # 
                  # 
                  # 
                  #                         ),
                  #                         dashboardBody(
                  #                           fluidRow(
                  #                             valueBox("Coming Soon","Support Vector Machine", icon=icon("car-side", color = "yellow"))
                  #                           ),
                  # 
                  #                         ) #dashboardbody
                  #          )
                  # 
                  #          ),
                  # tabPanel("K-Nearast Neighbors",
                  #          dashboardPage(
                  #                         dashboardHeader(title = title4, titleWidth = 500,
                  # 
                  #                                         dropdownMenu(type = "message",
                  #                                                      messageItem(from = "Developer",message = "Coming Soon!")
                  #                                         ),
                  # 
                  #                                         dropdownMenuOutput("msgOutput")),
                  #                         dashboardSidebar(
                  # 
                  # 
                  # 
                  #                         ),
                  #                         dashboardBody(
                  #                           fluidRow(
                  #                             valueBox("Coming Soon","K-Nearast Neighbors", icon=icon("car-side", color = "yellow"))
                  #                           ),
                  # 
                  #                         ) #dashboardbody
                  #          )
                  # 
                  #          ),
                  # tabPanel("Random Forest",
                  #          dashboardPage(
                  #                         dashboardHeader(title = title5, titleWidth = 500,
                  # 
                  #                                         dropdownMenu(type = "message",
                  #                                                      messageItem(from = "Developer",message = "Coming Soon!")
                  #                                         ),
                  # 
                  #                                         dropdownMenuOutput("msgOutput")),
                  #                         dashboardSidebar(
                  # 
                  # 
                  # 
                  #                         ),
                  #                         dashboardBody(
                  #                           fluidRow(
                  #                             valueBox("Coming Soon","Random Forest", icon=icon("car-side", color = "yellow"))
                  #                           ),
                  # 
                  #                         ) #dashboardbody
                  #          )
                  # 
                  #          ),
                  # tabPanel("Logistic Regression",
                  #          dashboardPage(
                  #                         dashboardHeader(title = title6, titleWidth = 500,
                  # 
                  #                                         dropdownMenu(type = "message",
                  #                                                      messageItem(from = "Developer",message = "Coming Soon!")
                  #                                         ),
                  # 
                  #                                         dropdownMenuOutput("msgOutput")),
                  #                         dashboardSidebar(
                  # 
                  # 
                  # 
                  #                         ),
                  #                         dashboardBody(
                  #                           fluidRow(
                  #                             valueBox("Coming Soon","Logistic Regression", icon=icon("car-side", color = "yellow"))
                  #                           ),
                  # 
                  #                         ) #dashboardbody
                  #          )
                  # 
                  #          ),
       
     ),#Akhir dari Navbar PAge #navigator

  div(id='shiny10',img(src = 'logoapp.png', style="display: block; margin-left: auto; margin-right: auto;", height=220, width=235))

   ) #fluidPage()

  
  