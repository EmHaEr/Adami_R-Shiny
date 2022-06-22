server <- function(input, output, session)
{
  bs_themer()
  
  output$shiny <- renderUI(
    {
      source("shiny.R")
      
      callModule(server_shiny, id = "shiny")
      
      ui_shiny(id = "shiny")
    })
  
    data <- reactive(
    {
      upload_data <- input$upload_data
      if(is.null(upload_data))
        return(NULL)
      pemisah_variabel = input$pemisah_variabel
    
     if(pemisah_variabel == 'xlsx')
       {
        inFile <- input$upload_data
        if(is.null(inFile))
          return(NULL)
        file.rename(inFile$datapath,
                  paste(inFile$datapath, ".xlsx", sep=""))
        data <- read_excel(paste(inFile$datapath, ".xlsx", sep="",
                                 row.names = 1), 1)
      
        return(data)
       }
      
      if(pemisah_variabel != 'xlsx')
      {
       data <- read.csv(upload_data$datapath, sep = input$pemisah_variabel)
      
       return(data)
      }
    })
  
  output$data2 <- DT::renderDT(
    {
      data()
    })
  
  data3 <- function()
  {
    data2 <- data()
    
    data_name <- colnames(data2)
    
    return(data_name)
  }
  
  output$variable_selectizeinput <- renderUI(
    {
      if(is.null(data3()))
        return(NULL)
      
      if(is.null(length(data3()) == 0 ))
        return(NULL)
      
      selectizeInput('selected_selectizeinput', 'Pilih Variabel (tekan backspace untuk menghapus) :', 
                     choices = c(data3()), selected=c(), 
                     multiple = TRUE)
    })
  
  output$data_selectizeinput <- DT::renderDT(
    {
      data2 <- data()
      
      data_selected = data2[c(input$selected_selectizeinput)]
      
      print(data_selected)
    })
  
  

  
  output$download_data <- downloadHandler(
    filename = function()
    {
      paste("Data PCA", "csv", sep = ".")
      
<<<<<<< Updated upstream
    },
    
    content = function(file)
    {
      write.csv(data_selectizeinput(),file)
    }
  )
  
}
=======
       
     })
   
   cm3 <-
     reactive({
       sample5 <- as.factor(act())
       sample6 <- as.factor(price_predict())

       statistics3 <- confusionMatrix(
         sample5,
         sample6
       )
       data.frame(statistics3$table)


     })
   

   output$crossT <- renderPrint(
     tryCatch(CrossTable(price_predict(), act()),
              error=function(e) "Maaf, program tidak dapat memuat Confusion Matrix!")
   )
   
   output$Test <- renderPrint(
     tryCatch(cm(),
              error=function(e) "Maaf, laporan tidak tersedia karena keterbatasan library!")
     )
   output$Test2 <- renderText(
     tryCatch(cm2(),
              error=function(e) "Maaf, laporan tidak tersedia karena keterbatasan library!")
   )
   
   output$Test3 <- renderPrint(
     tryCatch(cm3(),
              error=function(e) "Maaf, laporan tidak tersedia karena keterbatasan library!")
   )
   
   output$Test4 <- renderPrint(
     tryCatch(compare(),
              error=function(e) "Maaf, laporan tidak tersedia karena keterbatasan library!")
   )
   
   
   output$profil_dataset <- renderPrint(
     {
       profil_dataset <- myData()
       glimpse(profil_dataset)
       
     })
   
   output$summary_dataset <- renderPrint(
     {
       summary_dataset <- myData()
       summary(summary_dataset)
       skim(summary_dataset)
     })
   
   output$summary <- renderPrint(
     {
     dataset <- myData()
     summary(dataset)
   })
   
   #Predict MyTest
   MyTestData <- reactive({
     actmytest <- myTest()
   })
   
   output$predict_myTest <- renderText(
     tryCatch(paste0(1:nrow(MyTestData())," ", input$SelectY, " : ",predict(NB_Model(), MyTestData()), sep = "\n"),
              error=function(e) "Harap import data yang akan diprediksi!")
   )
   
   # output$report = downloadHandler(
   #   filename = 'report.txt',
   #   
   #   content = function(file) {
   #     V1 <- "REPORT \n\n----------------\n Predictor:"
   #     V2 <- input$SelectX
   #     V3 <- "\n\n----------------\n Prediction:"
   #     V4 <- input$SelectY
   #     V5 <- "\n\n----------------\n Data Summary: \n"
   #     V6 <- summary(InputDataset())
   #     V7 <- "\n\n----------------\n Model: \n"
   #     V8 <- summary(NB_Model())
   #     # V9 <- "\n\n----------------\n Profile Dataset: \n"
   #     # V10 <- profil_dataset
   #     textfile=file.path("tuning_parameter.txt");
   #     printer = file(textfile,"a+");
   #     write(c(V1,V2,V3,V4,V5,V6,V7,V8,V9,V10), textfile,sep = " ",append = TRUE, ncolumns = 6);
   #     write("\n", textfile, append=TRUE)
   #     close(printer)
   #     out = textfile
   #     
   #     file.rename(out, file) # move pdf to file for downloading
   #   },
   #   
   #   contentType = 'application/txt'
   # )
   
   myResult <- reactive({
     dataAkhir <- myTest()
     dataAkhir[input$SelectY] <- predict(NB_Model(), MyTestData())
     dataAkhir
     
   })

   
   
   output$downloadDataBaru <-
     downloadHandler(
       filename = function () {
         paste("Data Hasil Prediksi.csv", sep = "")
       },
       content = function(file) {
         write.csv(myResult(), file)
       }
     )
   
   output$downloadDataBaru2 <-
     downloadHandler(
       filename = function () {
         paste("Data Filter.csv", sep = "")
       },
       content = function(file) {
         write.csv(input$SelectX, file)
       }
     )
   
   
   output$download123 <- downloadHandler(
     filename = function() {
       paste0('report_', Sys.Date(), '.', switch(
         input$report123, PDF = 'pdf', HTML = 'html', Word = 'docx'
       ))
     },
     
     content = function(file) {
       src <- normalizePath('markdown.Rmd')
       
       # temporarily switch to the temp dir, in case you do not have write
       # permission to the current working directory
       owd <- setwd(tempdir())
       on.exit(setwd(owd))
       file.copy(src, 'markdown.Rmd')
       
       library(rmarkdown)
       out <- render(input = 'markdown.Rmd',
                     output_format = switch(
                       input$report123, 
                       PDF = pdf_document(), 
                       HTML = html_document(),
                       Word = word_document()
                     )
       )
       file.rename(out, file)
     }
   )
   
#################################################   PCA######################################
    
   output$shiny <- renderUI(
     {
       source("shiny.R")
       
       callModule(server_shiny, id = "shiny")
       
       ui_shiny(id = "shiny")
     })
   
   data <- reactive(
     {
       upload_data <- input$upload_data
       if(is.null(upload_data))
         return(NULL)
       pemisah_variabel = input$pemisah_variabel
       
       if(pemisah_variabel == 'xlsx')
       {
         inFile <- input$upload_data
         if(is.null(inFile))
           return(NULL)
         file.rename(inFile$datapath,
                     paste(inFile$datapath, ".xlsx", sep=""))
         data <- read_excel(paste(inFile$datapath, ".xlsx", sep="",
                                  row.names = 1), 1)
         
         return(data)
       }
       
       if(pemisah_variabel != 'xlsx')
       {
         data <- read.csv(upload_data$datapath, sep = input$pemisah_variabel)
         
         return(data)
       }
     })
   
   output$data2 <- DT::renderDT(
     {
       data()
     })
   
   data3 <- function()
   {
     data2 <- data()
     
     data_name <- colnames(data2)
     
     return(data_name)
   }
   
   output$variable_selectizeinput <- renderUI(
     {
       if(is.null(data3()))
         return(NULL)
       
       if(is.null(length(data3()) == 0 ))
         return(NULL)
       
       selectizeInput('selected_selectizeinput', 'Pilih Variabel (tekan backspace untuk menghapus) :', 
                      choices = c(data3()), selected=c(), 
                      multiple = TRUE)
     })
   
   output$data_selectizeinput <- DT::renderDT(
     {
       data2 <- data()
       
       data_selected = data2[c(input$selected_selectizeinput)]
       
       print(data_selected)
     })
   
   
   
   
   output$download_data <- downloadHandler(
     filename = function()
     {
       paste("Data PCA", "csv", sep = ".")
       
     },
     
     content = function(file)
     {
       write.csv(data_selectizeinput(),file)
     }
   )

} 
)
>>>>>>> Stashed changes
