# unused libraries is removed for deployer
library(shiny)
library(shinydashboard)
library(caret)
library(shinycssloaders)
library(DT)
library(skimr)
library(dplyr)
library(gmodels)
library(broom)
library(kableExtra)



shinyServer(function(input, output, session) {
  
  # Data Import ETL
  # Import Data for training and testing
  myData <- reactive({
    take_file <- input$take_file
    if (is.null(take_file))
      return(NULL)
    p <- read.csv(take_file$datapath, sep = input$pemisah_variabel)
    return(p)
  }) 
  # myData <- reactive({
  #   take_file <- input$take_file
  #   if (is.null(take_file))
  #   return(NULL)
  #   if (substr_right(paste0(take_file$datapath),4) == "xlsx")
  #   {
  #     p <- read_xlsx(take_file$datapath)
  #   } else {
  #     p <- read.csv(take_file$datapath, sep = input$xlsxriabel)
  #   }
  #  
  #   return(p)
  #   
  #   }) 
  
  output$show_data <- DT::renderDT({
    import_data <- myData()
    DT::datatable(import_data)
    })
  
  output$txtOutput <- renderText({
    paste("Hai",input$txtInput,"!")})
  
  observe(addScrollAnim(session, 'shiny1', 'pulse')) 
  observe(addScrollAnim(session, 'shiny2', 'pulse')) 
  observe(addScrollAnim(session, 'shiny3', 'bounceInRight'))
  observe(addScrollAnim(session, 'shiny4', 'bounceInLeft')) 
  observe(addScrollAnim(session, 'shiny5', 'bounceInRight')) 
  observe(addScrollAnim(session, 'shiny6', 'bounceInRight'))
  observe(addScrollAnim(session, 'shiny7', 'bounceInRight'))
  observe(addScrollAnim(session, 'shiny8', 'bounceInRight')) 
  observe(addScrollAnim(session, 'shiny9', 'bounceInLeft')) 
  observe(addScrollAnim(session, 'shiny10', 'pulse'))
  observe(addScrollAnim(session, 'shiny11', 'bounceInLeft')) 
  observe(addScrollAnim(session, 'shiny12', 'bounceInRight')) 
  observe(addScrollAnim(session, 'shiny13', 'bounceInRight')) 
  observe(addScrollAnim(session, 'shiny14', 'bounceInRight')) 
  observe(addScrollAnim(session, 'shiny15', 'pulse')) 
  observe(addScrollAnim(session, 'shiny16', 'pulse'))
  observe(addScrollAnim(session, 'shiny17', 'pulse'))
  observe(addScrollAnim(session, 'shiny18', 'pulse'))
  observe(addScrollAnim(session, 'shiny19', 'pulse'))
  observe(addScrollAnim(session, 'shiny20', 'pulse'))
  observe(addScrollAnim(session, 'shiny21', 'pulse')) 
  observe(addScrollAnim(session, 'shiny22', 'pulse'))
  observe(addScrollAnim(session, 'shiny23', 'pulse'))
  observe(addScrollAnim(session, 'shiny24', 'pulse'))
  observe(addScrollAnim(session, 'shiny25', 'pulse'))
  observe(addScrollAnim(session, 'shiny26', 'pulse'))
  observe(addScrollAnim(session, 'shiny27', 'pulse')) 
  observe(addScrollAnim(session, 'shiny28', 'pulse'))
  observe(addScrollAnim(session, 'shiny29', 'pulse'))
  observe(addScrollAnim(session, 'shiny30', 'pulse'))
  observe(addScrollAnim(session, 'shiny31', 'pulse'))
  observe(addScrollAnim(session, 'shiny32', 'pulse'))
  observe(addScrollAnim(session, 'shiny33', 'pulse'))
  observe(addScrollAnim(session, 'shiny34', 'pulse'))
  
  
  # Import New Data for Testing
  myTest <- reactive({
    take_test <- input$take_test
    if (is.null(take_test))
      return(NULL)
    p <- read.csv(take_test$datapath, sep = input$pemisah_variabel2)
    return(p)
  })
  
  output$show_myTest <- DT::renderDT({
    import_data2 <- myTest()
    DT::datatable(import_data2)
  })
  
  
  # Naive Bayes Model
  library(e1071)
  
  
  # Menentukan Percentage Split
  splitSlider <- reactive({
    input$Slider1 / 100
  })
  
  
  # Deklarasi Input Data Model
  InputDataset <- myData
  # Deklarasi Variable Selections
  InputDataset_model <- myData
  
  output$SelectX <-  renderUI({
    box(selectizeInput("SelectX", 
                       label = "Predictor:",
                       choices = names(InputDataset_model()), 
                       selected = 1, 
                       multiple = TRUE),
        solidHeader = TRUE, width = "12", title = "Predictor (X)")
  })
  
  
  
  observe({
    lstname <- names(InputDataset())
    updateSelectInput(session = session,
                      inputId = "SelectX",
                      choices = lstname,
                      selected = if(input$all) names(InputDataset_model())
                      )
  })
  
  output$SelectY <-  renderUI({
    box(selectizeInput("SelectY",
                       label = "Pilih variabel yang akan diprediksi:",
                       choices = names(InputDataset_model()), 
                       selected = 1, 
                       multiple = FALSE),
        solidHeader = TRUE, width = "12", title = "Target (Y)")
  })
  
  observe({
    lstname <- names(InputDataset())
    updateSelectInput(session = session,
                      inputId = "SelectY",
                      choices = lstname)
  })
  
   
   # We choose high number for random sampling, assuming the total data is hundred thousands to provide good accuracy
   set.seed(1e+7)
   trainingRowIndex <- reactive({
         sample(1:nrow(InputDataset_model()),
                splitSlider() * nrow(InputDataset_model()))
      })
   trainingData <- reactive({
      acttraindt <- InputDataset_model()
      acttraindt[trainingRowIndex(),]
   })
   
   testData <- reactive({
      acttestdt <- InputDataset_model()
      acttestdt[-trainingRowIndex(),]
   })
   output$cntTrain <-
      renderText(paste("Train Data:", 
                       tryCatch(NROW(trainingData()), 
                                error=function(e) "harap import data!") 
                       ))
   output$cntTest <-
      renderText(paste("Test Data:", 
                        tryCatch(NROW(testData()), 
                                 error=function(e) "harap import data!") ))
   
   # output$Data <- renderDT(InputDataset())
   
   output$Data <- DT::renderDT({
     
     
     tampil <- myData()
     
     data_terpilih = tampil[c(input$SelectX)]
     
     print(data_terpilih)
     
   })
   
   
   # Screenshot
   observeEvent(input$cetak_gambar1,
                screenshot(
                  selector = "#summary_dataset",
                  filename = "summary_dataset",
                  id = "",
                  # scale = 1,
                  timer = 0,
                  download = TRUE,
                  
                )
             )
   observeEvent(input$cetak_gambar2,
                screenshot(
                  selector = "#profil_dataset",
                  filename = "profile_dataset",
                  id = "",
                  # scale = 1,
                  timer = 0,
                  download = TRUE,
                  
                )
            )
   observeEvent(input$cetak_gambar3,
                screenshot(
                  selector = "#Model",
                  filename = "model",
                  id = "",
                  # scale = 1,
                  timer = 0,
                  download = TRUE,
                  
                )
            )
   observeEvent(input$cetak_gambar4,
                screenshot(
                  selector = "#Model_summary",
                  filename = "summary",
                  id = "",
                  # scale = 1,
                  timer = 0,
                  download = TRUE,
                  
                )
            )
   observeEvent(input$cetak_gambar5,
                screenshot(
                  selector = "#crossT",
                  filename = "Confussion_Matrix",
                  id = "",
                  # scale = 1,
                  timer = 0,
                  download = TRUE,
                  
              )
              )
   observeEvent(input$cetak_gambar6,
                screenshot(
                  selector = "#Test",
                  filename = "overall",
                  id = "",
                  # scale = 1,
                  timer = 0,
                  download = TRUE,
                  
                )
            )
   observeEvent(input$cetak_gambar7,
                screenshot(
                  selector = "#Test2",
                  filename = "class",
                  id = "",
                  # scale = 1,
                  timer = 0,
                  download = TRUE,
                  
                )
            )
   observeEvent(input$cetak_gambar8,
                screenshot(
                  selector = "#Test3",
                  filename = "Table",
                  id = "",
                  # scale = 1,
                  timer = 0,
                  download = TRUE,
                  
                )
   )
   
   observeEvent(input$cetak_gambar9,
                screenshot(
                  selector = "#Test4",
                  filename = "Comparison",
                  id = "",
                  # scale = 1,
                  timer = 0,
                  download = TRUE,
                  
                )
   )
   
   observeEvent(input$cetak_gambar10,
                screenshot(
                  selector = "#summary",
                  filename = "Attribute Summary",
                  id = "",
                  # scale = 1,
                  timer = 0,
                  download = TRUE,
                  
                )
   )
   
   observeEvent(input$cetak_gambar11,
                screenshot(
                  selector = "#structure",
                  filename = "Structure Dataset",
                  id = "",
                  # scale = 1,
                  timer = 0,
                  download = TRUE,
                  
                )
   )
   
   
   
   # Variable Formulation
   f <- reactive({
     x_var <-paste0(input$SelectX)
     as.formula(paste(input$SelectY, "~", paste(x_var, collapse= "+")))
   })
   
   # Modelling
   NB_Model <- reactive({
      naiveBayes(f(), 
                 data = trainingData())
   })
   output$Model <- renderPrint(tryCatch(
     NB_Model(),
                                        error=function(e) "harap pilih predictor(X) terlebih dahulu!"))
   output$Model_summary <- renderPrint(tryCatch(summary(NB_Model()),
                                                error=function(e) "harap predictor(X) terlebih dahulu!"))
   
   # Assessment Features
   
   # Predict
   price_predict <- reactive({
     predict(NB_Model(), testData())
   })
   
   # Actual
   act <- reactive({
     act1 <- testData()
     act1[, c(input$SelectY)]
   })
   
   compare <- reactive({
     per <- cbind(as.factor(act()),
     as.factor(price_predict()))
     per

   })
   
   
   # Some statistics may not available, cause the library limitation
   cm <- 
     reactive({
       sample1 <- as.factor(act())
       sample2 <- as.factor(price_predict())
       
       statistics <- confusionMatrix(
         sample1,
         sample2
       )
       data.frame(statistics$overall)
       
     })
   
   cm2 <- 
     reactive({
       sample2 <- as.factor(act())
       sample3 <- as.factor(price_predict())
       
       statistics2 <- confusionMatrix(
         sample2,
         sample3
       )
       cm2d <- data.frame(statistics2$byClass)
       kable_styling(kable(cm2d))
      
       
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
   
   output$structure <- renderPrint({
     str(input$take_file)
   })
   
   #Predict MyTest
   MyTestData <- reactive({
     actmytest <- myTest()
   })
   
   output$predict_myTest <- renderText(
     tryCatch(paste0(1:nrow(MyTestData())," ", input$SelectY, " : ",predict(NB_Model(), MyTestData()), sep = "\n"),
              error=function(e) "Harap import data yang akan diprediksi!")
   )
   
   output$hist_dataset <- renderPlot(
     {
       hist_dataset <- myData()

       vis_miss(hist_dataset)
       vis_dat(hist_dataset)

       par(mfrow=c(3,4))
       for(i in 1:ncol(hist_dataset)) {
         hist(hist_dataset[, i], main = paste(colnames(hist_dataset[i])), xlab = "")
       }
     })
   
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
       paste0('reportAdami_', Sys.Date(), '.', switch(
         input$report123, 
         PDF = 'pdf',
         HTML = 'html', 
         Word = 'docx'
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
   
   
   

} 
)