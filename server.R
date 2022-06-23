server <- function(input, output, session)
{
    data <- reactive(
    {
      upload_data <- input$upload_data
      
      if(is.null(upload_data))
        return(NULL)
      
      data = read.csv(upload_data$datapath, sep = input$pemisah_variabel,
                      row.names = 1)
      return(data)
    })
  
  output$data2 <- DT::renderDT(
    {
       import_data <- data() 
       
       DT::datatable(import_data)
    })
  
  InputDataset<- data
 
  output$variabel <-  renderUI(
    {
      box(selectizeInput("variabel",
                        label = "Pilih variabel (tekan backspace untuk menghapus) :",
                        choices = names(InputDataset()),
                        selected = if(input$all) names(InputDataset()),
                        multiple = TRUE),
         solidHeader = TRUE, width = "12", title = "Variabel")
    })
  
  
  dataBaru <- reactive(
   {
     nama <- InputDataset()
     nama[input$variabel]
   })

  
  #############################
  ########## VARIABLE ########
  #############################
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
        data_selected = data2[c(input$variabel)]
        print(data_selected)
     })
  
  output$download_data <- downloadHandler(
     filename = function()
     {
        paste("Data PCA", "csv", sep = ".")
        
     },
     
     content = function(file)
     {
        write.csv(dataBaru(),file)
     })
  
  #############################
  ########### PCA #############
  #############################
  
  output$tampilan_dataset <- DT::renderDT(
     {
        tampil <- data()
        
        data_terpilih = tampil[c(variabel)]
        
        print(data_terpilih)
     })
  
  output$profil_dataset <- renderPrint(
    {
      profil_dataset <- dataBaru()
      
      glimpse(profil_dataset)
    })
  
  output$summary_dataset <- renderPrint(
    {
      summary_dataset <- dataBaru()
      
      summary(summary_dataset)
      skim(summary_dataset)
    })
  
  output$hist_dataset <- renderPlot(
    {
      hist_dataset <- dataBaru()
      
      vis_miss(hist_dataset)
      vis_dat(hist_dataset)
      
      par(mfrow=c(2,4))
      for(i in 1:ncol(hist_dataset)) {
        hist(hist_dataset[, i], main = paste(colnames(hist_dataset[i])), xlab = "")
      }
    })
  
  output$hasil_summaryPCA <- renderPrint(
     {
        hasil_summaryPCA <- dataBaru()
        
        pcaModel <- prcomp(hasil_summaryPCA, scale. = TRUE, center = TRUE)
        pcaModel$rotation
        
        summary(pcaModel)
     })
  
  output$hasil_modelPCA <- DT::renderDT(
    {
      hasil_modelPCA <- dataBaru()
      
      pcaModel <- prcomp(hasil_modelPCA, scale. = TRUE, center = TRUE)
      pcaModel$rotation
    })
  
  dataModel <- reactive(
     {
        hasil_modelPCA <- dataBaru()
        
        pcaModel <- prcomp(hasil_modelPCA, scale. = TRUE, center = TRUE)
        pcaModel$rotation
     })
  
  output$download_model <- downloadHandler(
     filename = function()
     {
        paste("Data_Model PCA", "csv", sep = ".")
        
     },
     
     content = function(file)
     {
              write.csv(dataModel(),file)
     })
  
  
  output$hasil_predictPCA <- DT::renderDT(
    {
      hasil_predictPCA <- dataBaru()
      
      pcaModel <- prcomp(hasil_predictPCA, scale. = TRUE, center = TRUE)
      pcaModel$rotation
      
      predict_PCA=predict(pcaModel, newdata=hasil_predictPCA)
    })
  
  dataPredict <- reactive(
     {
        hasil_predictPCA <- dataBaru()
        
        pcaModel <- prcomp(hasil_predictPCA, scale. = TRUE, center = TRUE)
        pcaModel$rotation
        
        predict_PCA=predict(pcaModel, newdata=hasil_predictPCA)
     })
  
  output$download_predict <- downloadHandler(
     filename = function()
     {
        paste("Data_Predict PCA", "csv", sep = ".")
        
     },
     
     content = function(file)
     {
        write.csv(dataPredict(),file)
     })
  
  output$eigenvalue_PCA <- renderPrint(
    {
      eigenvalue_PCA <- dataBaru()
      
      modelPCA <- prcomp(eigenvalue_PCA, scale. = TRUE, center = TRUE)
      modelPCA$rotation
      
      get_eigenvalue(modelPCA)
    })
  
  output$coord_PCA <- renderPrint(
    {
      coord_PCA <- dataBaru()
      
      modelPCA <- prcomp(coord_PCA, scale. = TRUE, center = TRUE)
      modelPCA$rotation
      
      var <- get_pca_var(modelPCA)
      var
      
      head(var$coord)
    })
  
  output$cos2_PCA <- renderPrint(
    {
      cos2_PCA <- dataBaru()
      
      modelPCA <- prcomp(cos2_PCA, scale. = TRUE, center = TRUE)
      modelPCA$rotation
      
      var <- get_pca_var(modelPCA)
      var
      
      head(var$cos2)
    })
  
  output$contrib_PCA <- renderPrint(
    {
      contrib_PCA <- dataBaru()
      
      modelPCA <- prcomp(contrib_PCA, scale. = TRUE, center = TRUE)
      modelPCA$rotation
      
      var <- get_pca_var(modelPCA)
      var
      
      head(var$contrib)
    })
  
  output$screeplot_PCA <- renderPlot(
    {
      screeplot_PCA <- dataBaru()
      
      modelPCA <- prcomp(screeplot_PCA, scale. = TRUE, center = TRUE)
      modelPCA$rotation
      
      fviz_eig(modelPCA)
    })
  
  output$cos2plot_PCA <- renderPlot(
    {
      cos2plot_PCA <- dataBaru()
      
      modelPCA <- prcomp(cos2plot_PCA, scale. = TRUE, center = TRUE)
      modelPCA$rotation
      
      fviz_cos2(modelPCA, choice = "var", axes = 1:2)
    })
  
  output$individuals_PCA <- renderPlot(
    {
      biplot_PCA <- dataBaru()
      
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
      contribplot_PCA <- dataBaru()
      
      modelPCA <- prcomp(contribplot_PCA, scale. = TRUE, center = TRUE)
      modelPCA$rotation
      
      fviz_contrib(modelPCA, choice = "var", axes = 1:2, top = 10)
    })
  
  output$variables_PCA <- renderPlot(
    {
      variables_PCA <- dataBaru()
      
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
      biplot_PCA <- dataBaru()
      
      modelPCA <- prcomp(biplot_PCA, scale. = TRUE, center = TRUE)
      modelPCA$rotation
      
      fviz_eig(modelPCA)
      
      fviz_pca_biplot(modelPCA, repel = TRUE,
                      col.var = "blue",
                      col.ind = "#000000")
    })
  
  #############################
  ######## SCREENSHOT  ########
  #############################
  
  observeEvent(input$cetak_gambar1,
               screenshot(
                  selector = "#profil_dataset",
                  filename = "Profil Dataset",
                  id = "",
                  # scale = 1,
                  timer = 0,
                  download = TRUE,
                  
               )
  )
  
  observeEvent(input$cetak_gambar2,
               screenshot(
                  selector = "#summary_dataset",
                  filename = "Summary Dataset",
                  id = "",
                  # scale = 1,
                  timer = 0,
                  download = TRUE,
                  
               )
  )
  
  observeEvent(input$cetak_gambar3,
               screenshot(
                  selector = "#hist_dataset",
                  filename = "Histogram Dataset",
                  id = "",
                  # scale = 1,
                  timer = 0,
                  download = TRUE,
                  
               )
  )
  
  observeEvent(input$cetak_gambar4,
               screenshot(
                  selector = "#eigenvalue_PCA",
                  filename = "Eigenvalue",
                  id = "",
                  # scale = 1,
                  timer = 0,
                  download = TRUE,
                  
               )
  )
  
  observeEvent(input$cetak_gambar5,
               screenshot(
                  selector = "#coord_PCA",
                  filename = "Coordinates",
                  id = "",
                  # scale = 1,
                  timer = 0,
                  download = TRUE,
                  
               )
  )
  
  observeEvent(input$cetak_gambar6,
               screenshot(
                  selector = "#cos2_PCA",
                  filename = "Squared Cosinus",
                  id = "",
                  # scale = 1,
                  timer = 0,
                  download = TRUE,
                  
               )
  )
  
  observeEvent(input$cetak_gambar7,
               screenshot(
                  selector = "#contrib_PCA",
                  filename = "Contribution",
                  id = "",
                  # scale = 1,
                  timer = 0,
                  download = TRUE,
                  
               )
  )
  
  observeEvent(input$cetak_gambar8,
               screenshot(
                  selector = "#Visualisasi",
                  filename = "Visualisasi PCA",
                  id = "",
                  # scale = 1,
                  timer = 0,
                  download = TRUE,
                  
               )
  )
 
  
} #akhir function