server <- function(input, output, session)
{
  bs_themer()
  
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
  
}
