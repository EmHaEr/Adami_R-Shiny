server <- function(input, output, session)
{
  bs_themer()
  
  output$shiny <- renderUI(
    {
      source("PCA.R")
      
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
