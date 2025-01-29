
module_pack002_import_s01_xlsx_ui <- function(id){
  ns <- shiny::NS(id)
  
  
  
  div(uiOutput(ns("iu_base_selector")))
  
  
  
}

module_pack002_import_s01_xlsx_server <- function(id, sui_data_source){
  
  moduleServer(
    id,
    function(input, output, session) {
      
      # ns para el server!
      ns <- session$ns
      
      check_ok <- reactive({
        req(sui_data_source())
        sui_data_source() == "source_xlsx"
      })
      
      output$iu_base_selector <- renderUI({
        req(check_ok())
        div(
          fileInput(ns("file1"), "Elige un archivo xlsx (Solo primer hoja)",
                    accept = c(
                      ".xlsx")
          )
        )
        
        
        
        
      })
      
      
      
      str_import_general_sentence <- reactive({
        my_text <- "openxlsx::read.xlsx(xlsxFile = '_my_path_', sheet = 1)"
        
        my_text
      })
      
      full_import_info <- reactive({
        req(input$file1)
        #print(input$file1)
        input$file1
      })
      
      
      temporal_file_path <- reactive({
        req(input$file1$datapath)
        input$file1$datapath
      })
      
      
      
      original_file_name <- reactive({
        req(input$file1$name)
        input$file1$name
      })
      
      str_import_local <- reactive({
        req(str_import_general_sentence(), original_file_name())
        
        my_text <- str_import_general_sentence()
        my_text <- sub(pattern = "_my_path_", replacement = original_file_name(), x = my_text)
        my_text
        
      })
      
      database <- reactive({
        req(input$file1)
        req(check_ok())
        
        
        # Lee el archivo CSV
        #print(input$file1$datapath)
        my_text <- str_import_general_sentence()
        my_text <- sub(pattern = "'_my_path_'", "input$file1$datapath", x = my_text)
        aver <- eval(parse(text = my_text))
        
        # Muestra el contenido del archivo xlsx
        aver
      })
      
      
      
      
      import_pack <- reactive({
        req(str_import_general_sentence(), full_import_info(), temporal_file_path(),
            original_file_name(),  str_import_local(), database())
        
        
        armado <- list("str_import_general_sentence" = str_import_general_sentence(),
                       "full_import_info" = full_import_info(),
                       "temporal_file_path" = temporal_file_path(),
                       "original_file_name" = original_file_name(),
                       "str_import_local" = str_import_local(),
                       "database" = database()
                       
        )
        
        #$print(armado)
        
        armado
      })
      
      
      return(import_pack)
    }
  )
}


#######################################################################################

module_pack002_import_s02_csv_ui <- function(id){
  ns <- shiny::NS(id)
  
  
  
  div(uiOutput(ns("iu_base_selector")))
  
  
  
}

module_pack002_import_s02_csv_server <- function(id, sui_data_source){
  
  moduleServer(
    id,
    function(input, output, session) {
      
      # ns para el server!
      ns <- session$ns
      
      
      check_ok <- reactive({
        req(sui_data_source())
        sui_data_source() == "source_csv"
      })
      
      output$iu_base_selector <- renderUI({
        req(check_ok())
        div(
          fileInput(ns("file1"), "Elige un archivo CSV",
                    accept = c(
                      "text/csv",
                      "text/comma-separated-values,text/plain",
                      ".csv")
          ),
          tags$hr(),
          radioButtons(ns("header"), "Encabezado", choices = c(Yes = 1, No = 0)), br(),
          radioButtons(ns("sep"), "Separador",
                       choices = c(PuntoYComa = ";",
                                   Coma = ",",
                                   Tab = "\t"),
                       selected = ";"), br(),
          radioButtons(ns("dec"), "Decimal",
                       choices = c(Dot = ".",
                                   Comma = ","),
                       selected = "."), br(),
          
          radioButtons(ns("quote"), "Comillas",
                       choices = c(Ninguna = "",
                                   Doble = '"',
                                   Simple = "'"),
                       selected = ""), br()
          
        )
      })
      
      
      
      #return(database)
      
      str_import_general_sentence <- reactive({
        my_text <- "openxlsx::read.xlsx(xlsxFile = '_my_path_', sheet = 1)"
        
        my_text
      })
      
      full_import_info <- reactive({
        req(input$file1)
        #print(input$file1)
        input$file1
      })
      
      
      temporal_file_path <- reactive({
        req(input$file1$datapath)
        input$file1$datapath
      })
      
      
      
      original_file_name <- reactive({
        req(input$file1$name)
        input$file1$name
      })
      
      str_import_local <- reactive({
        req(str_import_general_sentence(), original_file_name())
        
        my_text <- str_import_general_sentence()
        my_text <- sub(pattern = "_my_path_", replacement = original_file_name(), x = my_text)
        my_text
        
      })
      
      database <- reactive({
        req(input$file1)
        req(check_ok())
        
        # Lee el archivo CSV
        database <- read.csv(file = input$file1$datapath,
                             header = as.logical(as.numeric(input$header)),
                             sep = input$sep,
                             dec = input$dec,
                             quote = input$quote,
                             na.strings = c("", NA),
                             stringsAsFactors = F)
        
        # Muestra el contenido del archivo CSV
        database
      })
      
      
      # database <- reactive({
      #   req(input$file1)
      #   req(check_ok())
      #   
      #   
      #   # Lee el archivo CSV
      #   print(input$file1$datapath)
      #   my_text <- str_import_general_sentence()
      #   my_text <- sub(pattern = "'_my_path_'", "input$file1$datapath", x = my_text)
      #   aver <- eval(parse(text = my_text))
      #   
      #   # Muestra el contenido del archivo xlsx
      #   aver
      # })
      
      
      
      
      import_pack <- reactive({
        req(str_import_general_sentence(), full_import_info(), temporal_file_path(),
            original_file_name(),  str_import_local(), database())
        
        
        armado <- list("str_import_general_sentence" = str_import_general_sentence(),
                       "full_import_info" = full_import_info(),
                       "temporal_file_path" = temporal_file_path(),
                       "original_file_name" = original_file_name(),
                       "str_import_local" = str_import_local(),
                       "database" = database()
                       
        )
        
        #$print(armado)
        
        armado
      })
      
      
      return(import_pack)
      
      
    }
  )
}

#######################################################################################


module_pack002_import_s03_revelio_ui <- function(id){
  ns <- shiny::NS(id)
  
  
  
  div(uiOutput(ns("iu_base_selector")))
  
  
  
}

module_pack002_import_s03_revelio_server <- function(id, sui_data_source){
  
  moduleServer(
    id,
    function(input, output, session) {
      
      # ns para el server!
      ns <- session$ns
      
      check_ok <- reactive({
        req(sui_data_source())
        sui_data_source() == "source_revelio"
      })
      
      all_names_database <- reactive({
        req(check_ok())
        
        # list.files(input_folder_master)
        #datasets_info <- data(package = "Revelio")
        #dataset_names <- datasets_info$results[, "Item"]
        dataset_names <- names(data_list_RMedic)
        dataset_names
        
      })
      
      select_opt_database <- reactive({
        req(check_ok())
        req(all_names_database())
        
        # # # Nombre de las bases de datos
        vector_obj <- all_names_database()
        
        # # # Numero de orden, desed 01 hasta la ultima
        vector_numbers <- 1:length(vector_obj)
        amount_digits <- max(nchar(vector_numbers))
        if(amount_digits < 2) amount_digits <- 2
        
        vector_formatted_numbers <- stringr::str_pad(string = vector_numbers,
                                                     width = amount_digits,
                                                     pad = "0")
        
        # Vector con la visualizacion que tiene el usuario
        vector_visual <- paste0(vector_formatted_numbers, " - ", vector_obj)
        
        # Asignamos la visual a al vector
        names(vector_obj) <- vector_visual
        
        
        # Salida!
        vector_obj
        
      })
      
      output$iu_base_selector <- renderUI({
        req(check_ok())
        
        vector_visual <- c("Seleccione una..." = "", select_opt_database())
        
        
        shiny::selectInput(
          inputId = ns("sui_file_revelio"),
          label = "RMedic Examples",
          choices = vector_visual
        )
      })
      
      
      #return(database)
      
      str_import_general_sentence <- reactive({
        my_text <- "openxlsx::read.xlsx(xlsxFile = '_my_path_', sheet = 1)"
        
        my_text
      })
      
      full_import_info <- reactive({
        selected_dataset <- input$sui_file_revelio
        selected_dataset
      })
      
      
      temporal_file_path <- reactive({
        req(input$sui_file_revelio)
        selected_dataset <- input$sui_file_revelio
        selected_dataset
        
      })
      
      
      
      original_file_name <- reactive({
        req(input$sui_file_revelio)
        selected_dataset <- input$sui_file_revelio
        selected_dataset
      })
      
      str_import_local <- reactive({
        req(str_import_general_sentence(), original_file_name())
        
        my_text <- str_import_general_sentence()
        my_text <- sub(pattern = "_my_path_", replacement = original_file_name(), x = my_text)
        my_text
        
      })
      
      database <- reactive({
        req(input$sui_file_revelio)
        
        selected_dataset <- input$sui_file_revelio
        #selected_dataset <- paste0("Revelio::", selected_dataset)
        
        #get(selected_dataset, "package:Revelio")
        #eval(parse(text=selected_dataset))
        
        # Objeto de GLOBAL
        data_list_RMedic[[selected_dataset]]
        # switch(input$sui_file_revelio,
        #        "mtcars" = mtcars,
        #        "iris" = iris,
        #        "airquality" = airquality,
        #        stop("Base de datos no encontrada"))
      })
      
      
      
      # database <- reactive({
      #   req(input$file1)
      #   req(check_ok())
      #   
      #   
      #   # Lee el archivo CSV
      #   print(input$file1$datapath)
      #   my_text <- str_import_general_sentence()
      #   my_text <- sub(pattern = "'_my_path_'", "input$file1$datapath", x = my_text)
      #   aver <- eval(parse(text = my_text))
      #   
      #   # Muestra el contenido del archivo xlsx
      #   aver
      # })
      
      
      
      
      import_pack <- reactive({
        req(str_import_general_sentence(), full_import_info(), temporal_file_path(),
            original_file_name(),  str_import_local(), database())
        
        
        armado <- list("str_import_general_sentence" = str_import_general_sentence(),
                       "full_import_info" = full_import_info(),
                       "temporal_file_path" = temporal_file_path(),
                       "original_file_name" = original_file_name(),
                       "str_import_local" = str_import_local(),
                       "database" = database()
                       
        )
        
        #$print(armado)
        
        armado
      })
      
      
      return(import_pack)
      
      
    }
  )
}



#######################################################################################


module_pack002_import_s04_rdata_ui <- function(id){
  ns <- shiny::NS(id)
  
  
  
  div(uiOutput(ns("iu_base_selector")))
  
  
  
}

module_pack002_import_s04_rdata_server <- function(id, sui_data_source){
  
  moduleServer(
    id,
    function(input, output, session) {
      
      # ns para el server!
      ns <- session$ns
      
      check_ok <- reactive({
        req(sui_data_source())
        sui_data_source() == "source_rdata"
      })
      
      
      output$iu_base_selector <- renderUI({
        req(check_ok(), sui_data_source())
        
        vector_opt <- c("01 - mtcars"     = "mtcars",
                        "02 - iris"       = "iris",
                        "03 - airquality" = "airquality")
        
        vector_opt <- c("Seleccione una..." = "", vector_opt)
        
        shiny::selectInput(
          inputId = ns("r_database"),
          label = "Bases de R",
          choices = vector_opt
        )
      })
      
      
      # database <- reactive({
      #   req(check_ok(), sui_data_source())
      #   req(input$r_database)
      #   switch(input$r_database,
      #          "mtcars" = mtcars,
      #          "iris" = iris,
      #          "airquality" = airquality)
      # })
      
      
      #      return(database)
      
      str_import_general_sentence <- reactive({
        my_text <- "openxlsx::read.xlsx(xlsxFile = '_my_path_', sheet = 1)"
        
        my_text
      })
      
      full_import_info <- reactive({
        selected_dataset <- input$r_database
        selected_dataset
      })
      
      
      temporal_file_path <- reactive({
        req(input$r_database)
        selected_dataset <- input$r_database
        selected_dataset
        
      })
      
      
      
      original_file_name <- reactive({
        req(input$r_database)
        selected_dataset <- input$r_database
        selected_dataset
      })
      
      str_import_local <- reactive({
        req(str_import_general_sentence(), original_file_name())
        
        my_text <- str_import_general_sentence()
        my_text <- sub(pattern = "_my_path_", replacement = original_file_name(), x = my_text)
        my_text
        
      })
      
      database <- reactive({
        req(check_ok(), sui_data_source())
        req(input$r_database)
        switch(input$r_database,
               "mtcars" = mtcars,
               "iris" = iris,
               "airquality" = airquality)
      })
      
      
      
      
      
      import_pack <- reactive({
        req(str_import_general_sentence(), full_import_info(), temporal_file_path(),
            original_file_name(),  str_import_local(), database())
        
        
        armado <- list("str_import_general_sentence" = str_import_general_sentence(),
                       "full_import_info" = full_import_info(),
                       "temporal_file_path" = temporal_file_path(),
                       "original_file_name" = original_file_name(),
                       "str_import_local" = str_import_local(),
                       "database" = database()
                       
        )
        
        #$print(armado)
        
        armado
      })
      
      
      return(import_pack)
      
      
    }
  )
}


#######################################################################################



# # # 01) UI - Selection for 'database'
module_pack002_import_s00_general_p01_ui <- function(id){
  ns <- shiny::NS(id)
  
  
  
  div(
    tags$head(
      #tags$link(rel = "stylesheet", type = "text/css", href = "inst/estilos.css"),
      tags$style(HTML("
      .shiny-output-error-AVISO {
        color: #0000ff;
        font-weight: bold;
      }
    ")),
      tags$style(HTML("
      .shiny-output-error-ERROR {
        color: #ff0000;
        font-weight: bold;
      }
    ")),
      tags$style(HTML("
        .content-wrapper, .right-side {
          overflow-y: hidden !important;
        }
      "))
    ),
    
    
    
    id = ns("input-panel"),
    #shiny::h1("Selección de base de datos"),
    shiny::fluidRow(
      shiny::column(12,
                    
                    uiOutput(ns("box01_database")),
                    shiny::br(),
                    shiny::br()
                    
      )
    )
  ) # End div
}




module_pack002_import_s00_general_p01_server <- function(id){
  
  moduleServer(
    id,
    function(input, output, session) {
      
      # ns para el server!
      ns <- session$ns
      
      
      output$box01_database <- renderUI({
        
        
        shiny::selectInput(
          inputId = ns("sui_data_source"),
          label = "Qué fuente de datos prefieres?",
          choices = c("01 - xlsx files" = "source_xlsx",
                      "02 - csv files"  = "source_csv",
                      "03 - RMedic examples"  = "source_revelio",
                      "04 - R examples" = "source_rdata"),
          selected = "source_rdata"
          
        )
        
        
        
        
        
      })
      
      sui_data_source <- reactive({
        req(input$sui_data_source)
        input$sui_data_source
        
      })
      
      return(sui_data_source)
    }
  )
}



#######################################################################################

module_pack002_import_s00_general_p02_ui <- function(id){
  ns <- shiny::NS(id)
  
  
  
  div(uiOutput(ns("salida_general")))
}




module_pack002_import_s00_general_p02_server <- function(id, sui_data_source){
  
  moduleServer(
    id,
    function(input, output, session) {
      
      # ns para el server!
      ns <- session$ns
      
      
      # Pack 002) Import database
      
      # Reactive values to store the database from each module
      rv <- reactiveValues(
        xlsx = NULL,
        csv = NULL,
        revelio = NULL,
        r = NULL
      )
      
      observeEvent(sui_data_source(), {
        
        rv$xlsx    <- NULL
        rv$csv     <- NULL
        rv$revelio <- NULL
        rv$r       <- NULL
        
        # Call each module server and store the reactive database in rv
        rv$xlsx    <- module_pack002_import_s01_xlsx_server(id = "space02_database_01", sui_data_source)
        rv$csv     <- module_pack002_import_s02_csv_server(id = "space02_database_02", sui_data_source)
        rv$revelio <- module_pack002_import_s03_revelio_server(id = "space02_database_03", sui_data_source)
        rv$r       <- module_pack002_import_s04_rdata_server(id = "space02_database_04", sui_data_source)
        
      })
      
      
      output$salida_general <- renderUI({
        req(sui_data_source())
        
        switch(sui_data_source(),
               "source_xlsx" = module_pack002_import_s01_xlsx_ui(ns("space02_database_01")),
               "source_csv" = module_pack002_import_s02_csv_ui(ns("space02_database_02")),
               "source_revelio" = module_pack002_import_s03_revelio_ui(ns("space02_database_03")),
               "source_rdata" = module_pack002_import_s04_rdata_ui(ns("space02_database_04")))
        
      })
      
      
      # # -----------------------------------------------------------------------------------------------
      
      
      temporal_file_path <- reactive({
        req(sui_data_source())
        switch(sui_data_source(),
               "source_xlsx" = rv$xlsx()$temporal_file_path,
               "source_csv" = rv$csv()$temporal_file_path,
               "source_revelio" = rv$revelio()$temporal_file_path,
               "source_rdata" = rv$r()$temporal_file_path)
      })
      
      original_file_name <- reactive({
        req(sui_data_source())
        switch(sui_data_source(),
               "source_xlsx" = rv$xlsx()$original_file_name,
               "source_csv" = rv$csv()$original_file_name,
               "source_revelio" = rv$revelio()$original_file_name,
               "source_rdata" = rv$r()$original_file_name)
      })
      
      
      str_import_local <- reactive({
        req(sui_data_source())
        switch(sui_data_source(),
               "source_xlsx" = rv$xlsx()$str_import_local,
               "source_csv" = rv$csv()$str_import_local,
               "source_revelio" = rv$revelio()$str_import_local,
               "source_rdata" = rv$r()$str_import_local)
      })
      
      
      database <- reactive({
        req(sui_data_source())
        switch(sui_data_source(),
               "source_xlsx" = rv$xlsx()$database,
               "source_csv" = rv$csv()$database,
               "source_revelio" = rv$revelio()$database,
               "source_rdata" = rv$r()$database)
      })
      
      
      # # -----------------------------------------------------------------------------------------------
      
      #     
      #     output$df_database <- renderDT({
      #       req(database())
      #       mi_tabla <- database()
      #       new_col_names <- colnames(mi_tabla)
      #       
      #   #     DT::datatable(
      #   #       mi_tabla, colnames = new_col_names,
      #   #       filter = 'top',
      #   #       extensions = 'Buttons',
      #   #       options = list(
      #   #         autowidth = TRUE,
      #   #         order = list(list(0, 'asc')),
      #   #         # columnDefs = list(list(className = 'dt-left', targets = c(0,1,2))),
      #   #         pageLength = 10,
      #   #         lengthMenu = c(10, 50, 75, 100, 150),
      #   #         dom = 'Bfrtip',  # Elementos de la tabla (botones, filtro, etc.)
      #   #         buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),  # Botones para exportar la tabla
      #   #         language = list(
      #   #     search = "Búsqueda:",
      #   #     lengthMenu = "Mostrar _MENU_ registros",
      #   #     info = "Mostrando registros del _START_ al _END_ de un total de _TOTAL_ registros",
      #   #     infoFiltered = "(filtrados de un total de _MAX_ registros)",
      #   #     paginate = list(previous = "Anterior", `next` = "Siguiente")
      #   #   ),
      #   #         initComplete = JS("
      #   #   function(settings, json) {
      #   #     $('body').css({
      #   #       'font-family': 'Century Gothic', 'font-size': '150%'
      #   #     });
      #   #     $(this.api().table().header()).css({
      #   #       'font-family': 'Century Gothic',
      #   #       'font-size':'125%',
      #   #       'background-color': '#008000',
      #   #       'color': '#fff'
      #   #     });
      #   #   }
      #   # "),
      #   #         rowCallback = JS("
      #   #   function(row, data, index) {
      #   #     if(index % 2 === 0) {
      #   #       $(row).css('background-color', 'lightblue');
      #   #     } else {
      #   #       $(row).css('background-color', 'lightgreen');
      #   #     }
      #   #   }
      #   # ")
      #   #       ),
      #   #       rownames = TRUE
      #   #     )
      # 
      # # datatable(mi_tabla, rownames = F,  options = list(
      # #         initComplete = JS(
      # #           "function(settings, json) {",
      # #           "$(this.api().table().header()).css({'background-color': '#fff', 'color': '#000'});",
      # #           "}"), language = list(
      # #             search = "Búsqueda:",
      # #             lengthMenu = "Mostrar _MENU_ registros",
      # #             info = "Mostrando registros del _START_ al _END_ de un total de _TOTAL_ registros",
      # #             infoFiltered = "(filtrados de un total de _MAX_ registros)",
      # #             paginate = list(previous =    "Anterior", `next` = "Siguiente")
      # #           )
      # #       ))
      #     })
      #     
      # datatable(BaseSalida(), rownames = F,  options = list(
      #         initComplete = JS(
      #           "function(settings, json) {",
      #           "$(this.api().table().header()).css({'background-color': '#fff', 'color': '#000'});",
      #           "}"), language = list(
      #             search = "Búsqueda:",
      #             lengthMenu = "Mostrar _MENU_ registros",
      #             info = "Mostrando registros del _START_ al _END_ de un total de _TOTAL_ registros",
      #             infoFiltered = "(filtrados de un total de _MAX_ registros)",
      #             paginate = list(previous =    "Anterior", `next` = "Siguiente")
      #           )
      #       ))
      
      output_list <- reactive({
        armado <- list(
          "temporal_file_path" = temporal_file_path(),
          "original_file_name" = original_file_name(),
          "str_import_local" = str_import_local(),
          "database" = database())
        
        #print(armado)
        armado
        
      })
      
      return(output_list)
      
    }
  )
}


#######################################################################################
