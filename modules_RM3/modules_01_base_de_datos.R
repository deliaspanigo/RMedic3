
module_pack002_import_s00_general_p03_server <- function(id, output_list_database){
  
  moduleServer(
    id,
    function(input, output, session) {
      
      # ns para el server!
      ns <- session$ns
      
      
      output$TextBase_InfoDataSet_01 <- renderText({
        
        req(output_list_database())
        
        
        texto_salida <- c("<u><b>Archivo:</b></u> _mi_archivo_ <br/>
                           <u><b>Variables (Columnas):</b></u> _ncolBase01_ variables.<br/>
                           <u><b>Unidades (Filas o repeticiones):</b></u> _nrowBase01_ unidades.<br/>")
        
        
        
        
        texto_salida <- gsub("_mi_archivo_", output_list_database()$"original_file_name",texto_salida)
        texto_salida <- gsub("_ncolBase01_", ncol(output_list_database()$"database"),texto_salida)
        texto_salida <- gsub("_nrowBase01_", nrow(output_list_database()$"database"),texto_salida)
        
        texto_salida <- paste0("<div style='font-size: 20px;'>", texto_salida, "</div>")
        
        
        mi_salida <- HTML(texto_salida)
        
        return(mi_salida)
        
        
      })
      
      
      output$title01 <- renderText({
        
        req(output_list_database())
        
        
        texto_salida <- c("Visualización de la Base de Datos")
        
        
        
        
        return(texto_salida)
        
        
      })
      
      
      
      output$df_database <- renderDT({
        
        req(output_list_database())
        mi_tabla <- output_list_database()$"database"
        new_col_names <- colnames(mi_tabla)
        
        DT::datatable(
          mi_tabla, colnames = new_col_names,
          filter = 'top',
          options = list(
            autowidth = TRUE,
            order = list(list(0, 'asc')),
            pageLength = 10,
            lengthMenu = c(10, 50, 75, 100, 150),
            dom = 'frtip',  # Elementos de la tabla (sin botones)
            language = list(
              search = "Búsqueda:",
              lengthMenu = "Mostrar _MENU_ registros",
              info = "Mostrando registros del _START_ al _END_ de un total de _TOTAL_ registros",
              infoFiltered = "(filtrados de un total de _MAX_ registros)",
              paginate = list(previous = "Anterior", `next` = "Siguiente")
            ),
            initComplete = JS("
      function(settings, json) {
        $('body').css({
          'font-family': 'Century Gothic', 'font-size': '150%'
        });
        $(this.api().table().header()).css({
          'font-family': 'Century Gothic',
          'font-size':'125%',
          'background-color': '#008000',
          'color': '#fff'
        });
      }
    "),
            rowCallback = JS("
      function(row, data, index) {
        if(index % 2 === 0) {
          $(row).css('background-color', 'lightblue');
        } else {
          $(row).css('background-color', 'lightgreen');
        }
      }
    ")
          ),
          rownames = TRUE
        )
        
        
        
      })
      
      
      
      
    }
  )
}


module_pack002_import_s00_general_p03_ui <- function(id){
  ns <- shiny::NS(id)
  
  div(htmlOutput(ns("TextBase_InfoDataSet_01")),
      br(),
      
      h2(tags$u(tags$b(textOutput(ns("title01"))))),
      DTOutput(ns("df_database")))
  
  # 
  # div(htmlOutput(ns("TextBase_InfoDataSet_01"),
  #                DTOutput(ns("df_database"))
  # ))
}

#######################################################################################