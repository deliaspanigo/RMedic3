

ModuleTablasUI <- function(id) {
  
  ns <- NS(id)
  uiOutput(ns("ui_menuTABLAS"))
  
  
 
  
}



ModuleTablasSERVER <-  function(input, output, session, base,
                                RMedic_general, status_BaseSalida,
                                zocalo_CIE) { 
 
  observe({
    
  req(RMedic_general(), status_BaseSalida())
  
  # NameSpaceasing for the session
  ns <- session$ns
  
  
  UserSelection <- callModule(module = BatallaNavalSERVER, 
                              id =  "tablas01",
                              base = base,
                              zocalo_CIE = zocalo_CIE,
                              verbatim = FALSE)
  
  
  MiniBase <- callModule(module = MiniBaseSERVER, id =  "tablas02",
                         base = base,
                         batalla_naval = UserSelection$batalla_naval,
                         verbatim = FALSE)
  
  
  
  callModule(module = Tablas1Q_SERVER, id =  "tablas03",
             minibase = MiniBase,
             batalla_naval = UserSelection$batalla_naval,
             decimales = UserSelection$decimales)
  
  
  callModule(module = Tablas1C_SERVER, id =  "tablas04",
             minibase = MiniBase,
             batalla_naval = UserSelection$batalla_naval,
             decimales = UserSelection$decimales)
  
  
  callModule(module = Tablas2Q_SERVER, id =  "tablas05",
             minibase = MiniBase,
             batalla_naval = UserSelection$batalla_naval,
             decimales = UserSelection$decimales)
  
  
  callModule(module = Tablas2C_SERVER, id =  "tablas06",
             minibase = MiniBase,
             batalla_naval = UserSelection$batalla_naval,
             decimales = UserSelection$decimales)
  
  
  callModule(module = TablasQC_SERVER, id =  "tablas07",
             minibase = MiniBase,
             batalla_naval = UserSelection$batalla_naval,
             decimales = UserSelection$decimales)
  
  
  
  output$ui_menuTABLAS <- renderUI({
    
    # Si no hay orden de salir a la cancha... Nadie sale...
    if(is.null(RMedic_general())) return(NULL)
    if(!RMedic_general()) return(NULL)
    
    # Si no hay status de BaseSalida(), nos vamos...
    if(is.null(status_BaseSalida())) return(NULL)
    if(!status_BaseSalida()) return(NULL)
    
    
      fluidRow(
        column(1),
        column(10,
               h3("Menú para Tablas"),
               BatallaNavalUI(ns("tablas01")),
               MiniBaseUI(ns("tablas02")),
               Tablas1Q_UI(ns("tablas03")),
               Tablas1C_UI(ns("tablas04")),
               Tablas2Q_UI(ns("tablas05")),
               Tablas2C_UI(ns("tablas06")),
               TablasQC_UI(ns("tablas07"))
        ),
        column(1)
      )
      

    
    
   # tabs
    
  })
  
  
  # menuTABLAS <- reactive({
  #   
  #   # Si no hay orden de salir a la cancha... Nadie sale...
  #   if(is.null(RMedic_general())) return(NULL)
  #   if(!RMedic_general()) return(NULL)
  #   
  #   # Si no hay status de BaseSalida(), nos vamos...
  #   if(is.null(status_BaseSalida())) return(NULL)
  #   if(!status_BaseSalida()) return(NULL)
  #   
  #   
  #   tabs <- list()
  #   
  #   
  #   tabs[[1]] <-  tabPanel(
  #     title = "Tablas", 
  #     # con = icon("user-md"), 
  #     value = 3,
  #     fluidRow(
  #       column(1),
  #       column(10,
  #              h3("Menú para Tablas"),
  #              BatallaNavalUI(ns("tablas01")),
  #              MiniBaseUI(ns("tablas02")),
  #              Tablas1Q_UI(ns("tablas03")),
  #              Tablas1C_UI(ns("tablas04")),
  #              Tablas2Q_UI(ns("tablas05")),
  #              Tablas2C_UI(ns("tablas06")),
  #              TablasQC_UI(ns("tablas07"))
  #       ),
  #       column(1)
  #     )
  # 
  #   ) # End TabPanel
  #   
  #   
  #   
  #   tabs
  #   
  # })
  # 
  # 
  # #Return del Modulo
  # return(menuTABLAS)
  # 
  
  })
  
  }