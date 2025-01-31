

ModuleControlUI <- function(id) {
  
  ns <- NS(id)
  uiOutput(ns("ui_menuCONTROL"))
  
}



ModuleControlSERVER <-  function(input, output, session, base,
                                RMedic_general, status_BaseSalida,
                                zocalo_CIE) { 
  
  
  observe({
    
    req(RMedic_general(), status_BaseSalida())
    
    # NameSpaceasing for the session
    ns <- session$ns
    
    
    UserSelection <- callModule(module = BatallaNavalSERVER, 
                                id =  "control01",
                                base = base,
                                zocalo_CIE = zocalo_CIE,
                                verbatim = FALSE)
    
    req(UserSelection)
   observe({print(UserSelection$batalla_naval()$caso_tipo_variables)})
    
    # my_case <- reactiveVal(UserSelection$batalla_naval()$caso_tipo_variables)
    
   my_case <- reactive({
     
     UserSelection$batalla_naval()$caso_tipo_variables
     
   })
   
    MiniBase <- callModule(module = MiniBaseSERVER, id =  "control02",
                           base = base,
                           batalla_naval = UserSelection$batalla_naval,
                           verbatim = FALSE)
    
    
    # Determinamos los 5 casos para RMedic
    # 1) 1Q =  1 puntos
    # 2) 1C = 10 puntos
    # 3) 2Q =  2 puntos
    # 4) 2C = 20 puntos
    # 5) QC o CQ = 11 puntos
 
       
    observeEvent(my_case(), {
     if (my_case() == "1") {
        
          callModule(module = Control1Q_SERVER, id =  "control03",
                     base = base,
                     batalla_naval = UserSelection$batalla_naval,
                     decimales = UserSelection$decimales)
    
     }})
    
    
    observeEvent(my_case(), {
      if (my_case() == "2") {
    callModule(module = Control1C_SERVER, id =  "control04",
               base = base,
               batalla_naval = UserSelection$batalla_naval,
               decimales = UserSelection$decimales)
      }})
    
    
    observeEvent(my_case(), {
      if (my_case() == "3") {
        
    
    callModule(module = Control2Q_SERVER, id =  "control05",
               base = base,
               batalla_naval = UserSelection$batalla_naval,
               decimales = UserSelection$decimales)
      }})
    
    
    observeEvent(my_case(), {
      if (my_case() == "4") {
        
    
    callModule(module = Control2C_SERVER, id =  "control06",
               base = base,
               batalla_naval = UserSelection$batalla_naval,
               decimales = UserSelection$decimales)

      }})
    
    
        
    observeEvent(my_case(), {
      if (my_case() == "5") {
        
        
        callModule(module = ControlQC_SERVER, id =  "control07",
                   base = base,
                   batalla_naval = UserSelection$batalla_naval,
                   decimales = UserSelection$decimales)
        
      }})
    
    output$ui_menuCONTROL <- renderUI({
      
      # Si no hay orden de salir a la cancha... Nadie sale...
      if(is.null(RMedic_general())) return(NULL)
      if(!RMedic_general()) return(NULL)
      
      # Si no hay status de BaseSalida(), nos vamos...
      if(is.null(status_BaseSalida())) return(NULL)
      if(!status_BaseSalida()) return(NULL)
      
      
        fluidRow(
          column(1),
          column(10,
                 #h3(HTML("<u><b>Menú para Control</b></u>")),
                 h3_mod("Menú para Control"),
                 BatallaNavalUI(ns("control01")),
                 MiniBaseUI(ns("control02")),
                 tags$hr(style = "border-top: 3px solid #000000;"),
                 Control1Q_UI(ns("control03")),
                 Control1C_UI(ns("control04")),
                 Control2Q_UI(ns("control05")),
                 Control2C_UI(ns("control06")),
                 ControlQC_UI(ns("control07"))
                 # Tablas2Q_UI(ns("tablas05")),
                 # Tablas2C_UI(ns("tablas06")),
                 # TablasQC_UI(ns("tablas07"))
          ),
          column(1)
        )
        
    })
    
    
    # menuCONTROL <- reactive({
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
    #     title = "Control", 
    #     # icon = icon("user-md"), 
    #     value = 2,
    #     fluidRow(
    #       column(1),
    #       column(10,
    #              h3("Menú para Control"),
    #              BatallaNavalUI(ns("control01")),
    #              MiniBaseUI(ns("control02")),
    #              Control1Q_UI(ns("control03")),
    #              Control1C_UI(ns("control04")),
    #              Control2Q_UI(ns("control05")),
    #              Control2C_UI(ns("control06"))
    #              # Tablas2Q_UI(ns("tablas05")),
    #              # Tablas2C_UI(ns("tablas06")),
    #              # TablasQC_UI(ns("tablas07"))
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
    # return(menuCONTROL)
    
  })
  
}

