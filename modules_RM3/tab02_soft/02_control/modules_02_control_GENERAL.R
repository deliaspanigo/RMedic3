

module_02_control_GENERAL_UI <- function(id) {
  
  ns <- NS(id)
  uiOutput(ns("ui_menuCONTROL"))
  
}



module_02_control_GENERAL_SERVER <-  function(input, output, session, base,
                                 RMedic_general, status_BaseSalida,
                                 zocalo_CIE) { 
  
  
  observe({
    
    req(RMedic_general(), status_BaseSalida())
    
    # NameSpaceasing for the session
    ns <- session$ns
    
    
    UserSelection <- callModule(module = module_02_control_VarSelector_SERVER,
                                id =  "var_selection_control",
                                base = base,
                                zocalo_CIE = zocalo_CIE,
                                verbatim = FALSE)
    
    req(UserSelection)
   
    
    # Determinamos los 5 casos para RMedic
    # Caso 1) 1Q =  1 puntos
    # Caso 2) 1C = 10 puntos
    # Caso 3) 2Q =  2 puntos
    # Caso 4) 2C = 20 puntos
    # Caso 5) QC o CQ = 11 puntos

        
    my_case <- reactive({
      
      UserSelection$batalla_naval()$caso_tipo_variables
      
    })

    
    # Modules Soft - Tabs mapping
    control_server_modules <- list(
      "1" = "modules_02_control_Control1Q_SERVER",
      "2" = "modules_02_control_Control1C_SERVER",
      "3" = "modules_02_control_Control2Q_SERVER",
      "4" = "modules_02_control_Control2C_SERVER",
      "5" = "modules_02_control_ControlQC_SERVER"
    )

        
    observeEvent(my_case(), {
      req(base, UserSelection)

      selected_module_name <- control_server_modules[[my_case()]]
      req(selected_module_name)
      
      callModule(module = get(selected_module_name),
                 id = paste0("control", my_case()),
                 base = base,
                 batalla_naval = UserSelection$batalla_naval,
                 decimales = UserSelection$decimales)
      
    })
    
    

    
    

    output$ui_menuCONTROL <- renderUI({
      
      req(RMedic_general(), status_BaseSalida())

      
      fluidRow(
        column(1),
        column(10,
               h3_mod("Menú para Control"),
               module_02_control_VarSelector_UI(ns("var_selection_control")),
               tags$hr(style = "border-top: 3px solid #000000;"),
               modules_02_control_Control1Q_UI(ns("control1")),
               modules_02_control_Control1C_UI(ns("control2")),
               modules_02_control_Control2Q_UI(ns("control3")),
               modules_02_control_Control2C_UI(ns("control4")),
               modules_02_control_ControlQC_UI(ns("control5"))
        ),
        column(1)
      )
      
    })
    
    

  })
  
}


########################################################################################

