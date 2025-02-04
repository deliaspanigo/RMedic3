

module_04_graficos_GENERAL_UI <- function(id) {
  
  ns <- NS(id)
  
  uiOutput(ns("ui_menuGRAFICOS"))
  
  
  
  
}



module_04_graficos_GENERAL_SERVER <-  function(input, output, session, base,
                                  RMedic_general, status_BaseSalida,
                                  zocalo_CIE) { 
  
  observe({
    
    req(RMedic_general(), status_BaseSalida())
    
    # NameSpaceasing for the session
    ns <- session$ns
    
    
    UserSelection <- callModule(module = BatallaNavalSERVER, 
                                id =  "var_selection_graficos",
                                base = base,
                                zocalo_CIE = zocalo_CIE,
                                verbatim = FALSE)
    
    batalla_naval <- UserSelection$batalla_naval
    
    decimales <- UserSelection$decimales
    
    selected_vars <- reactive({batalla_naval()[[1]]})
    
    # observe(cat("casoRMedic()1: ", casoRMedic(), "\n"))
    
    
    
    MiniBase <- callModule(module = MiniBaseSERVER, id =  "minibase_tablas",
                           base = base,
                           batalla_naval = UserSelection$batalla_naval,
                           verbatim = FALSE)
    
    
    
    # my_case <- reactive({
    #   
    #   UserSelection$batalla_naval()$caso_tipo_variables
    #   
    # })
    casoRMedic <- reactive({
      
      if(is.null(batalla_naval())) return(NULL)
      if(is.null(batalla_naval()[[4]])) return(NULL)
      if(length(batalla_naval()[[4]]) == 0) return(NULL)
      if(batalla_naval()[[4]] == '') return(NULL)
      casoRMedic <- batalla_naval()[[4]]
      #casoRMedic <- as.numeric(as.character(as.vector(batalla_naval()[[4]])))
      casoRMedic
      
    })
    
    # Modules Soft - Tabs mapping
    graficos_server_modules <- list(
      "1" = "Graficos1Q_SERVER",
      "2" = "Graficos1C_SERVER",
      "3" = "Graficos2Q_SERVER",
      "4" = "Graficos1C_SERVER",
      "5" = "GraficosQC_SERVER"
    )
    
#     observeEvent(list(casoRMedic(), batalla_naval()),{
#       req(casoRMedic())
#       
#         selected_module_name <- graficos_server_modules[[casoRMedic()]]
#         req(selected_module_name)
#       
#       
# #      if(casoRMedic() == 1){
#         callModule(module = Graficos1Q_SERVER, 
#                    id =  paste0("graficos", casoRMedic()),
#                    minibase = MiniBase,
#                    casoRMedic = casoRMedic,
#                    caso = 1,
#                    decimales = decimales,
#                    batalla_naval = batalla_naval)
# #      }
#     })
    
    # observeEvent(list(MiniBase, casoRMedic(), batalla_naval()), {
    #   req(base(), UserSelection, casoRMedic(), batalla_naval())
    # 
    # 
    #   callModule(module = get(selected_module_name),
    #              id = paste0("graficos", casoRMedic()),
    #              minibase = MiniBase,
    #              casoRMedic = casoRMedic,
    #              caso = casoRMedic(), #1,
    #              decimales = decimales,
    #              batalla_naval = batalla_naval)
    # 
    # })
    
    
    # Caso 1: 1Q    
    observeEvent(list(casoRMedic(), batalla_naval()),{
      req(casoRMedic())
      if(casoRMedic() == 1){
        callModule(module = Graficos1Q_SERVER, id =  "graficos1",
                   minibase = MiniBase,
                   casoRMedic = casoRMedic,
                   caso = 1,
                   decimales = decimales,
                   batalla_naval = batalla_naval)
      }
    })
    

    # Caso 2 : 1C
    observeEvent(list(casoRMedic(), batalla_naval()),{
      req(casoRMedic())
      if(casoRMedic() == 2){
        
        callModule(module = Graficos1C_SERVER, id =  "graficos2",
                   minibase = MiniBase,
                   casoRMedic = casoRMedic,
                   caso = 2,
                   decimales = decimales,
                   batalla_naval = batalla_naval)
      }
    })
    

    # Caso 3: 2Q
    observeEvent(list(casoRMedic(), batalla_naval()),{
      req(casoRMedic())
      if(casoRMedic() == 3){
        
        callModule(module = Graficos2Q_SERVER, id =  "graficos3",
                   minibase = MiniBase,
                   casoRMedic = casoRMedic,
                   caso = 3,
                   decimales = decimales,
                   batalla_naval = batalla_naval)

      }
    })
    
    
    # Caso 4: 2C
    observeEvent(list(casoRMedic(), batalla_naval()),{
      req(casoRMedic())
      if(casoRMedic() == 4){
        


        callModule(module = Graficos2C_SERVER, id =  "graficos4",
                   minibase = MiniBase,
                   casoRMedic = casoRMedic,
                   caso = 4,
                   decimales = decimales,
                   batalla_naval = batalla_naval)

      }
    })
    
    
    # Caso 5: QC
    observeEvent(list(casoRMedic(), batalla_naval()),{
      req(casoRMedic())
      if(casoRMedic() == 5){
        
        
      

          callModule(module = GraficosQC_SERVER, id =  "graficos5",
                     minibase = MiniBase,
                     casoRMedic = casoRMedic,
                     caso = 5,
                     decimales = decimales,
                     batalla_naval = batalla_naval)
          
      }
    })
    
    
    ###################################################################### 
    
    
    my_case <- reactive({
      
      UserSelection$batalla_naval()$caso_tipo_variables
      
    })
    
    
    # Modules Soft - Tabs mapping
    tablas_server_modules <- list(
      "1" = "modules_03_tablas_Tablas1Q_SERVER",
      "2" = "modules_03_tablas_Tablas1C_SERVER",
      "3" = "modules_03_tablas_Tablas2Q_SERVER",
      "4" = "modules_03_tablas_Tablas2C_SERVER",
      "5" = "modules_03_tablas_TablasQC_SERVER"
    )
    
    observeEvent(my_case(), {
      req(base, UserSelection)
      
      selected_module_name <- tablas_server_modules[[my_case()]]
      req(selected_module_name)
      
      callModule(module = get(selected_module_name),
                 id = paste0("tablas_plot", my_case()),
                 minibase = MiniBase,
                 batalla_naval = UserSelection$batalla_naval,
                 decimales = UserSelection$decimales)
      
    })
    
    
    
    # # TABLAS!
    # callModule(module = modules_03_tablas_Tablas1Q_SERVER, id =  "graficos08",
    #            minibase = MiniBase,
    #            batalla_naval = UserSelection$batalla_naval,
    #            decimales = decimales)
    # 
    # 
    # callModule(module = modules_03_tablas_Tablas1C_SERVER, id =  "graficos09",
    #            minibase = MiniBase,
    #            batalla_naval = UserSelection$batalla_naval,
    #            decimales = decimales)
    # 
    # 
    # callModule(module = modules_03_tablas_Tablas2Q_SERVER, id =  "graficos10",
    #            minibase = MiniBase,
    #            batalla_naval = UserSelection$batalla_naval,
    #            decimales = decimales)
    # 
    # callModule(module = modules_03_tablas_Tablas2C_SERVER, id =  "graficos11",
    #            minibase = MiniBase,
    #            batalla_naval = UserSelection$batalla_naval,
    #            decimales = decimales)
    # 
    # callModule(module = modules_03_tablas_TablasQC_SERVER, id =  "graficos12",
    #            minibase = MiniBase,
    #            batalla_naval = UserSelection$batalla_naval,
    #            decimales = decimales)
    
    
    output$ui_menuGRAFICOS <- renderUI({
      
      # Si no hay orden de salir a la cancha... Nadie sale...
      if(is.null(RMedic_general())) return(NULL)
      if(!RMedic_general()) return(NULL)
      
      # Si no hay status de BaseSalida(), nos vamos...
      if(is.null(status_BaseSalida())) return(NULL)
      if(!status_BaseSalida()) return(NULL)
      
      
      fluidRow(
        column(1),
        column(10,
               h3("Menú para Gráficos"),
               BatallaNavalUI(ns("var_selection_graficos")),
               MiniBaseUI(ns("minibase_graficos")),
               Graficos1Q_UI(ns("graficos1")),
               Graficos1C_UI(ns("graficos2")),
               Graficos2Q_UI(ns("graficos3")),
               Graficos2C_UI(ns("graficos4")),
               GraficosQC_UI(ns("graficos5")),
               br(), br(), br(), br(), br(),
               modules_03_tablas_Tablas1Q_UI(ns("tablas_plot1")),
               modules_03_tablas_Tablas1C_UI(ns("tablas_plot2")),
               modules_03_tablas_Tablas2Q_UI(ns("tablas_plot3")),
               modules_03_tablas_Tablas2C_UI(ns("tablas_plot4")),
               modules_03_tablas_TablasQC_UI(ns("tablas_plot5"))
        ),
        column(1)
      )
      
      
      
    })
    
    # menuGRAFICOS <- reactive({
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
    #     title = "Gráficos", 
    #     # icon = icon("user-md"), 
    #     value = 4,
    #     fluidRow(
    #       column(1),
    #       column(10,
    #              h3("Menú para Gráficos"),
    #               BatallaNavalUI(ns("graficos01")),
    #                 MiniBaseUI(ns("graficos02")),
    #                   Graficos1Q_UI(ns("graficos03")),
    #                   Graficos1C_UI(ns("graficos04")),
    #                   Graficos2Q_UI(ns("graficos05")),
    #                   Graficos2C_UI(ns("graficos06")),
    #                   GraficosQC_UI(ns("graficos07")),
    #              br(), br(), br(), br(), br(),
    #                   Tablas1Q_UI(ns("graficos08")),
    #                   Tablas1C_UI(ns("graficos09")),
    #                   Tablas2Q_UI(ns("graficos10")),
    #                   Tablas2C_UI(ns("graficos11")),
    #                   TablasQC_UI(ns("graficos12"))
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
    # return(menuGRAFICOS)
    
  })
}