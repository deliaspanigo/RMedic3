
baseSalidaUI <- function(id) {
  ns <- NS(id)
  dataTableOutput(ns("BaseSalida"))
}

baseSalidaServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$BaseSalida <- renderDataTable({
      mtcars
    })
  })
}

##############################################################


RMedicTabUI <- function(id) {
  ns <- NS(id)
  
  div(
    tags$head(
            tags$style(HTML("
            .nav-tabs {
              width: 100%;
              display: flex;
              justify-content: space-between;
            }
            .nav-tabs > li {
              flex: 1;
              text-align: center;
              font-weight: bold;
              font-family: 'Arial', sans-serif;
            }
            .nav-tabs > li > a {
              font-weight: bold;
              font-size: 18px;
              font-family: 'Arial', sans-serif;
              display: flex;
              height: 50px;
              align-items: center;
              justify-content: center;
              margin: 0; /* Eliminar margen */
              padding: 0; /* Eliminar relleno */
            }
          ")),
          tags$script(HTML("
          $(document).ready(function() {
            var scrollPos = 0;
            $('a[data-toggle=\"tab\"]').on('shown.bs.tab', function(e) {
              window.scrollTo(0, scrollPos);
            });
            $('a[data-toggle=\"tab\"]').on('click', function(e) {
              scrollPos = $(window).scrollTop();
            });
          });
        "))
    ),
    
    titlePanel("R+Medic"),
    
    br(), br(),
    fluidRow(
      #column(1),
      column(4, class = "text-center",
             shinyBS::bsButton(ns("showpanel"), "", 
                      type = "toggle", value = TRUE,
                      icon("bars"), style = "primary", size = "large"
             )
      )
    ),
    br(), br(),
    
    sidebarLayout(
      div(id = "MySidebar",
          
          sidebarPanel(id = ns("Sidebar"), 
                       #SideBarBaseUI2("base01")
                       module_pack002_import_s00_general_p01_ui(id = ns("space02_database_00")),
                       module_pack002_import_s00_general_p02_ui(id = ns("space02_database_00"))
                       )
          ),
      mainPanel(id = ns("Main"),
                uiOutput(ns("RMedicSoft")),
                #"ACA ES  MAIN PANEL",
                #"LA 01",
                #baseSalidaUI(id = ns("base02")),
                br(), br(), br(),br(),br(),br(),br()
                
      )
      # End MainPanel ------------------------------------------
    ) 
  )
}



RMedicTabServer <- function(id) {
  moduleServer(id, function(input, output, session) {
   
    ns <- session$ns
    
    observeEvent(input$showpanel, {

      if(input$showpanel == TRUE) {
        removeCssClass("Main", "col-sm-12")
        addCssClass("Main", "col-sm-8")
        shinyjs::show(id = "Sidebar")
        shinyjs::enable(id = "Sidebar")
      }
      else {
        removeCssClass("Main", "col-sm-8")
        addCssClass("Main", "col-sm-12")
        shinyjs::hide(id = "Sidebar")
      }
    })


    
    # 1 - Base ------------------------------------------------
    {
      
    sui_data_source <- module_pack002_import_s00_general_p01_server(id = "space02_database_00")
    output_list_database        <- module_pack002_import_s00_general_p02_server(id = "space02_database_00", sui_data_source)
    
    temporal_file_path <- reactive({output_list_database()$"temporal_file_path"})
    str_import_local <- reactive({output_list_database()$"str_import_local"})
    database <- reactive({output_list_database()$"database"})
    my_name <-  reactive({output_list_database()$"original_file_name"})
    
    #module_pack002_import_s00_general_p03_server(id = "space02_database_00", database)
    module_pack002_import_s00_general_p03_server(id = "space02_database_00", output_list_database = output_list_database)
    #baseSalidaServer(id = "base02")
    
    ###
    }
    ###########################################################
    
    

    RMedic_general <- reactiveVal(FALSE)
    status_BaseSalida <- reactiveVal(FALSE)
    zocalo_CIE <- reactiveVal(NULL)
    #################################################
    active_tab <- reactiveVal("1")
    observeEvent(input$"custom-tabs", {
      active_tab(input$"custom-tabs")
    })
    #################################################
    
    observeEvent(output_list_database()$"database", {
      
      req(output_list_database()$"database")
      RMedic_general(TRUE)
      status_BaseSalida(TRUE)
      active_tab("1")
    })
    
    # 2 - Control ----------------------------------------------
    observeEvent(active_tab(), {
      if (active_tab() == "2") {
        callModule(module = ModuleControlSERVER,
                   id = "menuCONTROL",
                   base = reactive(output_list_database()$"database"),
                   RMedic_general = RMedic_general,
                   status_BaseSalida = status_BaseSalida,
                   zocalo_CIE = zocalo_CIE)
      }
    })
    
    # 3 - Tablas ----------------------------------------------
    observeEvent(active_tab(), {
      if (active_tab() == "3") {
        callModule(module = ModuleTablasSERVER,
                   id = "menuTABLAS",
                   base = reactive(output_list_database()$"database"),
                   RMedic_general = RMedic_general,
                   status_BaseSalida = status_BaseSalida,
                   zocalo_CIE = zocalo_CIE)
      }
    })
    
    # 4 - Graficos ----------------------------------------------
    observeEvent(active_tab(), {
      if (active_tab() == "4") {
        callModule(module = ModuleGraficosSERVER,
                   id = "menuGRAFICOS",
                   base = reactive(output_list_database()$"database"),
                   RMedic_general = RMedic_general,
                   status_BaseSalida = status_BaseSalida,
                   zocalo_CIE = zocalo_CIE)
      }
    })
    
    # 5 - Ho ----------------------------------------------
    observeEvent(active_tab(), {
      if (active_tab() == "5") {
        callModule(module = ModuleHoSERVER,
                   id = "menuHO",
                   base = reactive(output_list_database()$"database"),
                   RMedic_general = RMedic_general,
                   status_BaseSalida = status_BaseSalida,
                   zocalo_CIE = zocalo_CIE)
      }
    })
    
    # 6 - Sobrevida ----------------------------------------------
    observeEvent(active_tab(), {
      if (active_tab() == "6") {
        callModule(module = ModuleSobrevidaSERVER,
                   id = "menuSOBREVIDA",
                   base = reactive(output_list_database()$"database"),
                   RMedic_general = RMedic_general,
                   status_BaseSalida = status_BaseSalida,
                   zocalo_CIE = zocalo_CIE)
      }
    })
      
      output[["RMedicSoft"]] <- renderUI({

        tabsetPanel(
          id = ns("custom-tabs"),
          tabPanel(title = "Base de Datos",
                 # icon = icon("user-md"),
                 value = 1,
                 br(),
                 fluidRow(
                   column(1),
                   column(10, 
                          module_pack002_import_s00_general_p03_ui(id = ns("space02_database_00"))
                   ),
                   column(1)
                   #MiBase01_UI("base01")#,
                   
                 ),
                 br(), br()
        ),
        tabPanel(title = "Control",
                 # icon = icon("user-md"),
                 value = 2,
                 br(),
                 fluidRow(
                   ModuleControlUI(id = ns("menuCONTROL"))
                   #MiBase01_UI("base01")#,
                   
                 ),
                 br(), br()
        ),
        tabPanel(title = "Tablas",
                 # icon = icon("user-md"),
                 value = 3,
                 br(),
                 fluidRow(
                   ModuleTablasUI(id = ns("menuTABLAS"))
                   #MiBase01_UI("base01")#,
                   
                 ),
                 br(), br()
        ),
        tabPanel(title = "Gráficos",
                 # icon = icon("user-md"),
                 value = 4,
                 br(),
                 fluidRow(
                   ModuleGraficosUI(id = ns("menuGRAFICOS"))
                   #MiBase01_UI("base01")#,
                   
                 ),
                 br(), br()
        ),
        tabPanel(title = "Pruebas de Hipótesis",
                 # icon = icon("user-md"),
                 value = 5,
                 br(),
                 fluidRow(
                   ModuleHoUI(id = ns("menuHO"))
                 ),
                 br(), br()
        ),
        tabPanel(title = "Sobrevida",
                 # icon = icon("user-md"),
                 value = 6,
                 br(),
                 fluidRow(
                   ModuleSobrevidaUI(id = ns("menuSOBREVIDA"))
                   #MiBase01_UI("base01")#,
                   
                 ),
                 br(), br()
        )
        )
        
      })
            # # do.call(tabsetPanel,  c(id="goku", tabs1,tabs2, tabs3, tabs4, tabs5, tabs6))
            # do.call(tabsetPanel,  c(id = "PanelRMedic",
            #                         menuBASE(),
            #                         menuCONTROL()# ,
            #                         #menuTABLAS() ,
            #                         #menuGRAFICOS() ,
            #                         #menuHO(),
            #                         #menuSOBREVIDA()
            # )
            # )
      
       #     })
      
      #})
#     
    
  })
}

