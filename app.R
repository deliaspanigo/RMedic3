# # # 
# # #
# # #

# # # Loading...
source("global.R")



# # # User interface - UI
ui <- shiny::navbarPage(theme = "styles.css",inverse=TRUE,
      useShinyjs(),
      tags$head(tags$style(HTML("
            .selectize-input, .selectize-dropdown, .select-input, .select-dropdown,
            [type = 'number'], .radio, label, .nav-tabs, table, data.table{
            font-size: 120%;
            }
      "))),   
      title = strong("I need RMEDIC here!"),
      windowTitle = "RMedic - Medicina y R", 
      fluid = TRUE, 
      header = column(12, ""),
      footer = column(12,
                      div(id = "footer",
                          a("Consultoria Bioestadística de la Salud"), br(),
                          "Contacto: ", a("d.eliaspanigo@gmail.com"),
                          br(),
                          HTML('&copy; David Elías Panigo (2016)')
                      )
      ),
      id = "nav",
      
      

      shiny::tabPanel(title = "Inicio33", icon = icon("house"), homeTabUI("homeTab")),
      shiny::tabPanel(title = "RMedic33", icon = icon("house"), RMedicTabUI("RMedicTab")),
      shiny::tabPanel(title = "Herramientas33", source("tabs/HerramientasTab.R", encoding = "UTF-8")$value)
)




# # # Server - SERVER
server <- function(input, output, session) {
  
  # # # Section 01 - User Location  # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
  
  
  # User location
  user_location <- get_user_location()
  

  if (!is.null(user_location)) {
    # Definir el archivo de registro
    log_file <- "user_locations.log"
    
    # Crear el archivo de registro si no existe
    if (!file.exists(log_file)) {
      file.create(log_file)
    }
    
    # Save info from user on file
    log_entry <- paste(Sys.time(), user_location$ip, user_location$city, user_location$region, user_location$country, sep = ", ")
    write(log_entry, file = log_file, append = TRUE)
    
  }
  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
  
  
  
  
  # # # Server Modules 01 - Folder: modules_01_tabs
  
  # 01 - Homepage
  homeTabServer("homeTab")
  
  # 02 - RMedic Software
  RMedicTabServer("RMedicTab")
  
  # 03 - Tools
  
  
  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
  
  # 
  # 
  # 
  # juntos <-   callModule(module = Menu_DistribucionGeneral01_SERVER,
  #                        id =  "distribucion01.general",
  #                        carpeta_distribuciones = "009App/002_Distribucion_de_Probabilidades")
  # 
  # 
  # 
  # 
  # callModule(module = SideBarDistribucionElegida01_SERVER,
  #            id =  "espacio_elegido01",
  #            # la_distribucion = "001_Normal")
  #            la_distribucion = juntos)
  # 
  # callModule(module = MainPanelDistribucionElegida01_SERVER,
  #            id =  "espacio_elegido01",
  #            # la_distribucion = "001_Normal")
  #            la_distribucion = juntos)
  # 
  # callModule(module = ServerDistribucionElegida01_SERVER,
  #            id =  "espacio_elegido01",
  #            # la_distribucion = "001_Normal")
  #            la_distribucion = juntos)
  # 
  # # Distribucion Normal
  # callModule(module = Server01_Normal_Server,
  #            id =  "aver01A",
  #            # la_distribucion = "001_Normal")
  #            la_distribucion = juntos)
  # 
  # 
  # #####################################################################################
  # juntos2 <-   callModule(module = Menu_DistribucionGeneral02_SERVER,
  #                         id =  "distribucion02.general",
  #                         carpeta_distribuciones = "009App/002_Distribucion_de_Probabilidades")
  # 
  # 
  # 
  # 
  # callModule(module = SideBarDistribucionElegida02_SERVER,
  #            id =  "espacio_elegido02",
  #            # la_distribucion = "001_Normal")
  #            la_distribucion = juntos2)
  # 
  # callModule(module = MainPanelDistribucionElegida02_SERVER,
  #            id =  "espacio_elegido02",
  #            # la_distribucion = "001_Normal")
  #            la_distribucion = juntos2)
  # 
  # callModule(module = ServerDistribucionElegida02_SERVER,
  #            id =  "espacio_elegido02",
  #            # la_distribucion = "001_Normal")
  #            la_distribucion = juntos2)
  # 
  # # Distribucion Normal
  # callModule(module = Server01_Normal_Server02,
  #            id =  "aver02A",
  #            # la_distribucion = "001_Normal")
  #            la_distribucion = juntos2)
  # 
  # 
  # # Distribucion t
  # callModule(module = Server02_t_Server02,
  #            id =  "aver02B",
  #            # la_distribucion = "001_Normal")
  #            la_distribucion = juntos2)
  # 
  # # Distribucion Chi
  # callModule(module = Server03_chi_Server02,
  #            id =  "aver02C",
  #            # la_distribucion = "001_Normal")
  #            la_distribucion = juntos2)
  # 
  # # Distribucion F
  # callModule(module = Server04_f_Server02,
  #            id =  "aver02D",
  #            # la_distribucion = "001_Normal")
  #            la_distribucion = juntos2)
  
}



options(shiny.port = 3838)
options(shiny.host = "0.0.0.0")

# Ejecutar la aplicación Shiny
shinyApp(ui = ui, server = server)