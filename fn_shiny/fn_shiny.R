

h3_mod <- function(text){
  
  new_str <- paste0("<u><b>", text, "</b></u>")
  
  new_str <- shiny::h3(shiny::HTML(new_str))
  
  return(new_str)
  
}