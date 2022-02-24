
##############################################################################
# PER JUGAR, UTILITZA AQUESTA FUNCIÓ I SEGUEIX LES INSTRUCCIONS DE LA CONSOLA
##############################################################################
worldle()
##############################################################################


##############################################################################
# Llibreries necessaries
##############################################################################
library(tidiverse)
library(sf)
library(rnaturalearth)
library(rvest)
library(gtools)
library(units)
##############################################################################
# Funcions necessaries
##############################################################################
solve.worldle <- function(guess, country, attempts){
  options(warn = -1)
  
  if(!(guess %in% world$name)){
    cat("El nom d'aquest país no existeix. Prova un de la següents llista.\n")
    countries <- sort(world$name) 
    View(countries)
    
    return(attempts)
  }
  
  if(guess == country$name){
    cat("Molt bé, l'has encertat! Es tractava de", guess, "\n")
    
    world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
    
    p <- 
      ggplot()+
      geom_sf(data = world, col = "gray80", fill= "white") +
      geom_sf(data = country, col = "red", fill= "red") +
      coord_sf(crs = st_crs("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0")) +
      theme_void()
    print(p)
    
  }else{
    
    guess_centroid <- st_centroid(world %>% filter(name == guess))
    count_centroid <- st_centroid(country)
    
    world_centroid = st_centroid(world)
    
    distance <- set_units(st_distance(guess_centroid,
                                             count_centroid), "km")
    
    dst_diff <- as.numeric(st_coordinates(guess_centroid) - st_coordinates(count_centroid))
    angle <- (atan2(dst_diff[1], dst_diff[2]) + pi) * (180/pi)
    
    page <- rvest::read_html('http://snowfence.umn.edu/Components/winddirectionanddegreeswithouttable3.htm')
    directions_raw <- page %>% rvest::html_node('td table') %>% rvest::html_table(header = TRUE)
    
    directions <- directions_raw %>% 
      set_names(~ tolower(sub(' Direction', '', .x))) %>% 
      slice(-1) %>% 
      separate(degree, c('degree_min', 'degree_max'), sep = '\\s+-\\s+', convert = TRUE)
    
    angle <- cut(angle, 
                 breaks = c(0, directions$degree_max, 360), 
                 labels = c(directions$cardinal, 'N')) %>%
      as.character()
    
    cat(paste0("No és ", guess,". Està a ", round(distance,0), " km cap al ", angle, "."))
    
    attempts <- attempts + 1
  }
  options(warn = 1)
  
  return(attempts)
}

worldle <- function(){

  world <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf")
  
  world <- world %>%
    filter(type %in% c("Sovereign country", "Country")) %>%
    select(name)
  
  country <- world[sample(1:nrow(world), 1),]
  
  shape.to.guess <- 
    ggplot()+
    geom_sf(data = country, col = "gray50", fill= "gray50") +
    theme_void()
  
  print(shape.to.guess)
  
  attempts <- 0
  guess <- gtools::ask(msg = "Tens 6 intents. Si no saps quin país és, pots escriure 'aturar'.\nDe quin país es tracta?")
  
  if(guess == "aturar") return(cat("Es tractava de", country$name, "\n"))
  
  attempts <- solve.worldle(guess, country, attempts)
  
  while(guess != country$name){
    guess <- gtools::ask(msg = " Prova de nou: De quin país es tracta?")
    
    if(guess == "aturar") return(cat("Es tractava de", country$name, "\n"))
    
    attempts <- solve.worldle(guess, country, attempts)
    
    if(attempts == 6){
      cat("\nHas esgotat els", attempts,"intents. Es tractava de", country$name, "\n")
      break
    }
  }
}
