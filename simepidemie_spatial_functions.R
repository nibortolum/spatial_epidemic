
simulation_epidemie <- function(nb_personnes = 100, nb_jours = 30, confinement = 4, init_prop_malade = 0.05, progress = TRUE, space_ratio =1){
  if(confinement < 0 | confinement >5 ){
    stop("Confinement doit etre compris entre 0 et 5")
  }
  
  
  if(init_prop_malade < 0 | init_prop_malade >1 ){
    stop("init_prop_malade doit etre compris entre 0 et 1")
  }
  
  
  confinement = rescale(5 - confinement, to = c(0, 3), from = c(0, 5))
  
  limites = c(0,nb_personnes *space_ratio)
  
  df_length = nb_personnes * nb_jours * 24
  
  # df <- data.frame(trackId = vector(length = df_length), x = vector(length = df_length), y = vector(length = df_length), time = vector(length = df_length))
  
  trackId <- rep(1:nb_personnes, nb_jours * 24)
  x = vector(length = df_length)
  y = vector(length = df_length)
  time = vector(length = df_length)
  day_sick = rep(0, nb_personnes)
  immune = rep(FALSE,nb_personnes)
  
  person_status <- rep("sain", df_length)
  person_status[1:nb_personnes] <- sample(c("sain", "malade"), nb_personnes, replace = TRUE, prob = c(1 - init_prop_malade ,init_prop_malade))
  
  if(progress){
    pb <- progress_bar$new(format = "  Computing [:bar] :percent eta: :eta   :current/:total" ,total = nb_jours*24, clear = FALSE, width= 60)
    
  }
  
  RO <- 0.8
  for(i in 1:(nb_jours*24)){
    if(i == 1){
      x[1:nb_personnes] <- runif(nb_personnes, min = 0, max = nb_personnes*space_ratio)
      y[1:nb_personnes] <- runif(nb_personnes, min = 0, max = nb_personnes*space_ratio)
      # 
      # for (j in 1:nb_personnes){
      #   # df$time[j] <- start_time
      #   # df$trackId[j] <- j
      #   # df$x[j] <- df$y[j] <- j
      #   x[j] <- y[j] <- j
      # }
      
      day_sick[which(person_status[1:nb_personnes] == "malade")] <- 1
    }
    else{
      
      #move
      for (j in 1:nb_personnes){
        counter <- nb_personnes*(i-1) +j
        
        x[counter] <- setcoord(x[counter - nb_personnes], limites, rnorm(1, sd = confinement))
        y[counter] <- setcoord(y[counter - nb_personnes], limites, rnorm(1, sd = confinement))
        
        
      }
      
      #update infection status
      old_indices <- (nb_personnes*(i-2)+1):(nb_personnes*(i-1))
      new_indices <- (nb_personnes*(i-1)+1):(nb_personnes*(i)) 
      person_status[new_indices] <- person_status[old_indices]
      
      #check if people recovered and are immuned
      cured <- which(person_status[new_indices] == "malade" & day_sick == 8*24)
      day_sick[cured] <- 0
      immune[cured] <- TRUE
      
      person_status[new_indices[cured]] <- "sain"
      
      
      #find if person is in contact with infected people
      mdist <- as.matrix(dist(cbind(x[new_indices],y[new_indices])))
      mdist[which(mdist < 2)] <-1
      mdist[which(mdist != 1)] <-0
      diag(mdist) <- 0
      
      
      
      has_neighbours <- which(rowSums(mdist) != 0)
      
      for(guy in has_neighbours){
        
        if(person_status[guy + nb_personnes*(i-2)] == "malade"){
          
          person_status[guy + nb_personnes*(i-1)] <- "malade"
        }
        else{
          if(!immune[guy]){
            voisins_malades <- sum(person_status[which(mdist[guy,] !=0)] == "malade")
            person_status[guy + nb_personnes*(i-1)] <- sample(c("malade", "sain"), 
                                                              1,
                                                              prob = c(voisins_malades*RO, ifelse(voisins_malades*RO < 1,1 - (voisins_malades*RO),0)))
            
            
          }
          
          
        }
        
      }
      
      
      day_sick[which(person_status[new_indices] == "malade")] <- day_sick[which(person_status[new_indices] == "malade")] +1
      
    }
    if(progress) pb$tick()
  }
  
  
  df <- data.frame(trackId , x  , y, person_status)
  df$timebis <- rep(1:(nb_jours*24), each = nb_personnes)
  df$trackId <- as.factor(df$trackId)
  
  return(df)
}


setcoord <- function(coord, limite, movement){
  if(((coord + movement) < min(limite)) | ((coord + movement) > max(limite))){
    return(coord - movement)
  }
  else{
    return(coord + movement)
  }
}

