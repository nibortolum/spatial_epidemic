library(progress)
library(furrr)
library(tidyverse)
library(scales)
library(ggforce)

source("simepidemie_spatial_functions.R")


# simulation simple -------------------------------------------------------


df <- simulation_epidemie(confinement =2, nb_personnes = 200, init_prop_malade = 0.01, nb_jours = 100, space_ratio = 0.5)


#calcul du nombre de malade total
nb_malades <- rep(FALSE,200)
sum_malades <- vector()

for(i in 1:max(df$timebis)){
  malades_n <- df %>% 
    filter(timebis == i) %>%
    mutate(ismalade = ifelse(person_status == "sain", FALSE, TRUE)) %>% 
    select(ismalade) %>% 
    pull()
  nb_malades <- nb_malades|malades_n 
  sum_malades[i] <- sum(nb_malades)
  
}

plot(sum_malades)

# nombre de malade simultané par heure
df %>% 
  group_by(timebis) %>% 
  summarise(malades = sum(person_status == "malade")) %>% 
  ggplot() +
  geom_line(aes(x = timebis, y = malades)) +
  theme_bw()


# position des personnes pour un temps donné
df %>% 
  filter(timebis ==300) %>% 
  ggplot() +
  geom_point(aes(x = x, y = y, col=person_status)) +
  coord_fixed() +
  theme_bw()



# simulation sur différents degrés de confinement -------------------------

confinement <- 1:4

sim_wrapper <- function(confinement){
  df <- simulation_epidemie(confinement = confinement, progress = FALSE, nb_personnes = 100, init_prop_malade = 0.005, nb_jours = 30, space_ratio = 0.5)
  df$confinement <- rep(confinement, nrow(df))
  return(df)
}


plan(multiprocess) # permet de passer en parallel grâce au package future
out <- future_map_dfr(confinement, sim_wrapper)

# nombre de malade simultané par heure
out %>% 
  group_by(confinement, timebis) %>% 
  summarise(malades = sum(person_status == "malade")) %>% 
  ggplot() +
  geom_line(aes(x = timebis, y = malades, col = as.factor(confinement))) +
  theme_bw()



# Simulation Monte Carlo --------------------------------------------------

sim_MC <- function(i){
  # cat("MC :", i, "\n")
  confinement <- 1:4
  out <- future_map_dfr(confinement, sim_wrapper)
  simsim <- out %>% 
    group_by(confinement, timebis) %>% 
    summarise(malades = sum(person_status == "malade")) 
  
  simsim$boot <- rep(i, nrow(simsim))
  
  return(simsim)
}


plan(list(sequential, multiprocess)) # définit la topologie du parallèlisme

test <- future_map_dfr(1:20, sim_MC)

p <- test %>% 
  group_by(confinement, timebis) %>% 
  summarise(malades = mean(malades))  %>% 
  mutate(timeter = floor(timebis/24) ) %>% 
  group_by(confinement, timeter) %>% 
  summarise(malades = max(malades))%>% 
  ungroup() %>% 
  # filter(timeter < 25) %>% 
  ggplot() +
  geom_line(aes(x = timeter, y = malades, col = as.factor(confinement))) +
  theme_bw() +
  xlab("jours") +
  ylab("Nombre de malades") +
  scale_color_discrete(name = "Degré de\nconfinement") +
  ggtitle("Efficacité du confinement sur la rapidité de contagion")




#graph avec zoom sur une fenêtre de jours
p + facet_zoom(xlim = c(0,25)) 



# génération d'images pour la vidéo ---------------------------------------



png("input%03d.png", width = 1280, height = 720, res = 108)
for(i in 1:max(df$timebis)){
      day <- floor(i/24)
      p<- df %>% 
      filter(timebis ==i) %>% 
      ggplot() +
      geom_point(aes(x = x, y = y, col=person_status)) +
      coord_fixed() +
      theme_bw()+
      xlim(0,100) +
      ylim(0,100) +
      xlab("") +
      ylab("") +
      ggtitle(paste("Propagation d'une maladie, jour ", day, sep=""), "Confinement 4") +
      scale_color_manual(drop = FALSE, 
                         labels = c("Sain", "Malade"), 
                         name = "Status", 
                         values = c("green", "red"), 
                         breaks = c("sain", "malade"))
  
  print(p)
}
dev.off()



# debug zone --------------------------------------------------------------


# end debug zone ----------------------------------------------------------


