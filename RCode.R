library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(sf)
library(spdep)
library(leaflet)
library(INLA)
library(cowplot)


#Load Census data
load(census_data)
#This data is not public

#Load table with district codes and names
f1 <- "https://raw.githubusercontent.com/manjago85/ChOvObCR/main/Data/Map_cen_codes.csv" 
Mapcencodes <- read.csv(f1,stringsAsFactors = FALSE) %>%
  mutate(CODIGO = as.character(CODIGO))
#Load covariates values
f2 <- "https://raw.githubusercontent.com/manjago85/ChOvObCR/main/Data/vars_dist.csv"
Vardist <- read.csv(f2,stringsAsFactors = FALSE) %>%
  mutate(CODIGO = as.character(CODIGO))

# Descriptive Analysis
## Condition Prevalence
owobdistrict <- censusdb %>%
  dplyr::select(Provincia,Canton,Distrito,EstadoNutricional) %>%
  mutate(Canton = as.character(Canton),
         Distrito = as.character(Distrito)) %>%
  mutate(EstadoNutricional = recode(EstadoNutricional,
                                    '1' = "nnu",                          
                                    '2' = "nnu",
                                    '3' = "nnu",
                                    '4' = "now",
                                    '5' = "nob")) %>%
  mutate(EstadoNutricional = as.factor(EstadoNutricional)) %>% 
  group_by(Provincia,Canton,Distrito) %>% 
  count(EstadoNutricional,Distrito) %>%
  mutate(N = sum(n)) %>%  
  pivot_wider(names_from = EstadoNutricional,
              values_from = n) %>% 
  replace_na(list(now = 0,
                  nob = 0)) %>% 
  mutate(DIST_INDEX = paste0(Provincia,Canton,Distrito)) %>%
  left_join(Mapcencodes[,c("DIST_INDEX","CODIGO")],by = "DIST_INDEX")

## Load shapefile
# Shapefile url: https://github.com/manjago85/ChOvObCR/blob/main/Data/CR_district_geom.shp
f3 <- "path of CR_district_geom.shp"
# Read file
CRdist <- st_read(f3)
#Define area of interest and crop it
box <- c(xmin = -86.5, ymin = 7.5, xmax = -82.5, ymax = 11.5)
CRdistc <- st_crop(CRdist,box)

CRdistc <- CRdistc %>%
  mutate(CODIGO = as.character(CODIGO),
         NOM_DIST = as.character(NOM_DIST))
#Update the name of some cantons
CRdistc$NOM_CANT <- str_replace_all(CRdistc$NOM_CANT,c("AGUIRRE"="QUEPOS",
                                                       "VALVERDE VEGA"="SARCHI"))
#Join all tables geometry,district names and covariates
CRdistc <- CRdistc %>% left_join(owobdistrict[,c("CODIGO","N","nnu","now","nob")],
                                 by = "CODIGO") %>%
  left_join(Vardist[,c(1,6:19)],by = "CODIGO")
#Create a copy without geometries
CRdistdf <- st_drop_geometry(CRdistc)

remove(censusdb)

#With geometries
crm <- CRdistc %>% 
  # drop_na(N) %>%
  mutate(ID = factor(ID))
#Without geometries
crv <- CRdistdf %>% 
  # drop_na(N) %>%
  mutate(ID = factor(ID))

#Identify islands (geographic units with 0 neighbors)
nb <- spdep::poly2nb(crm,
                     queen = TRUE)
iv <- which(sapply(nb,FUN = function(x) sum(x) == 0))
#Island in position 79 is a district, it can't be eliminated 
iv <- iv[-79]

#Keep all districts outside iv vector
crm <-  crm %>% slice(-c(iv))
crv <- crv %>% slice(-c(iv))

#Create neighbors
crm$pid <- factor(1:nrow(crm))
crv$pid <- factor(1:nrow(crv))
nb <- spdep::poly2nb(crm,
                     queen = FALSE,
                     row.names = crm$pid)
names(nb) <- attr(nb,"region.id")

#Descriptive statistics

#Correlation
descdf <- crv %>% 
  dplyr::select(N,now,nob,
         FUERA_TRABAJO,
         POB_URBANA,
         CARENCIA,
         POB_014,
         HOG_MUJER,
         OCUPANTES,
         ESCOLA) %>%
  rename('Unemployment' = FUERA_TRABAJO,
         'Urban Population' = POB_URBANA,
         'Deprivation' = CARENCIA,
         'Population < 14' = POB_014,
         'Single Mother Homes' = HOG_MUJER,
         'Ocupants' = OCUPANTES,
         'Schooling' = ESCOLA) %>% 
  mutate(Overweight = now/N,
         Obesity = nob/N,
         .keep = 'unused') %>% 
  relocate(Overweight,Obesity)
corm <- cor(descdf,use = "complete.obs")

#Variables relationships
crvl <- crv %>% 
  dplyr::select(ID,N,now,nob,
         FUERA_TRABAJO,
         POB_URBANA,
         CARENCIA,
         POB_014,
         HOG_MUJER,
         OCUPANTES,
         ESCOLA) %>%
  rename('Unemployment' = FUERA_TRABAJO,
         'Urban Population' = POB_URBANA,
         'Deprivation' = CARENCIA,
         'Population < 14' = POB_014,
         'Single Mother Homes' = HOG_MUJER,
         'Occupants' = OCUPANTES,
         'Schooling' = ESCOLA) %>% 
  mutate(Overweight = now/N,
         Obesity = nob/N) %>%
  pivot_longer(c(Overweight,Obesity),
               names_to = "Condition",
               values_to = "Prevalence") %>%
  pivot_longer(!c(ID,Condition,Prevalence,N,now,nob),
               names_to = "Effect",
               values_to = "Value") %>%
  mutate(Effect = factor(Effect,
                         levels = c("Unemployment",
                                    "Urban Population",
                                    "Deprivation",
                                    "Population < 14",
                                    "Single Mother Homes",
                                    "Occupants",
                                    "Schooling")))

ggplot(data = crvl, 
       aes(x = Value,  y = Prevalence, color = Condition)) + 
  geom_point(size = .5) +
  geom_smooth(method = "gam") +
  scale_color_viridis_d(begin = .3,
                        end = .7) +
  facet_wrap(~ Effect,
             scales = "free") +
  labs(x = "",
       color = "") +
  theme_bw() +
  theme(legend.position = c(.5,.15))

#Function to create leaflet inst maps
insetplot <- function(c){
  c <- c +
    geom_rect(
      xmin = -84.47231,
      ymin = 9.742248,
      xmax = -83.758,
      ymax = 10.16013,
      fill = NA, 
      colour = "black",
      size = 0.6
    )
  ggdraw(c) +
    draw_plot(
      {c +
          coord_sf(
            xlim = c(-84.47231,-83.758),
            ylim = c(9.742248,10.16013),
            expand = FALSE) +
          theme(legend.position = "none")
      },
      x = 0.05,
      y = 0.125,
      width = 0.4,
      height = 0.4)
}

#Calculate prevalence values
crm <- crm %>% 
  mutate(Prevow = now/N) %>%
  mutate(Prevob = nob/N)

#Inset map of overweight prevalence
c0 <- ggplot(data = crm) +
  geom_sf(aes(fill = Prevow),color = NA) +
  scale_fill_viridis_c(option = "viridis",
                       limits = c(0,.35)) +
  theme_void() +
  labs(fill = "") +
  theme(
    legend.position = c(.90,.85)
  )
c10 <- insetplot(c0)

#Inset map of obesity prevalence
c0 <- ggplot(data = crm) +
  geom_sf(aes(fill = Prevob),color = NA) +
  scale_fill_viridis_c(option = "viridis",
                       limits = c(0,.35)) +
  theme_void() +
  labs(fill = "") + 
  theme(
    legend.position = c(.90,.85)
  )
c11 <- insetplot(c0)

# Covariates maps
c1 <- ggplot(data = crm) +
  geom_sf(aes(fill = FUERA_TRABAJO),color = NA) +
  scale_fill_viridis_c(option = "viridis") +
  labs(fill = "") +
  theme_void()
c2 <- ggplot(data = crm) +
  geom_sf(aes(fill = POB_URBANA),color = NA) +
  scale_fill_viridis_c(option = "viridis") +
  labs(fill = "") +
  theme_void()
c3 <- ggplot(data = crm) +
  geom_sf(aes(fill = CARENCIA),color = NA) +
  scale_fill_viridis_c(option = "viridis") +
  labs(fill = "") +
  theme_void()
c4 <- ggplot(data = crm) +
  geom_sf(aes(fill = POB_014),color = NA) +
  scale_fill_viridis_c(option = "viridis") +
  labs(fill = "") +
  theme_void()
c5 <- ggplot(data = crm) +
  geom_sf(aes(fill = HOG_MUJER),color = NA) +
  scale_fill_viridis_c(option = "viridis") +
  labs(fill = "") +
  theme_void()
c6 <- ggplot(data = crm) +
  geom_sf(aes(fill = OCUPANTES),color = NA) +
  scale_fill_viridis_c(option = "viridis") +
  labs(fill = "") +
  theme_void()
c7 <- ggplot(data = crm) +
  geom_sf(aes(fill = ESCOLA),color = NA) +
  scale_fill_viridis_c(option = "viridis") +
  labs(fill = "") +
  theme_void()
plot_grid(plotlist = list(c1,c2,c3,c4,c5,c6,c7),
          labels = c("Unemployment",
                     "Urban Population",
                     "Deprivation",
                     "Population < 14",
                     "Single Mother Homes",
                     "Occupants",
                     "Schooling"),
          label_size = 9, 
          nrow = 4, 
          label_x = c(0.3,0.26,0.33,0.3,
                      0.24,0.34,0.34),
          label_y = 1)

#Model
#Create subpopulations
crv <- crv %>% 
  mutate(N1 = nnu + now,
         N2 = nnu + nob)
#New index variable
crv$ID2 <- 1:nrow(crv)
#Graph of neighbors
nb2INLA("map.adj", nb)
g <- inla.read.graph(filename = "map.adj")

#Define priors
sprior <- list(
  prec = list(
    prior = "pc.prec",
    param = c(0.5 / 0.31, 0.01)),
  phi = list(
    prior = "pc",
    param = c(0.5, 2 / 3))
)

#Define model for overweight
fr <- now ~ 
  CARENCIA +
  POB_014 +
  ESCOLA + 
  I(ESCOLA^2) +
  f(ID2, model = "bym2", graph = g,hyper = sprior)
#Calculate model for N1
fit11 <- inla(fr,
              family = 'binomial',
              Ntrials = N1,
              data = crv,
              control.family = list(link = 'logit'),
              control.predictor = list(link = 1,
                                       compute = TRUE),
              control.compute = list(dic = TRUE,
                                     cpo = TRUE,
                                     waic = TRUE,
                                     return.marginals.predictor = TRUE))
#save(fit11,file = "fit11.Rdata")
#load("fit11.Rdata")
#Fixed coefficients 
fit11$summary.fixed
#Estimated values for overweight
ft11 <- fit11$summary.fitted.values
est11 <- ft11[,1]
#Marginals
ft11 <- fit11$marginals.fitted.values

#Define model for obesity
fr <- nob ~ 
  CARENCIA +
  POB_014 +
  ESCOLA + I(ESCOLA^2) +
  f(ID2, model = "bym2", graph = g,hyper = sprior)
#Calculate model for N2
fit12 <- inla(fr,
              family = 'binomial',
              Ntrials = N2,
              data = crv,
              control.family = list(link = 'logit'),
              control.predictor = list(link = 1,
                                       compute = TRUE),
              control.compute = list(dic = TRUE,
                                     cpo = TRUE,
                                     waic = TRUE,
                                     return.marginals.predictor = TRUE))
#save(fit12, file = "fit12.Rdata")
#load("fit12.Rdata")
#Fixed coefficients 
fit12$summary.fixed
#Estimated values for obesity
ft12 <- fit12$summary.fitted.values
est12 <- ft12[,1]
#Marginals
ft12 <- fit12$marginals.fitted.values

#Create matrices to save the draws for each district
ndr <- 2000
ndi <- 474
dmft11 <- matrix(NA,ncol = ndi,nrow = ndr)
dmft12 <- matrix(NA,ncol = ndi,nrow = ndr)

#Softmax function
softmaxf <- function(a,b){
  return(a/(a + b + 1))
}

#Get the draws and use softmax function
for(i in 1:ndi){
  ft11r <- inla.rmarginal(ndr,ft11[[i]])
  ft12r <- inla.rmarginal(ndr,ft12[[i]])
  aa <- ft11r/(1-ft11r)
  bb <- ft12r/(1-ft12r)
  dmft11[,i] <- softmaxf(aa,bb) 
  dmft12[,i] <- softmaxf(bb,aa)
}

#Calculate the mean
esow <- apply(dmft11,2,mean)
esob <- apply(dmft12,2,mean)

c0 <- ggplot(data = crm) +
  geom_sf(aes(fill = esow),color = NA) +
  scale_fill_viridis_c(option = "viridis",
                       limits = c(.13,.25)) +
  theme_void() +
  labs(fill = "") +
  theme(
    legend.position = c(.9,.85)
  )
c20 <- insetplot(c0)
c0 <- ggplot(data = crm) +
  geom_sf(aes(fill = esob),color = NA) +
  scale_fill_viridis_c(option = "viridis",
                       limits = c(.06,.21)) +
  theme_void() +
  labs(fill = "") + 
  theme(
    legend.position = c(.9,.85)
  )
c21 <- insetplot(c0)

#Exceedence
#Exceedance probability function
excp <- function(x,p){
  sum(x>p)/length(x)
}

#Calculate exceedance probabilities
excow <- apply(dmft11,2,excp,0.2)
excob <- apply(dmft12,2,excp,0.14)

c0 <- ggplot(data = crm) +
  geom_sf(aes(fill = excow),color = NA) +
  scale_fill_viridis_c(option = "viridis",
                       limits = c(0,1)) +
  theme_void() +
  labs(fill = "") +
  theme(
    legend.position = c(.9,.85)
  )
c30 <- insetplot(c0)
c0 <- ggplot(data = crm) +
  geom_sf(aes(fill = excob),color = NA) +
  scale_fill_viridis_c(option = "viridis",
                       limits = c(0,1)) +
  theme_void() +
  labs(fill = "") + 
  theme(
    legend.position = c(.9,.85)
  )
c31 <- insetplot(c0)

#Function to calculate standard errors as odd ratios
get.or.se <- function(model) {
  model$summary.fixed %>%
    mutate(or = exp(mean),
           var.coef = sd^2,
           or.se = sqrt(or^2 * var.coef)) %>%
    select(or.se) %>% unlist %>% unname
}

sefit11 <- get.or.se(fit11)
sefit12 <- get.or.se(fit12)
