#-------------------------------------------------------#
# Sample code ----
# Last update: october 17, 2022
# Process tree cover raw satellite images (GFC)
# Create descriptive statistics (bar chart and maps)
# Author: Ana Pirela
#-------------------------------------------------------#

#--------------------------#
# packages ----
#--------------------------#

rm(list=ls())
pacman::p_load(tidyverse, sf, data.table, glue, raster)
# .rs.restartR()

#--------------------------#
# paths ----
#--------------------------#

datos_ori <- "01_Original_data"
datos <- "02_Data"
graficas <- "03_Graphs"
options(scipen = 999)

# Graphs options
w <- 4.5*2.5
h <- 3.2*2.5
text <- 15
d <- 900
s <- 1.2
a_line <- 0.6
a_dot <- 0.7

theme <- list(
  theme_classic(base_size = text),
  theme(axis.title.x = element_text(colour = "black"),
        axis.title.y = element_text(colour = "black"),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        legend.title = element_blank()))

map_theme <- list(
  theme(
    panel.background = element_blank(),
    plot.background = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
    axis.title = element_text(face = "bold", size = rel(1)),
    axis.title.y = element_blank(),
    axis.title.x = element_text(vjust = 0.5,colour = "black"),
    axis.text = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
  ))

col_palette <- c("#328F76", "#55A374", "#E0C775", "#E06027", "#CC2525", "#990909")


#-------------------------------------------------------#
# 0. Tree cover rasters ----
#-------------------------------------------------------#

# Tree cover and forest loss rasters
treecover <- raster(glue('{datos_ori}/GFC/treecover_colombia_completo.tif'))
losscover <- raster(glue('{datos_ori}/GFC/lossyear_colombia_completo.tif'))

#-------------------------------------------------------#
# 1. Extract and organize raw data ----
#-------------------------------------------------------#

# Create municipality code and delete islands (San Andres)
mpios <- sf::st_read(glue("{datos_ori}/DANE/mgn/MunicipiosVeredas.shp")) %>%
  janitor::clean_names() %>%
  mutate(coddepto = substr(dptompio, 1, 2)) %>%
  dplyr::rename(codmpio = dptompio) %>%
  dplyr::filter(coddepto != "88") %>% 
  dplyr::select(coddepto, codmpio)

#---------------------------#
# A. Tree cover and tree loss ----
#---------------------------# 

# Loop over municipalities to obtain their baseline tree cover and yearly forest loss
df <- lapply(1:nrow(mpios), function(x){
  
  # Select municipality polygon and create panel for its tree loss values
  print(x)
  
  cod <- mpios$codmpio[[x]]
  poly <- mpios[x,]
  df_cover <- data.frame(year = c(2000:2019))
  df_cover$codmpio <- cod
  
  # Tree loss (yearly)
  loss <- exactextractr::exact_extract(losscover, poly, progress = T) %>% as.data.frame()
  
  loss <- loss %>% 
    drop_na(value) %>% dplyr::filter(value > 0) %>% count(value) %>% 
    as.data.frame() %>%
    mutate(value = value + 2000) %>% dplyr::rename(year = value)
  
  # We count the deforested pixels for each year and multiply them by their area (30mts each)
  df_cover <- df_cover %>%
    left_join(loss, by = "year") %>%
    mutate(loss_mts = n*30, loss_ha = loss_mts/10000) %>% dplyr::rename(px_loss = n)
  
  # Tree cover: we count the % of a pixel that is covered by trees and calculate their area
  # Pixel = 30mts, if 80% of pixel are trees, then tree area is 30*0.8 = 24mts of trees
  tree <- exactextractr::exact_extract(treecover, poly, progress = T) %>% as.data.frame()
  px_tree <- as.numeric(nrow(tree))
  tree$codmpio <- cod
  tree$treecover <- (tree$value/100)*30
  
  tree <- tree %>% 
    group_by(codmpio) %>% 
    dplyr::summarise(treecover_mts = sum(treecover, na.rm = T)) %>% 
    ungroup()
  
  tree$treecover_ha <- tree$treecover_mts/10000
  tree$px_tree <- px_tree
  
  # Join tree cover and tree loss
  df_cover <- df_cover %>% left_join(tree, by = "codmpio") 
  return(df_cover)
  
}) %>% bind_rows()

# Municipalities can have years without deforestation 
df <- df %>%
  group_by(codmpio) %>%
  mutate(loss_mts = ifelse(is.na(loss_mts), 0, loss_mts),
         loss_ha = ifelse(is.na(loss_ha), 0, loss_ha),
         px_loss = ifelse(is.na(px_loss), 0, px_loss)) %>% 
  ungroup()

# Municipalities can have multiple polygons, we aggregate them
df <- df %>% 
  group_by(codmpio, year) %>%
  dplyr::summarise_all(funs(sum), na.rm = T) %>%
  ungroup() 

# Organize and save dataframe
df <- df %>%
  group_by(codmpio) %>%
  # Deforestation is cumulative
  mutate(tree_cover_mts = treecover_mts - cumsum(loss_mts),
         tree_cover_ha = treecover_ha - cumsum(loss_ha),
         px_tree_cum = px_tree - cumsum(px_loss)) %>%
  ungroup() %>%
  dplyr::select(-c(treecover_mts, treecover_ha)) %>%
  # Drop outliers in tree cover 
  dplyr::filter(tree_cover_ha >= 0)

# Deforestation is only measured between 2001-2019, we fill year 2000 with NA (no information)
df$loss_ha[df$year == 2000] <- NA
df$loss_mts[df$year == 2000] <- NA
df$px_loss[df$year == 2000] <- NA

saveRDS(df, glue('{datos}/GFC/deforestacion_municipios_2000-2019.rds'))
rm(df, mpios)

#-------------------------------------------------------#
# 2. Descriptive statistics ----
#-------------------------------------------------------#

#--------------------------#
# A. Load maps ----
#--------------------------#

# Create municipality code and delete islands (San Andres)
mpios <- sf::st_read(glue("{datos}/Mapas/mapa_municipios_colombia.shp")) %>%
  dplyr::filter(coddepto != "88") %>% 
  dplyr::select(coddepto, codmpio)

# Shapefiles for department and region division
map_dptos <- sf::st_read(glue("{datos_ori}/DANE/mapas/mapa_departamentos_colombia.shp")) %>% 
  dplyr::select(nivl_vl, nvl_lbl, X, Y) %>%
  dplyr::rename(cod_dpto = nivl_vl, nom_dpto = nvl_lbl) %>%
  dplyr::filter(cod_dpto != 88)

dpto_label <- map_dptos
sf::st_geometry(dpto_label) <- NULL

regiones <- sf::st_read(glue("{datos}/Mapas/regiones.shp"))

# PDET Municipalities
pdet <- haven::read_dta(glue("{datos_ori}/PDET/pdet.dta")) %>% janitor::clean_names() %>%
  rename(codmpio = cod_dane) %>% haven::zap_labels()

mpios_pdet <- mpios %>%
  mutate(codmpio = as.numeric(codmpio)) %>%
  left_join(pdet, by = "codmpio") %>% 
  mutate(pdet = ifelse(is.na(pdet), 0, pdet)) %>% 
  dplyr::filter(pdet == 1)

#--------------------------#
# B. Organize deforestation data ----
#--------------------------#

# Deforestation panel 2001-2019 at the municipality level
# Aggregate deforestation for pre and post period
gfc_ag <- readRDS(glue('{datos}/GFC/deforestacion_municipios_2000-2019.rds')) %>%
  dplyr::filter(year >= 2014) %>%
  mutate(post = ifelse(year >= 2016, 1, 0)) %>%
  drop_na(loss_ha)

# Keep only municipalities with deforestation above the median (>50%)
med_loss <- gfc_ag %>%
  dplyr::filter(post == 0) %>%
  dplyr::select(tree_cover_ha) %>% drop_na(tree_cover_ha)

med_loss <- median(med_loss$tree_cover_ha)

# Aggregate accumulative deforestation before and after armed conflict
gfc_ag <- gfc_ag %>% 
  dplyr::filter(tree_cover_ha > med_loss) %>%
  group_by(codmpio, post) %>%
  dplyr::summarise(loss_ha = sum(loss_ha, na.rm = T)) %>%
  ungroup() 

# Join with municipality shapefile
gfc_ag <- mpios %>% inner_join(gfc_ag, by = "codmpio")

#--------------------------#
# C. Plot maps ----
#--------------------------#

# Split data in percentiles
classes <- 6
q0 <- quantile(gfc_ag$loss_ha , na.rm=T, probs = seq(0, 1, length.out = classes + 1))
gfc_ag$q_value <- cut(gfc_ag$loss_ha, breaks = q0, include.lowest = T, dig.lab = 4)

gfc_ag$post[gfc_ag$post == 1] <- "2016-2019"
gfc_ag$post[gfc_ag$post == 0] <- "2014-2015"

g_ag <- ggplot() +
  geom_sf(data = mpios, fill = NA, color = "#606060", size = 0.05, alpha = 0.6) +
  geom_sf(data = gfc_ag, aes(fill = q_value), size = 0.01, alpha = 0.6) +
  geom_sf(data = mpios_pdet, aes(group = pdet), size = 0.25, alpha = 1, fill = NA, color = "black") +
  geom_sf(data = regiones, fill = NA, color = "black", size = 0.6, alpha = 0.5) +
  geom_text(data = regiones, aes(X, Y, label = Subregion), vjust = -0.5, color = "black",
            position = position_dodge(0.9), size = 4, alpha = 1) +
  scale_fill_manual(values = rep(col_palette, 40), na.value = "#ededed", na.translate = F) +
  guides(fill = guide_legend(ncol = 4)) +
  xlab("Hectáreas deforestadas (acumuladas)") +
  labs(fill="Hectáreas deforestadas (acumuladas)") +
  theme + map_theme +
  facet_wrap(~post)

ggsave(plot = g_ag, glue("{graficas}/mapa_deforestacion_municipal_2014-2019.jpeg"), height = h, width = w*1.3, dpi = d)
ggsave(plot = g_ag, glue("{graficas}/mapa_deforestacion_municipal_2014-2019.svg"), height = h, width = w*1.3, dpi = d)

#--------------------------#
# D. Bar chart ----
#--------------------------#

# Open deforestation panel at the municipality level
gfc <- readRDS(glue('{datos}/GFC/deforestacion_municipios_2000-2019.rds')) %>%
  dplyr::filter(year >= 2014) %>%
  drop_na(loss_ha) %>%
  mutate(codmpio = as.numeric(codmpio)) %>% 
  left_join(pdet, by = 'codmpio') %>%
  # Deforestation growth rate
  mutate(pdet = ifelse(is.na(pdet), 0, pdet)) %>%
  group_by(codmpio) %>%
  mutate(prc_loss = 100*(tree_cover_ha - lag(tree_cover_ha))/lag(tree_cover_ha)) %>%
  ungroup()

# Calculate changes in tree cover in PDET vs no PDET municipalities per year
gfc_sum <- gfc %>%
  dplyr::filter(tree_cover_ha > med_loss) %>%
  dplyr::filter(year >= 2014) %>%
  group_by(pdet, year) %>%
  summarise(prc_loss = mean(prc_loss, na.rm = T)) %>%
  ungroup() %>%
  mutate(prc_loss = prc_loss*(-1))

gfc_sum$pdet[gfc_sum$pdet == 1] <- "Municipios PDET"
gfc_sum$pdet[gfc_sum$pdet == 0] <- "Municipios no PDET"
gfc_sum$pdet = factor(gfc_sum$pdet, levels = c("Municipios PDET", "Municipios no PDET"))

# Graph
ggplot(data = gfc_sum, aes(x = year, y = prc_loss, fill = pdet)) +
  geom_bar(stat = 'identity', position = position_dodge(), alpha = 0.8) +
  geom_text(aes(label = round(prc_loss, 1)), color = "black", vjust = -0.5, 
            position = position_dodge(0.9), size = text/4, alpha = 1) +
  xlab("\nAño") + ylab("Cambio promedio en la deforestación (%)\n") +
  scale_fill_manual(values = c(col_palette[1], "#8079B4"), na.value = "#ededed", na.translate = F) +
  theme
ggsave(glue("{graficas}/barras_deforestacion_pdet_2014-2019.jpeg"), height = h, width = w, dpi = d)
