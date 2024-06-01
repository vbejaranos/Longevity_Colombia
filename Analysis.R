# Load libraries
library(openxlsx)
library(tidyverse)

# Clean environment
rm(list = ls())

# scale value (10,000)
scl <- 10000

# PPP
PPP <- openxlsx::read.xlsx("Data/Implicit deflator.xlsx") %>%
  slice(3) %>% select(-c("Country_Name","Country_Code","Indicator_Code")) %>%
  pivot_longer(cols = -Indicator_Name,names_to = 'year',values_to = 'PPP_USD') %>%
  mutate(year = as.numeric(substr(year,1,4))) %>% filter(!is.na(PPP_USD)) %>%
  select(-Indicator_Name)

# Sex
sexs <- data.frame(sexo = c('FEMENINO','MASCULINO'),
                   sex = c('Female','Male'))
# Age groups
agegr <- c('80-89','90-99','100+')

# Regions
# Epidemiological
regions_ER <- c("Amazon-Orinoquia","Bogotá-Cundinamarca",
           "Caribbean","Central","Oriental","Pacific")
tabreg <- data.frame(REGION = c('Amazonia_Orinoquia','Bogota_Cundinamarca','Caribe',
                                'Central','Oriental','Pacifico'),
                     region = regions_ER)
regs_ER <- openxlsx::read.xlsx('Data/Regiones.xlsx') %>% 
  group_by(COD_DPTO,NAME_DPTO,REGION) %>% summarise(.groups = 'drop') %>%
  mutate(CODDPTO = ifelse(nchar(COD_DPTO) != 2,paste0('0',COD_DPTO),COD_DPTO)) %>%
  left_join(tabreg,by = 'REGION') %>% select(CODDPTO,NAME_DPTO,region)
# Urban-Rural
regions_UR <- c('Urban','Rural')
regs_UR <- read.csv("Data/Categorias_de_Ruralidad.csv",encoding = "UTF-8",
                             sep = ";") %>% 
  mutate(region = ifelse(Categoria_ %in% c("Ciudades y aglomeraciones","Intermedio"),
                                     "Urban","Rural"),
         CODMUNI = ifelse(nchar(Municipaly) == 5,Municipaly,
                                              formatC(Municipaly,width = 5,flag = "0"))) %>%
  select(CODMUNI,Municipali,region)

# ICD-10 cause classification
cie10 <- data.frame(code3 = c(paste0("A",15:19),"B90",paste0("B",20:24),
                              paste0("C0",0:9),paste0("C",10:97),paste0("D",50:89),
                              paste0("E",10:14),paste0("F0",0:3),paste0("G",20:21),
                              "G30","I10","I12","I15",paste0("I",20:25),paste0("I",60:69),
                              paste0("J",10:18),paste0("J",40:44),paste0("J",45:46),
                              "K70","K73","K74",paste0("O0",0:9),paste0("O",10:99),
                              paste0("P0",0:9),paste0("P",10:96),
                              paste0("V0",1:9),paste0("V",10:99),
                              paste0("W0",0:9),paste0("W",10:99),
                              paste0("X0",0:9),paste0("X",10:59),
                              paste0("Y",85:86),paste0("X",60:84),"Y870",
                              paste0("X",85:99),paste0("Y0",0:9),
                              "Y871","U071","U072","U099","U109","Z735")) %>%
  mutate(cause = case_when(code3 %in% c(paste0("A",15:19),"B90") ~ "Tuberculosis",
                           code3 %in% c(paste0("B",20:24)) ~ "HIV-AIDS",
                           code3 %in% c(paste0("C0",0:9),paste0("C",10:97)) ~ "Cancer",
                           code3 %in% c(paste0("D",50:89)) ~ "Diseases of the blood",
                           code3 %in% c(paste0("E",10:14)) ~ "Diabetes mellitus",
                           code3 %in% c(paste0("F0",0:3)) ~ "Dementia",
                           code3 %in% c(paste0("G",20:21)) ~ "Parkinson's disease",
                           code3 %in% c("G30") ~ "Alzheimer's disease",
                           code3 %in% c("I10","I12","I15") ~ "Hypertension",
                           code3 %in% c(paste0("I",20:25)) ~ "Ischemic heart disease",
                           code3 %in% c(paste0("I",60:69)) ~ "Cerebrovascular diseases",
                           code3 %in% c(paste0("J",10:18)) ~ "Influenza and pneumonia",
                           code3 %in% c(paste0("J",40:44)) ~ "Chronic obstructive pulmonary disease",
                           code3 %in% c(paste0("J",45:46)) ~ "Asthma",
                           code3 %in% c("K70","K73","K74") ~ "Chronic liver disease and cirrhosis",
                           code3 %in% c(paste0("O0",0:9),paste0("O",10:99)) ~ "Pregnancy, childbirth and the puerperium",
                           code3 %in% c(paste0("P0",0:9),paste0("P",10:96)) ~ "Certain conditions originating in the perinatal period",
                           code3 %in% c(paste0("V0",1:9),paste0("V",10:99),
                                        paste0("W0",0:9),paste0("W",10:99),
                                        paste0("X0",0:9),paste0("X",10:59),
                                        paste0("Y",85:86)) ~ "Accidents",
                           code3 %in% c(paste0("X",60:84),"Y870") ~ "Suicide",
                           code3 %in% c(paste0("X",85:99),paste0("Y0",0:9),
                                        "Y871") ~ "Assault",
                           code3 %in% c("U071","U072","U099","U109") ~ "COVID-19",
                           code3 %in% c("Z735") ~ "Social role conflict"),
         code3 = as.character(code3))
cod4 <- cie10 %>% filter(nchar(code3) == 4)

# ------------------------------ | Data reading | ------------------------- ####
# Population by department and municipality
population <- readRDS('Data/population.rds')
popMun <- readRDS('Data/populationMun.rds')
# Population by department
popTOT <- population %>% rename(year = ANO) %>% 
  group_by(year,sex,COD_DPTO) %>% summarise(pop = sum(N),.groups = "drop")
# Oldest old population
popold <- population %>% rename(year = ANO) %>% 
  filter(age >= 80) %>% 
  mutate(grupo_edad = case_when(age %in% 80:89~ "80-89",
                                age %in% 90:99~ "90-99",
                                age >= 100 ~ "100+",
                                TRUE~NA)) %>%
  group_by(year,grupo_edad,sex,COD_DPTO) %>% summarise(pop = sum(N),.groups = "drop") %>%
  mutate(grupo_edad = factor(grupo_edad,levels = agegr))
# Oldest old population by epidemiological region
pop_ER <- population %>% rename(year = ANO) %>% 
  filter(age >= 80,year >= 2008,year <= 2022) %>% 
  mutate(grupo_edad = case_when(age %in% 80:89~ "80-89",
                                age %in% 90:99~ "90-99",
                                age >= 100 ~ "100+",
                                TRUE~NA)) %>%
  left_join(regs_ER,by = c('COD_DPTO' = 'CODDPTO')) %>%
  group_by(year,grupo_edad,sex,region) %>% summarise(pop = sum(N),.groups = "drop") %>%
  mutate(grupo_edad = factor(grupo_edad,levels = agegr))
# Oldest old population by urban-rural region
pop_UR <- popMun %>% rename(year = ANO) %>% 
  filter(age >= 80,year >= 2008,year <= 2022) %>% 
  mutate(grupo_edad = case_when(age >= 80~ "80+",
                                TRUE~NA)) %>%
  left_join(regs_UR,by = c('MPIO' = 'CODMUNI')) %>%
  group_by(year,grupo_edad,sex,region) %>% summarise(pop = sum(N),.groups = "drop")

# Vital statistics deaths
deaths <- readRDS('Data/deaths.rds') %>% 
  mutate(CODDPTO = formatC(ifelse(!(is.na(CODPTORE)|CODPTORE %in% c(1,75))&!(is.na(CODMUNRE)|CODMUNRE == 999),CODPTORE,COD_DPTO),width = 2,flag = '0'),
         CODMUNI = paste0(formatC(ifelse(!(is.na(CODPTORE)|CODPTORE %in% c(1,75))&!(is.na(CODMUNRE)|CODMUNRE == 999),CODPTORE,COD_DPTO),width = 2,flag = '0'),
                           formatC(ifelse(!(is.na(CODPTORE)|CODPTORE %in% c(1,75))&!(is.na(CODMUNRE)|CODMUNRE == 999),CODMUNRE,COD_MUNIC),width = 3,flag = "0")))

# Total health spending
GD_tot <- readRDS('Data/QueryTot.rds') %>% 
  mutate(age2 = as.numeric(gsub("[Dea ]","",substr(age,1,6)))) %>%
  filter(age2 >= 80,sexo %in% c('FEMENINO','MASCULINO')) %>% 
  mutate(grupo_edad = case_when(age2 %in% 80:89~ "80-89",
                                age2 %in% 90:99~ "90-99",
                                age2 >= 100 ~ "100+",
                                TRUE~NA),
         year = as.numeric(year)) %>% group_by(year,grupo_edad,sexo) %>%
  summarise(vm = sum(vm),pu = sum(pu),aten = sum(aten),.groups = "drop") %>%
  mutate(grupo_edad = factor(grupo_edad,levels = agegr),
         sexo = factor(sexo,labels = c('Female','Male'))) %>% 
  left_join(PPP,by = 'year') %>% mutate(ppp = vm/PPP_USD)
# Health spending by condition
GD_cause <- readRDS('Data/QueryC.rds') %>% 
  mutate(age2 = as.numeric(gsub("[Dea ]","",substr(age,1,6)))) %>%
  filter(age2 >= 80,sexo %in% c('FEMENINO','MASCULINO')) %>% rename(cause = cie10) %>%
  mutate(grupo_edad = case_when(age2 %in% 80:89~ "80-89",
                                age2 %in% 90:99~ "90-99",
                                age2 >= 100 ~ "100+",
                                TRUE~NA),
         year = as.numeric(year)) %>% group_by(year,cause,grupo_edad,sexo) %>%
  summarise(vm = sum(vm),pu = sum(pu),aten = sum(aten),.groups = "drop") %>%
  mutate(grupo_edad = factor(grupo_edad,levels = agegr),
         sexo = factor(sexo,labels = c('Female','Male'))) %>% 
  left_join(PPP,by = 'year') %>% mutate(ppp = vm/PPP_USD)
# Health spending by cause and epidemiological region
GD_cER <- readRDS('Data/QueryCER.rds') %>% 
  mutate(age2 = as.numeric(gsub("[Dea ]","",substr(age,1,6)))) %>%
  filter(age2 >= 80,sexo %in% c('FEMENINO','MASCULINO')) %>%
  mutate(grupo_edad = case_when(age2 %in% 80:89~ "80-89",
                                age2 %in% 90:99~ "90-99",
                                age2 >= 100 ~ "100+",
                                TRUE~NA),
         year = as.numeric(year)) %>% group_by(year,cause,region,grupo_edad,sexo) %>%
  summarise(vm = sum(vm),pu = sum(pu),aten = sum(aten),.groups = "drop") %>%
  mutate(grupo_edad = factor(grupo_edad,levels = agegr),
         sexo = factor(sexo,labels = c('Female','Male'))) %>% 
  left_join(PPP,by = 'year') %>% mutate(ppp = vm/PPP_USD)
# Health spending by cause and urban-rural region
GD_cUR <- readRDS('Data/QueryCUR.rds') %>% 
  mutate(age2 = as.numeric(gsub("[Dea ]","",substr(age,1,6)))) %>%
  filter(age2 >= 80,sexo %in% c('FEMENINO','MASCULINO')) %>%
  mutate(grupo_edad = case_when(age2 %in% 80:89~ "80-89",
                                age2 %in% 90:99~ "90-99",
                                age2 >= 100 ~ "100+",
                                TRUE~NA),
         year = as.numeric(year)) %>% group_by(year,cause,region,grupo_edad,sexo) %>%
  summarise(vm = sum(vm),pu = sum(pu),aten = sum(aten),.groups = "drop") %>%
  mutate(grupo_edad = factor(grupo_edad,levels = agegr),
         sexo = factor(sexo,labels = c('Female','Male'))) %>% 
  left_join(PPP,by = 'year') %>% mutate(ppp = vm/PPP_USD)
# Health attentions by subgroup ICD-10 
GD_freq <- readRDS('Data/QueryFreq.rds') %>% 
  mutate(age2 = as.numeric(gsub("[Dea ]","",substr(age,1,6)))) %>%
  filter(age2 >= 80,sexo %in% c('FEMENINO','MASCULINO')) %>%
  mutate(grupo_edad = case_when(age2 %in% 80:89~ "80-89",
                                age2 %in% 90:99~ "90-99",
                                age2 >= 100 ~ "100+",
                                TRUE~NA),
         year = as.numeric(year)) %>% group_by(year,subgroup,grupo_edad,sexo) %>%
  summarise(vm = sum(vm),pu = sum(pu),aten = sum(aten),.groups = "drop") %>%
  mutate(grupo_edad = factor(grupo_edad,levels = agegr),
         sexo = factor(sexo,labels = c('Female','Male'))) %>% 
  left_join(PPP,by = 'year') %>% mutate(ppp = vm/PPP_USD)

# Mortality
# DANE codes
# 24 = De 80 a 84 años
# 25 = De 85 a 89 años
# 26 = De 90 a 94 años
# 27 = De 95 a 99 años
# 28 = De 100 años y más

# Deaths by cause
dold <- deaths %>% filter(GRU_ED1 %in% 24:28,SEXO != 3) %>% rename(year = ANO) %>%
  mutate(code3 = ifelse(C_BAS1 %in% cod4$code3,C_BAS1,substr(C_BAS1,1,3)),
         grupo_edad = case_when(GRU_ED1 %in% 24:25~"80-89",
                                GRU_ED1 %in% 26:27~"90-99",
                                GRU_ED1 %in% 28~"100+"),
         sex = factor(SEXO,labels = c('Male','Female'))) %>%
  left_join(cie10,by = "code3") %>%
  replace_na(list(cause = 'Other')) %>%
  group_by(year,cause,grupo_edad,sex) %>% summarise(deaths = n(),.groups = "drop") %>%
  mutate(grupo_edad = factor(grupo_edad,levels = agegr))
# Deaths by cause and epidemiological region
d_ER <- deaths %>% filter(GRU_ED1 %in% 24:28,SEXO != 3) %>% rename(year = ANO) %>%
  mutate(code3 = ifelse(C_BAS1 %in% cod4$code3,C_BAS1,substr(C_BAS1,1,3)),
         grupo_edad = case_when(GRU_ED1 %in% 24:25~"80-89",
                                GRU_ED1 %in% 26:27~"90-99",
                                GRU_ED1 %in% 28~"100+"),
         sex = factor(SEXO,labels = c('Male','Female'))) %>%
  left_join(cie10,by = "code3") %>% left_join(regs_ER,by = "CODDPTO") %>%
  replace_na(list(cause = 'Other')) %>%
  group_by(year,cause,grupo_edad,sex,region) %>% summarise(deaths = n(),.groups = "drop") %>%
  mutate(grupo_edad = factor(grupo_edad,levels = agegr))
# Deaths by cause and urban-rural region
d_UR <- deaths %>% filter(GRU_ED1 %in% 24:28,SEXO != 3) %>% rename(year = ANO) %>%
  mutate(code3 = ifelse(C_BAS1 %in% cod4$code3,C_BAS1,substr(C_BAS1,1,3)),
         grupo_edad = case_when(GRU_ED1 %in% 24:28~"80+"),
         sex = factor(SEXO,labels = c('Male','Female'))) %>%
  left_join(cie10,by = "code3") %>% left_join(regs_UR,by = "CODMUNI") %>%
  replace_na(list(cause = 'Other')) %>%
  group_by(year,cause,grupo_edad,sex,region) %>% summarise(deaths = n(),.groups = "drop") %>%
  filter(!is.na(region))

# Specific mortality rate by cause and regions
tem <- dold %>% left_join(popold %>% group_by(year,grupo_edad,sex) %>% 
                            summarise(pop = sum(pop),.groups = 'drop'),by = c('year','grupo_edad','sex')) %>%
  mutate(rate = ifelse(is.na(deaths),0,deaths/pop))
tem_ER <- d_ER %>% left_join(pop_ER,by = c('year','grupo_edad','sex','region')) %>%
  mutate(rate = ifelse(is.na(deaths),0,deaths/pop))
tem_UR <- d_UR %>% left_join(pop_UR,by = c('year','grupo_edad','sex','region')) %>%
  mutate(rate = ifelse(is.na(deaths),0,deaths/pop))

# ------------------------- | Tables | ------------------------------------ ####

wb <- createWorkbook()
# wb <- loadWorkbook('Tabs.xlsx')

# Table 1. Prevalence and number of the oldest old
addWorksheet(wb,'t1')
t1a <- popold %>% group_by(year,grupo_edad) %>% summarise(popold = sum(pop)) %>%
  left_join(popTOT %>% group_by(year) %>% summarise(poptot = sum(pop))) %>%
  ungroup %>% mutate(prop = popold/poptot*scl) %>%
  filter(year %in% 2008:2022) 
T1 <- bind_rows(t1a %>% select(-c(popold,poptot)) %>% mutate(type = 'Prev') %>%
  pivot_wider(names_from = year,values_from = prop),
  t1a %>% select(-c(prop,poptot)) %>% mutate(type = 'Numb') %>%
    pivot_wider(names_from = year,values_from = popold)) 
writeData(wb,'t1',T1)

# Table S2. Morbidity top 10 frequencies of use of health care services
addWorksheet(wb,'ts2')
Ts2 <- GD_freq %>% mutate(freq = aten/pu) %>% select(-c(vm,pu,aten,PPP_USD,ppp)) %>%
  group_by(year,grupo_edad,sexo) %>% arrange(freq %>% desc) %>%
  arrange(year,grupo_edad,sexo) %>% top_n(10)
writeData(wb,'ts2',Ts2)

# Table S3. Morbidity top 10 prevalence
addWorksheet(wb,'ts3')
Ts3 <- GD_freq %>% select(-c(vm,aten,PPP_USD,ppp)) %>% 
  left_join(GD_tot %>% select(-c(vm,aten,PPP_USD,ppp)) %>% rename(totpu = pu),
                            by = c('year','grupo_edad','sexo')) %>%
  mutate(prev = pu/totpu*100) %>% 
  group_by(year,grupo_edad,sexo) %>% arrange(prev %>% desc) %>%
  arrange(year,grupo_edad,sexo) %>% top_n(10)
writeData(wb,'ts3',Ts3)
# Save
saveWorkbook(wb,'Tables.xlsx',overwrite = TRUE)
rm(wb)

# ------------------------ | Figures | ------------------------------------ ####

# Color palette
Pal.1 = RColorBrewer::brewer.pal(n=8, name = "PuBu")[c(5,7)]
Pal.2 = RColorBrewer::brewer.pal(n = 8, name = "BuGn")[c(4,7)]
Palb.1 = RColorBrewer::brewer.pal(n=8, name = "PuBu")[c(3,8)]
Palb.2 = RColorBrewer::brewer.pal(n = 8, name = "BuGn")[c(3,8)]
Palete = c(Pal.1[2],Pal.1[1],Pal.2[2],Pal.2[1])
Paleteb = c(Palb.1[1],Palb.1[2],Palb.2[1],Palb.2[2])

# Figure 1. Population pyramid 
pop_tot <- population %>% group_by(ANO) %>% summarise(pop = sum(N),.groups = "drop")
ggpir <- population %>% group_by(ANO,age,sex) %>% summarise(pop = sum(N),.groups = "drop") %>% 
  mutate(agegr = cut(age,breaks = c(seq(0,100,5),Inf),right = F,
                                     labels = c('0-4','5-9','10-14','15-19','20-24',paste0(seq(25,95,5),'-',seq(29,99,5)),
                                                '100+'))) %>%
  group_by(ANO,sex,agegr) %>% summarise(n = sum(pop),.groups = 'drop') %>%
  left_join(pop_tot,by = c('ANO')) %>%
  mutate(sex = factor(sex,labels = c('Male','Female')),
         label = paste(sex,ANO),prop = n/pop*100) %>%
  filter(ANO %in% c(2005,2018))
ggplot(data = ggpir, mapping = aes(x = agegr, y = prop, fill = label, color = label)) +
  geom_bar(data = subset(ggpir, label == "Female 2005"), aes(x = agegr, y = -1*prop), stat = "identity", width = 1.0, alpha = 0.8)  + 
  geom_bar(data = subset(ggpir, label == "Female 2018"), aes(x = agegr, y = -1*prop), stat = "identity", width = 1.0, alpha = 0.4)  +
  geom_bar(data = subset(ggpir, label == "Male 2005"), stat = "identity", width = 1.0, alpha = 0.8)  + 
  geom_bar(data = subset(ggpir, label == "Male 2018"), stat = "identity", width = 1.0, alpha = 0.4)  + 
  coord_flip() + scale_y_continuous(breaks = c(seq(-20,0,1),seq(1,20,1)),
                                    labels = function(x){abs(x)}) +
  labs(y = "Proportion (%)", x = "Age", color = "", fill ="") +
  scale_fill_manual(values = Palete) + 
  scale_color_manual(values = Paleteb) + 
  theme_minimal(base_size = 18) +
  theme(legend.position = "bottom",strip.text = element_text(face = 'bold'))
ggsave('Figures/Pir.png',width = 14,height = 7)


# Figure 2. Health spending per capita
GD_cause %>% mutate(per_capita = (ppp/pu)) %>%
  filter(!cause %in% c("Other","Pregnancy, childbirth and the puerperium")) %>%
  ggplot(aes(x = year,y = cause,fill = per_capita/1e3)) +
  facet_grid(sexo~grupo_edad) + geom_tile() +
  scale_fill_gradient(low = 'white',high = 'red',na.value = 'gray') +
  scale_x_continuous(breaks = 2013:2021) +
  labs(fill = 'Expenditure per capita PPP USD 2017 (thousands)', x= 'Year',y = 'Health condition') +
  theme_minimal() + theme(legend.position = 'bottom',
                          legend.key.width = unit(1,'cm'))
ggsave('Figures/HSpC.png',width = 14,height = 7)

# Figure 3. Frequency of use of health care services
GD_cause %>% mutate(per_capita = (aten/pu)) %>%
  filter(!cause %in% c("Other","Pregnancy, childbirth and the puerperium")) %>%
  ggplot(aes(x = year,y = cause,fill = per_capita)) +
  facet_grid(sexo~grupo_edad) + geom_tile() +
  scale_fill_gradient(low = 'white',high = 'red',na.value = 'gray') +
  scale_x_continuous(breaks = 2013:2021) +
  labs(fill = 'Frequency of use per capita', x= 'Year',y = 'Health condition') +
  theme_minimal() + theme(legend.position = 'bottom',
                          legend.key.width = unit(1,'cm'))
ggsave('Figures/FUpC.png',width = 14,height = 7)

# Figure 4. Mortality rates
tem %>% filter(!cause %in% c("Other","Pregnancy, childbirth and the puerperium","Certain conditions originating in the perinatal period"),
               year %in% 2008:2022) %>%
  ggplot(aes(x = year,y = cause,fill = rate*scl)) +
  facet_grid(sex~grupo_edad) + geom_tile() +
  scale_fill_gradient(low = 'white',high = 'red',na.value = 'gray') +
  scale_x_continuous(breaks = 2008:2022) +
  labs(fill = 'Mortality rate (x 10,000)',x = 'Year',y = 'Cause of death') +
  theme_minimal() + theme(legend.position = 'bottom',
                          legend.key.width = unit(1,'cm'),
                          axis.text.x = element_text(angle = 90))
ggsave('Figures/MR.png',width = 14,height = 7)

# Figure S1. Maps of prevalence
map1 <- readRDS("Data/MunicipiosBien.rds")
crs <- sf::st_crs(map1)
map <- sf::st_read('Data/Mapa/MGN_DPTO_POLITICO.shp')
map <- sf::st_cast(map, "POLYGON")
map <- sf::st_transform(map, crs)
map <- map %>% filter(DPTO_CCDGO != '88') %>% 
  bind_rows(map1 %>% rename(DPTO_CCDGO = DPTO) %>% filter(DPTO_CCDGO == '88'))
rm(map1)

map_prev <- popold %>% group_by(year,grupo_edad,COD_DPTO) %>% summarise(popold = sum(pop)) %>%
  left_join(popTOT %>% group_by(year,COD_DPTO) %>% summarise(poptot = sum(pop))) %>%
  ungroup %>% mutate(prop = popold/poptot*scl) %>%
  filter(year %in% c(2005,seq(2010,2050,10)))
m80 <- map %>% left_join(map_prev %>% filter(grupo_edad == '80-89'),
                         by = c('DPTO_CCDGO'='COD_DPTO'),relationship = 'many-to-many') %>%
  ggplot(aes(fill = prop)) + geom_sf() + 
  labs(fill = 'Prevalence octogenarians') +
  facet_wrap(year~.,nrow = 1) + theme_void(base_size = 22) + 
  scale_fill_gradient2(high = '#0570B0',na.value = "gray75") +
  theme(legend.position = 'bottom',
        legend.key.width = unit(1,'cm'),
        legend.text = element_text(size = 11))
m90 <- map %>% left_join(map_prev %>% filter(grupo_edad == '90-99'),
                         by = c('DPTO_CCDGO'='COD_DPTO'),relationship = 'many-to-many') %>%
  ggplot(aes(fill = prop)) + geom_sf() + 
  labs(fill = 'Prevalence nonagenarians') +
  facet_wrap(year~.,nrow = 1) + theme_void(base_size = 22) + 
  scale_fill_gradient2(high = '#238B45',midpoint = 0,
                       na.value = "gray75") +
  theme(legend.position = 'bottom',
        legend.key.width = unit(1,'cm'),
        legend.text = element_text(size = 11))
m100 <- map %>% left_join(map_prev %>% filter(grupo_edad == '100+'),
                          by = c('DPTO_CCDGO'='COD_DPTO'),relationship = 'many-to-many') %>%
  ggplot(aes(fill = prop)) + geom_sf() + 
  labs(fill = 'Prevalence centenarians') +
  facet_wrap(year~.,nrow = 1) + theme_void(base_size = 22) + 
  scale_fill_gradient2(na.value = "gray75") +
  theme(legend.position = 'bottom',
        legend.key.width = unit(1,'cm'),
        legend.text = element_text(size = 11))
library(patchwork)
m80/m90/m100

ggsave(filename = paste0('Figures/Mapa_Prevalence.png'),
       units = 'cm',width = 40,height = 30,limitsize = F)

# Figure S2-S7. Heatmaps by regions ####

# Figure S2. Health spending per capita by urbal-rural regions
GD_cUR %>% mutate(per_capita = (ppp/pu),per_aten = (ppp/aten),aten_pu = (aten/pu)) %>% 
  filter(sexo == 'Female') %>%
  filter(!cause %in% c("Other","Pregnancy, childbirth and the puerperium")) %>% 
  ggplot(aes(x = year,y = cause,fill = per_capita/1e3)) + 
  facet_grid(grupo_edad~region) + geom_tile() +
  scale_fill_gradient(low = 'white',high = 'red',na.value = 'gray') +
  scale_x_continuous(breaks = 2013:2021) + 
  labs(fill = 'Expenditure per capita PPP USD 2017 (thousands)', x= 'Year',y = 'Health condition') +
  theme_minimal() + theme(legend.position = 'bottom',
                          axis.text.x = element_text(size = 8))
ggsave(filename = 'Figures/UpC_F.png',width = 12,height = 10)
GD_cUR %>% mutate(per_capita = (ppp/pu),per_aten = (ppp/aten),aten_pu = (aten/pu)) %>% 
  filter(sexo == 'Male') %>%
  filter(!cause %in% c("Other","Pregnancy, childbirth and the puerperium")) %>% 
  ggplot(aes(x = year,y = cause,fill = per_capita/1e3)) + 
  facet_grid(grupo_edad~region) + geom_tile() +
  scale_fill_gradient(low = 'white',high = 'red',na.value = 'gray') +
  scale_x_continuous(breaks = 2013:2021) + 
  labs(fill = 'Expenditure per capita PPP USD 2017 (thousands)', x= 'Year',y = 'Health condition') +
  theme_minimal() + theme(legend.position = 'bottom',
                          axis.text.x = element_text(size = 8))
ggsave(filename = 'Figures/UpC_M.png',width = 12,height = 10)

# Figure S3. Health spending per capita by epidemiological regions
GD_cER %>% mutate(per_capita = (ppp/pu),per_aten = (ppp/aten),aten_pu = (aten/pu)) %>% 
  filter(sexo == 'Female') %>%
  filter(!cause %in% c("Other","Pregnancy, childbirth and the puerperium")) %>% 
  ggplot(aes(x = year,y = cause,fill = per_capita/1e3)) + 
  facet_grid(grupo_edad~region) + geom_tile() +
  scale_fill_gradient(low = 'white',high = 'red',na.value = 'gray') +
  scale_x_continuous(breaks = 2013:2021) + 
  labs(fill = 'Expenditure per capita PPP USD 2017 (thousands)', x= 'Year',y = 'Health condition') +
  theme_minimal() + theme(legend.position = 'bottom',
                          axis.text.x = element_text(size = 8))
ggsave(filename = 'Figures/EpC_F.png',width = 20,height = 10)
GD_cER %>% mutate(per_capita = (ppp/pu),per_aten = (ppp/aten),aten_pu = (aten/pu)) %>% 
  filter(sexo == 'Male') %>%
  filter(!cause %in% c("Other","Pregnancy, childbirth and the puerperium")) %>% 
  ggplot(aes(x = year,y = cause,fill = per_capita/1e3)) + 
  facet_grid(grupo_edad~region) + geom_tile() +
  scale_fill_gradient(low = 'white',high = 'red',na.value = 'gray') +
  scale_x_continuous(breaks = 2013:2021) + 
  labs(fill = 'Expenditure per capita PPP USD 2017 (thousands)', x= 'Year',y = 'Health condition') +
  theme_minimal() + theme(legend.position = 'bottom',
                          axis.text.x = element_text(size = 8))
ggsave(filename = 'Figures/EpC_M.png',width = 20,height = 10)

# Figure S4. Frequency of use of health care services by urbal-rural regions
GD_cUR %>% mutate(per_capita = (ppp/pu),per_aten = (ppp/aten),aten_pu = (aten/pu)) %>% 
  filter(sexo == 'Female') %>%
  filter(!cause %in% c("Other","Pregnancy, childbirth and the puerperium")) %>% 
  ggplot(aes(x = year,y = cause,fill = aten_pu)) + 
  facet_grid(grupo_edad~region) + geom_tile() +
  scale_fill_gradient(low = 'white',high = 'red',na.value = 'gray') +
  scale_x_continuous(breaks = 2013:2021) + 
  labs(fill = 'Frequency of use per capita', x= 'Year',y = 'Health condition') +
  theme_minimal() + theme(legend.position = 'bottom',
                          axis.text.x = element_text(size = 8))
ggsave(filename = 'Figures/UApC_F.png',width = 12,height = 10)
GD_cUR %>% mutate(per_capita = (ppp/pu),per_aten = (ppp/aten),aten_pu = (aten/pu)) %>% 
  filter(sexo == 'Male') %>%
  filter(!cause %in% c("Other","Pregnancy, childbirth and the puerperium")) %>% 
  ggplot(aes(x = year,y = cause,fill = aten_pu)) + 
  facet_grid(grupo_edad~region) + geom_tile() +
  scale_fill_gradient(low = 'white',high = 'red',na.value = 'gray') +
  scale_x_continuous(breaks = 2013:2021) + 
  labs(fill = 'Frequency of use per capita', x= 'Year',y = 'Health condition') +
  theme_minimal() + theme(legend.position = 'bottom',
                          axis.text.x = element_text(size = 8))
ggsave(filename = 'Figures/UApC_M.png',width = 12,height = 10)

# Figure S5. Frequency of use of health care services by epidemiological regions
GD_cER %>% mutate(per_capita = (ppp/pu),per_aten = (ppp/aten),aten_pu = (aten/pu)) %>% 
  filter(sexo == 'Female') %>%
  filter(!cause %in% c("Other","Pregnancy, childbirth and the puerperium")) %>% 
  ggplot(aes(x = year,y = cause,fill = aten_pu)) + 
  facet_grid(grupo_edad~region) + geom_tile() +
  scale_fill_gradient(low = 'white',high = 'red',na.value = 'gray') +
  scale_x_continuous(breaks = 2013:2021) + 
  labs(fill = 'Frequency of use per capita', x= 'Year',y = 'Health condition') +
  theme_minimal() + theme(legend.position = 'bottom',
                          axis.text.x = element_text(size = 8))
ggsave(filename = 'Figures/EApC_F.png',width = 20,height = 10)
GD_cER %>% mutate(per_capita = (ppp/pu),per_aten = (ppp/aten),aten_pu = (aten/pu)) %>% 
  filter(sexo == 'Male') %>%
  filter(!cause %in% c("Other","Pregnancy, childbirth and the puerperium")) %>% 
  ggplot(aes(x = year,y = cause,fill = aten_pu)) + 
  facet_grid(grupo_edad~region) + geom_tile() +
  scale_fill_gradient(low = 'white',high = 'red',na.value = 'gray') +
  scale_x_continuous(breaks = 2013:2021) + 
  labs(fill = 'Frequency of use per capita', x= 'Year',y = 'Health condition') +
  theme_minimal() + theme(legend.position = 'bottom',
                          axis.text.x = element_text(size = 8))
ggsave(filename = 'Figures/EApC_M.png',width = 20,height = 10)


# Figure S6. Mortality rates by urbal-rural regions
tem_UR %>% filter(!cause %in% c("Other","Pregnancy, childbirth and the puerperium","Certain conditions originating in the perinatal period"),
                  year %in% c(2008:2022)) %>% 
  ggplot(aes(x = year,y = cause,fill = rate*scl)) + 
  facet_grid(sex~region) + geom_tile() +
  scale_fill_gradient(low = 'white',high = 'red',na.value = 'gray') +
  scale_x_continuous(breaks = 2008:2022) + 
  labs(fill = 'Mortality rate (x 10,000)',x = 'Year',y = 'Cause of death') +
  theme_minimal() + theme(legend.position = 'bottom',
                          legend.key.width = unit(1,'cm'),
                          axis.text.x = element_text(angle = 90))
ggsave(filename = 'Figures/UM.png',width = 12,height = 10)

# Figure S7. Mortality rates by epidemiological regions
tem_ER %>% filter(sex == 'Female') %>%
  filter(!cause %in% c("Other","Pregnancy, childbirth and the puerperium","Certain conditions originating in the perinatal period"),
         year %in% c(2008:2022)) %>% 
  ggplot(aes(x = year,y = cause,fill = rate*scl)) + 
  facet_grid(grupo_edad~region) + geom_tile() +
  scale_fill_gradient(low = 'white',high = 'red',na.value = 'gray') +
  scale_x_continuous(breaks = 2008:2022) + 
  labs(fill = 'Mortality rate (x 10,000)',x = 'Year',y = 'Cause of death') +
  theme_minimal() + theme(legend.position = 'bottom',
                          legend.key.width = unit(1,'cm'),
                          axis.text.x = element_text(angle = 90))
ggsave(filename = 'Figures/EM_F.png',width = 20,height = 10)
tem_ER %>% filter(sex == 'Male') %>%
  filter(!cause %in% c("Other","Pregnancy, childbirth and the puerperium","Certain conditions originating in the perinatal period"),
         year %in% c(2008:2022)) %>% 
  ggplot(aes(x = year,y = cause,fill = rate*scl)) + 
  facet_grid(grupo_edad~region) + geom_tile() +
  scale_fill_gradient(low = 'white',high = 'red',na.value = 'gray') +
  scale_x_continuous(breaks = 2008:2022) + 
  labs(fill = 'Mortality rate (x 10,000)',x = 'Year',y = 'Cause of death') +
  theme_minimal() + theme(legend.position = 'bottom',
                          legend.key.width = unit(1,'cm'),
                          axis.text.x = element_text(angle = 90))
ggsave(filename = 'Figures/EM_M.png',width = 20,height = 10)
