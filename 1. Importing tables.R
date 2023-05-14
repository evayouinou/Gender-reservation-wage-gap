################################################################################
#                                                                              #
#                 Applied Labor Economics - Gender reservation wage gap        #
#                      Eva Youniou & Audin Roger                               #
#                                                                              #
################################################################################

# Input : FH-DADS database
# Output : formated database for the study with selected variables 

# Librairies
library(dplyr)
library(ggplot2)


############## Importing data ##################################################

data_dads <- read.csv("xx.CSV")
data_de <- read.csv("xx.csv")

############## Filtering data to select a meaningful sample ####################

data_dads = data_dads %>% filter(dp>=0) #remove only 2 obs
data_dads = data_dads %>% filter(ce!="A") #remove paid unemployed workers
#356,551 unique idfhda left
data_dads$ce = ifelse(data_dads$ce=="",NA,data_dads$ce)
data_dads = data_dads %>% filter(filtre==1) #remove jobs tagged as annex
#338,301 unique idfhda left
data_dads = data_dads %>% filter(data_dads$ce!="D") #338,005 unique idfhda left
data_dads = data_dads %>% filter(data_dads$nbheur!=0)

#Gross salary full-time equivalent
data_dads$sb_h = data_dads$sb/data_dads$nbheur
summary(data_dads$sb_h)

#remove hourly salary below the SMIC in 2004
data_dads = data_dads %>% filter(data_dads$sb_h>=7.61)

#remove extreme hourly salary above the 99th percentile
sb_h_q = quantile(data_dads$sb_h, probs=0.99, na.rm=T)

data_dads = data_dads %>% filter(data_dads$sb_h <= sb_h_q)

#remove people looking for a part time
data_de = data_de %>% filter(data_de$temps == 1)

#define the reservation wage as hourly
data_de$rw_h <- ifelse(data_de$salunit == "M" & data_de$temps==1,data_de$salmt/140,
                       ifelse(data_de$salunit == "H" & data_de$temps==1, data_de$salmt,
                              ifelse(data_de$salunit == "A" & data_de$temps==1, data_de$salmt/1680, 
                                     NA)))

#Exclude reservation wage above the 95th percentile
rw_h_q = quantile(data_de$rw_h, probs = 0.95, na.rm = T)

data_de = data_de %>% filter(data_de$rw_h <= rw_h_q)
data_de = data_de %>% filter(data_de$rw_h >=7.61)

#create a variable for unemployment duration

data_de$datins = as.Date(data_de$datins)
data_de$datann = as.Date(data_de$datann)

#remove people that do not have an end date

data_de = data_de %>% filter(is.na(data_de$datann)==F)

#create a variable for unemployment duration
data_de$duration = data_de$datann - data_de$datins
summary(as.numeric(data_de$duration))

data_de = data_de %>% filter(data_de$duration>0)

#remove unemployment duration below the 5th percentile
dur_q = quantile(as.numeric(data_de$duration), probs=0.05, na.rm=T)

data_de = data_de %>% filter(as.numeric(data_de$duration)>=dur_q)

#keep only people that find a job
data_de = data_de %>% filter(data_de$motann %in% c("11","12","13","14","15","16","21","22","23","24","25","CA","CB","CC"))

#find a job within 2 years
data_de = data_de %>% filter(as.numeric(data_de$duration) <= 720)

data_dads = data_dads %>% filter(data_dads$idfhda %in% unique(test_de$idfhda))

write.csv(data_dads, "dads_clean.csv")
write.csv(data_de, "de_clean.csv")

############## Create final dataset ############################################

# Looping to create a dataframe which match each working timepsan with an employment timespan
# to track the employer across time 

boucle = function(t){
  y = xx%>% filter(xx$an < t) %>% filter(an == max(an))
  if (nrow(y)==0){
    past_job = NA
    past_sb_h = NA
    past_sir = NA
    past_ce = NA
    past_contrat = NA
    past_cs = NA
  } else {
    if (nrow(y)==1){
      past_job = as.Date(y$an)
      past_sb_h = y$sb_h
      past_sir = y$sir
      past_ce = y$ce
      past_contrat = y$contrat_travail
      past_cs = y$cs2
    } else {
      yy = y%>% filter(y$finremu == max(y$finremu))
      if (nrow(yy)==1){
        past_job = as.Date(yy$an)
        past_sb_h = yy$sb_h
        past_sir = yy$sir
        past_ce = yy$ce
        past_contrat = yy$contrat_travail
        past_cs = yy$cs2
      } else {
        yy = yy[1,]
        past_job = as.Date(yy$an)
        past_sb_h = yy$sb_h
        past_sir = yy$sir
        past_ce = yy$ce
        past_contrat = yy$contrat_travail
        past_cs = yy$cs2
      }
    }
    
  }
  
  y = xx %>% filter(xx$an>t) %>% filter(an == max(an))
  if (nrow(y)==0){
    next_job = NA
    next_sb_h =NA
    next_sir = NA
    next_ce = NA
    next_contrat = NA
    next_cs = NA
  } else {if (nrow(y)==1){
    next_job = as.Date(y$an)
    next_sb_h = y$sb_h
    next_sir = y$sir
    next_ce = y$ce
    next_contrat = y$contrat_travail
    next_cs = y$cs2
  } else {
    yy = y%>% filter(y$debremu == max(y$debremu))
    if(nrow(yy)==1){
      next_job = as.Date(yy$an)
      next_sb_h = yy$sb_h
      next_sir = yy$sir
      next_ce = yy$ce
      next_contrat = yy$contrat_travail
      next_cs = yy$cs2
    } else {
      yy = yy[1,]
      next_job = as.Date(yy$an)
      next_sb_h = yy$sb_h
      next_sir = yy$sir
      next_ce = yy$ce
      next_contrat = yy$contrat_travail
      next_cs = yy$cs2
    }
    
  }
  }
  
  y = x[1,] %>% select(idfhda)
  y$datins = t
  y$past_job = past_job
  y$next_job = next_job
  y$past_sb_h = past_sb_h
  y$past_sir = past_sir
  y$next_sb_h = next_sb_h
  y$next_sir = next_sir
  y$past_ce = past_ce
  y$next_ce = next_ce
  y$past_contrat = past_contrat
  y$next_contrat = next_contrat
  y$past_cs = past_cs
  y$next_cs = next_cs
  
  return(y)
}

list_id_de = unique(data_de$idfhda)
list_id = ifelse(list_id %in% unique(data_dads$idfhda)==T,list_id,NA)
list_id = list_id[!is.na(list_id)]

data_tot = as.data.frame(matrix(nrow=1, ncol=14))
colnames(data_tot) = c("idfhda","datins","past_job","next_job","past_sb_h","past_sir","next_sb_h","next_sir","past_ce","next_ce","past_contrat","next_contrat","past_cs","next_cs")

for (id in list_id){
  x = data_de[,c("idfhda","datins")] %>% filter(idfhda == id)
  x$datins = as.Date(x$datins)
  xx = data_dads[,c("idfhda","an", "debremu","finremu","sir", "sb_h", "ce", "contrat_travail", "cs2")] %>% filter(idfhda == id)
  xx$an = as.Date(paste(xx$an,"01-01",sep="-"))
  
  list_unempl = unique(x$datins)
  db = sapply(list_unempl, function(x) boucle(x), simplify = FALSE)
  db = bind_rows(db)
  db = db %>% filter(is.na(db$past_job)==F & is.na(db$next_job)==F)
  
  data_tot = rbind(data_tot, db)
}

data_tot$datins = as.Date(data_tot$datins, origin = "1970-01-01")
data_tot$past_job = as.Date(data_tot$past_job, origin = "1970-01-01")
data_tot$next_job = as.Date(data_tot$next_job, origin = "1970-01-01")
data_tot = data_tot[-1,]

############## Adding control variables to the final sample ####################

# Merging to produce descriptive statistics 
dads_merge = data_dads[,c("idfhda","an","sir","sx","age", "apen", "apet", "avr", "catjur", 
                          "ce", "comr", "comt", "depnai", "depr", "entsir", "msbr_ent", 
                          "nbheur", "nbsa_ent", "nbsa_et", "netnet", "regn", "regr", "regt", "sn","contrat_travail")] 
data_tot$an = ifelse(is.na(data_tot$past_job)==F,as.numeric(substr(data_tot$past_job,1,4)),as.numeric(substr(data_tot$next_job,1,4)))
data_tot$sir = ifelse(is.na(data_tot$past_sir)==F,data_tot$past_sir,data_tot$next_sir)

data = data_tot%>%
  left_join(dads_merge,by=c("idfhda","an","sir"))

data$datins <- as.Date(data$datins)

de_merge = data_de[, c("idfhda", "datins", "datann", "duration","rw_h","catregr", "cemploi", "diplome","mobdist", "nation_r",
                       "motins", "nenf", "nivfor", "qualif", "salmt", "salunit", "sitmat", "temps","contrat", "depcom2", "exper")]

de_merge$datins <- as.Date(de_merge$datins)

data = data %>%
  left_join(de_merge, by=c("idfhda", "datins"))

data = data[,-1]

##################### Creating new control variables ###########################


data$enf = ifelse(data$nenf > 0, 1,0)
data$past_cdi = ifelse(data$past_contrat == 1, 1, 0)
data = data %>% filter(is.na(data$past_cdi)==F)
data$contrat = ifelse(data$contrat==1, 1, 0)
data$married = ifelse(data$sitmat == "M", 1,0)
data$sx = ifelse(data$sx==1,0,1)
data = data %>% group_by(idfhda) %>% mutate(count=n()) %>% ungroup


write.csv(data, "data_final.csv")