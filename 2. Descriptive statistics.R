################################################################################
#                                                                              #
#                 Applied Labor Economics - Gender reservation wage gap        #
#                                                                              #
#                             Audin Roger & Eva Youinou                        #                                                            #
################################################################################

# Input : our sample from the FH-DADS database
# Output : descriptive statistics 

library(dplyr)
library(ggplot2)
library(stargazer)
library(tidyr)
library(cowplot)
library(plm)

data <- read.csv("data_final.csv")

############## Descriptive statistics ##########################################

# Number of unique observations by gender in the panel
data_sx = data %>%
  group_by(sx) %>%
  summarize(unique_obs=n_distinct(idfhda))

# Mean age
data_age = data %>%
  group_by(sx)%>%
  summarize(mean_age = mean(age))

# Number of children by gender 

data_enf = data %>%
  select(idfhda, nenf, sx)%>%
  mutate(enf = ifelse(data$nenf>0,1,0))%>%
  distinct(idfhda, sx, enf, nenf)%>%
  group_by(sx)%>%
  summarise(mean_child = mean(nenf),
            nb_enf = sum(enf),
            tot = n_distinct(idfhda),
            enf_percent = nb_enf/tot*100)

# Marital situation
data_mar = data %>%
  mutate(mar = ifelse(data$sitmat=="M",1,0))%>%
  group_by(sx)%>%
  summarize(mar = sum(mar),
            tot = n_distinct(idfhda),
            mar_percent = mar/tot*100)

# Average salary over the period, by gender
data_sal = data %>%
  group_by(sx)%>%
  summarize(mean_raw_salary = mean(past_sb_h, na.rm=T))

data_rw = data%>%
  group_by(sx)%>%
  summarise(mean_rw = mean(rw_h, na.rm=T))

# Average hourly salary by year, by gender 
data_sal_year = data %>%
  group_by(sx, an)%>%
  summarize(mean_raw_salary_yearly = mean(past_sb_h, na.rm=T)) 

plot_sal = ggplot(data = data_sal_year, aes(x = an, y = mean_raw_salary_yearly, group = as.factor(sx), color = as.factor(sx)))+
  geom_line()+
  labs(x="", y="Hourly salary")+
  ggtitle("Hourly salary per gender")+
  scale_color_discrete(name = "Gender", labels = c("Female","Male"))+
  theme_classic()
ggsave("plot_sal.jpg", plot_sal)

# Average hours worked
data_hours = data %>%
  group_by(sx) %>%
  summarize(mean_hours = mean(nbheur))

# Share of graduate people by gender
data_grad <- unique(data[c("idfhda", "diplome", "sx")])
# Observations can switch between diploma status  between periods => to discuss
data_grad <- data.frame(data_grad)
data_grad <- aggregate(idfhda ~ sx + diplome, data_grad, length)

data_grad = data %>%
  distinct(idfhda, diplome, sx)%>%
  mutate(diplome = ifelse(diplome=="D",1,0))%>%
  group_by(sx)%>%
  summarise(nb_grad = sum(diplome),
            tot = n_distinct(idfhda),
            grad_percent = nb_grad/tot*100)

# Table with all type of education  
data_educ <- unique(data[c("idfhda", "nivfor", "sx")])
data_educ <- data_educ %>%
  mutate(nivfor = case_when(
    nivfor == " " ~ "NR",
    nivfor == "0" ~ "Inconnu",
    nivfor == "AFS" ~ "Aucune Formation",
    nivfor == "C12" ~ "2nde/1ère G",
    nivfor == "C3A" ~ "BEPC",
    nivfor == "CP4" ~ "Certificat Etude",
    nivfor == "NV1" ~ "> Bac+5",
    nivfor == "NV2" ~ "Bac+3,Bac+4",
    nivfor == "NV3" ~ "Bac+2",
    nivfor == "NV4" ~ "Bac",
    nivfor == "NV5" ~ "CAP/BEP",
  ))

list_grad = unique(data_educ$nivfor)
list_grad = list_grad[!is.na(list_grad)]
for (i in  list_grad){
  data_educ[,i] = ifelse(data_educ$nivfor==i,1,0)
  
}

data_educ = data_educ %>%
  filter(is.na(data_educ$nivfor)==F)%>%
  group_by(sx)%>%
  summarise(`Bac+2` = sum(`Bac+2`),
            `CAP/BEP` = sum(`CAP/BEP`),
            `2nde/1ère G` = sum(`2nde/1ère G`),
            `> Bac+5` = sum(`> Bac+5`),
            Bac = sum(Bac),
            `Bac+3,Bac+4` = sum(`Bac+3,Bac+4`),
            BEPC = sum(BEPC),
            `Certificat Etude` = sum(`Certificat Etude`),
            `Aucune Formation` = sum(`Aucune Formation`),
            tot = n_distinct(idfhda),
            `Bac+2` = `Bac+2`/tot*100,
            `CAP/BEP` = `CAP/BEP`/tot*100,
            `2nde/1ère G` = `2nde/1ère G`/tot*100,
            `> Bac+5` = `> Bac+5`/tot*100,
            Bac = Bac/tot*100,
            `Bac+3,Bac+4` = `Bac+3,Bac+4`/tot*100,
            BEPC = BEPC/tot*100,
            `Certificat Etude` = `Certificat Etude`/tot*100,
            `Aucune Formation` = `Aucune Formation`/tot*100
  )


# Share of part-time job/full-time job
data_job <- unique(data[c("idfhda", "ce", "sx")])
# Observations can switch between part time and full time status  between periods => to discuss

data_job = data %>%
  distinct(idfhda, ce, sx)%>%
  mutate(ce = ifelse(ce=="C",1,0))%>%
  group_by(sx)%>%
  summarise(full_time = sum(ce),
            tot = n_distinct(idfhda),
            full_percent = full_time/tot*100)

#Type of contract

data_contrat = data %>%
  mutate(cdi = ifelse(data$contrat_travail==1,1,0))%>%
  group_by(sx)%>%
  summarize(cdi = sum(cdi, na.rm=T),
            tot = n_distinct(idfhda),
            cdi_percent = cdi/tot*100)

#Share of unmployed workers looking for a full-time job

data_contrat_u = data %>%
  mutate(cdi_u = ifelse(data$contrat==1,1,0))%>%
  group_by(sx)%>%
  summarize(cdi_u = sum(cdi_u, na.rm=T),
            tot = n(),
            cdi_u_percent = cdi_u/tot*100)

#Mean duration of unemployment

data_duration = data %>%
  group_by(sx)%>%
  summarise(duration = mean(duration))

#Mean maximum distance to job

data_mobility = data %>%
  group_by(sx)%>%
  summarise(mobility = mean(mobdist))

#Mean experience in the previous job in the same ROME

data_exp = data %>%
  group_by(sx)%>%
  summarise(exp = mean(exper))

#Descriptive statistics
stat_descr = cbind(data_sx, data_age, data_sal, data_rw, data_grad,data_educ,data_enf, data_mar, data_job, data_hours, data_contrat, data_contrat_u, data_duration, data_exp, data_mobility)
stat_descr = as.data.frame(t(stat_descr))
colnames(stat_descr) = c("Male","Female")
stat_descr=stat_descr[-1,]

stat_descr = stat_descr %>% filter(grepl("tot", rownames(stat_descr))==F)
stat_descr = stat_descr %>% filter(grepl("sx", rownames(stat_descr))==F)

rm(data_sx, data_sal, data_rw, data_educ, data_grad, data_job, data_contrat, 
   data_hours, data_mar, data_enf, data_age, data_sal_year, data_hours_year, 
   plot_hours, plot_sal, data_contrat_u, data_duration, data_mobility, data_exp)

write.csv(stat_descr, "stat_descr.csv")

# Wage density by gender 

plot_densities = ggplot(data, aes(x=past_sb_h, group=as.factor(sx), fill=as.factor(sx)))+
  geom_density(position = "identity", alpha = 0.4)+
  theme_classic()+
  geom_vline(xintercept = median(data$past_sb_h))+
  scale_fill_discrete(labels=c("Male","Female"), name = "Gender")+
  labs(x="Past hourly salary", y="", title = "Past hourly salary by gender")
ggsave("plot_densities.jpg", plot_densities)
rm(plot_densities)

# Wage gender gap by decile

data_f = data %>% filter(data$sx==1)
q_f = quantile(data_f$past_sb_h, probs = seq(0,1,0.1), na.rm=T)
q_rw_f = quantile(data_f$rw_h, probs = seq(0,1,0.1), na.rm = T)

data_h = data %>% filter(data$sx==0)
q_h = quantile(data_h$past_sb_h, probs = seq(0,1,0.1), na.rm=T)
q_rw_h = quantile(data_h$rw_h, probs = seq(0,1,0.1), na.rm = T)

q = q_h-q_f
q_rw = q_rw_h-q_rw_f

q = as.data.frame(q)
q$quantiles = seq(0:10)-1

plot_q = ggplot(q[-11,], aes(x=as.factor(quantiles), y=q))+
  geom_point()+
  labs(x="Deciles", y = "Gender wage gap")+
  theme_classic()

q_rw = as.data.frame(q_rw)
q_rw$quantiles = seq(0:10)-1

plot_q_rw = ggplot(q_rw[-11,], aes(x=as.factor(quantiles),y=q_rw) )+
  geom_point()+
  labs(x="Deciles", y = "Gender reservation wage gap")+
  theme_classic()

plot_quantiles = plot_grid(plot_q, plot_q_rw, ncol=2, nrow = 1, labels = "Gender gap across the distribution by quantile", vjust = 1, scale = c(0.9,0.9))
ggsave("plot_quantiles.jpg",plot_quantiles)
rm(plot_q, plot_q_rw)

# Reservation wage on past wage ratio

plot_ratio = ggplot(data, aes(x=rw_h/past_sb_h, group=as.factor(sx), fill = as.factor(sx)))+
  geom_histogram(bins = 80, alpha = 0.6, color = "grey", position = "identity")+
  geom_vline(xintercept = mean(data$rw_h/data$past_sb_h))+
  labs(x="Reservation wage on pre-unemployment wage ratio", y="", title = "Distribution of reservation wage")+
  scale_fill_discrete(labels=c("Male","Female"), name = "Gender")+
  theme_classic()
ggsave("plot_ratio.jpg",plot_ratio)