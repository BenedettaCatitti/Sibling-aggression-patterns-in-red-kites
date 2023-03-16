# =================================================================================== #
#                                                                                     #
#                                                                                     #      
#                             R script for manuscript                                 #  
#                                                                                     #
#   "Turning tables: food availability shapes dynamic aggressive behaviour among      #
#                siblings of an asynchronously hatching species"                      #
#                                                                                     #
#           Authors: Benedetta Catitti, Urs G. Kormann, Martin U. Grüebler            #
#                                                                                     #
#    1. In the first part we quantify pecks across phases and food conditions (Fig.1) #
#    2. In the second part we calculate the tendency to interact (Fig.2)              #    
#    3. In the third part we calculate food distribution (Fig.3)                      #
#                                                                                     #
#                                                                                     #
#             For questions, email benedetta.catitti@vogelwarte.ch                    #
#                                                                                     #
#                                                                                     #
# =================================================================================== #

# Clean R's mind 

rm(list = ls())

# load libraries

library(tidyverse)
library(reshape)
library(dplyr)
library(lubridate)
library(bamlss)
library(magrittr)
library(bayestestR)
library(ggpubr)
library(foreach)
library(doParallel)
library(grid)
library(igraph)

# load dataset that will be used throughout the script -- > pecks with corresponding daily biomass delivered

peck_tot <- read.csv2("./Data/peck_tot.csv") 

#  ====================== 1. Pecks distribution ========================

# Data Exploration

# Create a table of interaction for early and late nestling period, and all 3 food conditions

Results0 <- peck_tot %>% 
  filter(!is.na(food_cat == "NA")) %>%
  mutate(N_att = 1) %>%
  group_by(phase, Date_analysis) %>% 
  summarise(agg_tot = sum(N_att)) %>%
  group_by(phase) %>%
  summarise(agg_mean = mean(agg_tot), SE = sd(agg_tot)/sqrt(dplyr::n())) # average number of pecks per day across the two phases

Results1 <- peck_tot %>% 
  filter(!is.na(food_cat == "NA")) %>%
  mutate(N_att = 1) %>%
  group_by(Date_analysis, phase, food_cat) %>%
  summarise(N_sum = sum(N_att)) %>%
  dplyr::select (N_sum, phase, food_cat) %>%
  ungroup() %>%
  group_by(phase, food_cat) %>%
  mutate(N_sum = replace_na(N_sum, 0)) %>%
  summarise(mean_N = mean(N_sum), sd = sd(N_sum), N = dplyr::n(), SE = sd/sqrt(N))


Results2 <- peck_tot %>% 
   filter(!is.na(food_cat == "NA")) %>%
   mutate(N_att = 1) %>%
  group_by(Date_analysis, direction, phase) %>%
  summarise(N_sum = sum(N_att)) %>%
  dplyr::select (N_sum, direction, phase) %>%
  ungroup() %>%
  tidyr::complete(direction, phase) %>%
  group_by(phase, Date_analysis)  %>%
  complete(direction = c("AB","BA","BC","CB","CA","AC")) %>%
  ungroup() %>% 
  drop_na(Date_analysis) %>%
  group_by(phase, direction) %>%
  mutate(N_sum = replace_na(N_sum, 0)) %>%
  summarise(mean_N = mean(N_sum), sd = sd(N_sum), N = dplyr::n(), SE = sd/sqrt(N))

Results3 <- peck_tot %>% 
  filter(!is.na(food_cat == "NA")) %>%
  mutate(N_att = 1) %>%
  group_by(Date_analysis, direction, phase, food_cat) %>%
  summarise(N_sum = sum(N_att)) %>%
  dplyr::select (N_sum, direction, phase, food_cat) %>%
  ungroup() %>%
  tidyr::complete(direction, phase, food_cat) %>%
  group_by(phase, Date_analysis, food_cat)  %>%
  complete(direction = c("AB","BA","BC","CB","CA","AC")) %>%
  ungroup() %>% 
  drop_na(Date_analysis) %>%
  group_by(phase, direction, food_cat) %>%
  mutate(N_sum = replace_na(N_sum, 0)) %>%
  summarise(mean_N = mean(N_sum), sd = sd(N_sum), N = dplyr::n(), SE = sd/sqrt(N))


#  --------------------------------------- 1. Raw peck networks (Fig. 1) ------------------------------------------

## 1. Early high food 

g1 <- graph( edges=c(1,2,2,1,2,3,3,2,1,3,3,1), n=3, directed=F) 
plot(g1)

V(g1)$label.color <- "black"
V(g1)$label <- NA
V(g1)$size <- 45
V(g1)$color <- c("#613659","#211522","#D3B1C2")
V(g1)$frame.color <- c("#613659","#211522","#D3B1C2")

# Set edge width based on weight:

table_early_h <- Results3 %>% filter(phase == "early" , food_cat == "high") %>%
  arrange(match(direction, c("AB", "BA", "BC", "CB","AC","CA")))
                                                                                         
E(g1)$width <- sqrt(c(156,7.33,1.33,0,20.7,0.667))+1 #AB BA BC CB AC CA

#change arrow size and edge color:

E(g1)$arrow.size <- .5
E(g1)$color <- c("#613659","#211522","#211522","#D3B1C2","#613659","#D3B1C2")

tiff(filename = "./Plots/net_early_h.tiff",width = 4, height = 4, units = "in", res=500,compression = "lzw")

plot(g1, edge.curved=c(0.3,-0.3,0.3,-0.3,0.3,-0.3))

dev.off()

## 2. Early medium food 

g2 <- graph(edges=c(1,2,2,1, 2,3,3,2,1,3, 3, 1), n=3, directed=F) 
plot(g2)

# The labels are currently node IDs.Setting them to NA will render no labels:

V(g2)$label <- NA
V(g2)$size <- 45
V(g2)$color <- c("#613659","#211522","#D3B1C2")
V(g2)$frame.color <- c("#613659","#211522","#D3B1C2")

# Set edge width based on weight:

table_early_m <- Results3 %>% filter(phase == "early" , food_cat == "medium") %>%
                  arrange(match(direction, c("AB", "BA", "BC", "CB","AC","CA")))

E(g2)$width <- sqrt(c(73.6,13.8,7.33,0.111,18.9,1.17))+1  #AB BA BC CB AC CA total 2753

#change arrow size and edge color:

E(g2)$arrow.size <- .5
E(g2)$color <- c("#613659","#211522","#211522","#D3B1C2","#613659","#D3B1C2")

tiff(filename = "./Plots/net_early_m.tiff",width = 4, height = 4, units = "in", res=500,compression = "lzw")

plot(g2, edge.curved=c(0.3,-0.3,0.3,-0.3,0.3,-0.3))

dev.off()

## 3. Early low food 

g3 <- graph( edges=c(1,2,2,1,2,3,3,2,1,3,3,1), n=3, directed=F) 
plot(g3)

# The labels are currently node IDs.Setting them to NA will render no labels:

V(g3)$label <- NA
V(g3)$size <- 45
V(g3)$color <- c("#613659","#211522","#D3B1C2")
V(g3)$frame.color <- c("#613659","#211522","#D3B1C2")

# Set edge width based on weight:

table_early_l <- Results3 %>% filter(phase == "early" , food_cat == "low") %>%
                  arrange(match(direction, c("AB", "BA", "BC", "CB","AC","CA")))

E(g3)$width <- sqrt(c(99.5,20.5,7.42,0,13,0.5))+1 #AB BA BC CB AC CA - 1608

#change arrow size and edge color:

E(g3)$arrow.size <- .5
E(g3)$color <- c("#613659","#211522","#211522","#D3B1C2","#613659","#D3B1C2")

tiff(filename = "./Plots/net_early_l.tiff",width = 4, height = 4, units = "in", res=500,compression = "lzw")

plot(g3, edge.curved=c(0.3,-0.3,0.3,-0.3,0.3,-0.3))

dev.off()

## 4. Late high food 

table_late_h <- Results3 %>% filter(phase == "late" , food_cat == "high") %>%
  arrange(match(direction, c("AB", "BA", "BC", "CB","AC","CA")))

E(g1)$width <- sqrt(c(6.75,29.5,27.6,0.25,5.25,2.75)) + 1  #AB BA BC CB AC CA 565 in total

#change arrow size and edge color:

tiff(filename = "./Plots/net_late_h.tiff",width = 4, height = 4, units = "in", res=500,compression = "lzw")

plot(g1, edge.curved=c(0.3,-0.3,0.3,-0.3,0.3,-0.3))

dev.off()

## 5. Late medium food 

table_late_m <- Results3 %>% filter(phase == "late" , food_cat == "medium") %>%
  arrange(match(direction, c("AB", "BA", "BC", "CB","AC","CA")))

E(g2)$width <- sqrt(c(23.2,2.38,0.875,0,4,1.25)) + 1 #AB BA BC CB AC CA 266 

tiff(filename = "./Plots/net_late_m.tiff",width = 4, height = 4, units = "in", res=500,compression = "lzw")

plot(g2, edge.curved=c(0.3,-0.3,0.3,-0.3,0.3,-0.3))

dev.off()

## 6. Late low food 

table_late_l <- Results3 %>% filter(phase == "late" , food_cat == "low") %>%
  arrange(match(direction, c("AB", "BA", "BC", "CB","AC","CA")))

E(g3)$width <- sqrt(c(51,2.5,0.5,0,0,2)) + 1 #AB BA BC CB AC CA 225

#change arrow size and edge color:

tiff(filename = "./Plots/net_late_l.tiff",width = 4, height = 4, units = "in", res=500,compression = "lzw")

plot(g3, edge.curved=c(0.3,-0.3,0.3,-0.3,0.3,-0.3))

dev.off()

# Extract numbers for the manuscript

peck_tot %>% group_by(phase) %>% select(direction) %>% table # overall peck distribution
peck_tot %>% group_by(phase, food_cat) %>% select(direction) %>% table # overall peck distribution


peck_tot %>% group_by(phase) %>%
  distinct(brood_ID, Date_analysis, .keep_all=T) %>%
  summarise(N = mean(weight_tot,na.rm=T), SE = sd(weight_tot,na.rm=T)/sqrt(dplyr::n())) # average amount of biomass per phase



#  ====================== 2. Pecking propensity (Fig. 2) =======================
  
# 1. format data before entering the loop

peck_tot$attack_don <- as.factor(peck_tot$attack_don)
peck_tot$attack_rec <- as.factor(peck_tot$attack_rec)
peck_tot$brood_ID <- ordered(as.factor(peck_tot$brood_ID))

# 1.1 load the dataset of brood-specific age differences that will be later needed in the loop

agediff <- read_csv("./Data/agediff.csv") 

# 2. Parallel loop

registerDoParallel(cores=3) # I hope your laptop has more cores than mine

loop_perm <- function(peck_tot, mylist_pos, mylist_neg) {
  
  for.tendf <- list()
  for.tendf_pos <- list()
  for.tendf_neg <- list()
  m1 <- list()
  m2 <- list()
  nd_pos <- list()
  nd_neg <- list()
  mylist_pos <- list()
  mylist_neg <- list()
  final_diff <- list()
  
  peck_totr <- peck_tot %>%
    group_by(brood_ID, Date_analysis) %>% 
    nest() %>%            
    ungroup() %>% 
    mutate(samp = map2(data, .80, sample_frac)) %>% 
    select(-data) %>%
    unnest(samp) #randomly sample 80% of the data
  
  final_freq_obs <- list()
  final_freq_random <- list()
  
  for (j in 1:length(unique(peck_tot$brood_ID))){
    
    nests <- peck_totr %>% filter(brood_ID==unique(peck_totr$brood_ID)[j]) #subset the dataset in single nests
    
    freq_matrix <- list()
    freq_random <- list()
    
    for (i in 1:length(unique(nests$Date_analysis))){
      
      nests_date <- nests %>% dplyr::filter(Date_analysis==unique(nests$Date_analysis)[i])
      freq_matrix[[i]] <- nests_date %$% table(attack_don, attack_rec)
      names(freq_matrix)[i] <- as.character(unique(nests_date$Date_analysis))
      
      nests_date_rand <- nests_date
      
      for (for.perm in 1:10000) { # PERMUTATION 
        
        lines <- seq(1,nrow(nests_date))
        a <- sample(lines, 1)
        b <- c("A","B","C")
        
        nests_date_rand[a, "attack_rec"] <- sample(b[b!= nests_date$attack_don[a]],1) # randomise the recipient
        
      }  # randomise the recipient
      
      freq_random[[i]] <- nests_date_rand %$% table(attack_don, attack_rec) 
      names(freq_random)[i] <- as.character(unique(nests_date_rand$Date_analysis))
      
    } 
    
    final_freq_obs[[j]] <- freq_matrix
    names(final_freq_obs)[j] <- unique(nests$brood_ID)
    
    final_freq_random[[j]] <- freq_random
    names(final_freq_random)[j] <- unique(nests$brood_ID)
    
    f <- function (...) Reduce("-", list(...))
    final_diff[[j]] <- mapply(f,final_freq_obs[[j]],final_freq_random[[j]], SIMPLIFY=FALSE) # observed - random
    names(final_diff)[j] <- unique(as.character(nests$brood_ID))
  } # calculate random frequency and compute the difference between observed and randomised
  
  for.tendf[[t]] <- melt(final_diff) %>% drop_na() %>%
    mutate(direction=paste(attack_don,attack_rec,sep=""))%>% 
    bind_rows()%>% dplyr::rename(brood_ID =L1, Date_analysis=L2)%>%
    mutate(Date_analysis= as.numeric(Date_analysis)) %>%
    filter(!direction=="AA",! direction=="BB",!direction=="CC") %>%
    left_join(peck_tot[,c("weight_tot","brood_ID","Date_analysis","phase")], by=c("brood_ID","Date_analysis")) %>%
    left_join(agediff[,c("brood_ID", "direction", "age_diff")], by=c("brood_ID", "direction")) %>%
    distinct(brood_ID, Date_analysis, direction,.keep_all=T) %>%
    mutate(weight_tot = as.numeric(weight_tot))
  
  for.tendf_pos[[t]] <- for.tendf[[t]] %>%  filter(age_diff > 0 | age_diff == 0 & direction == "BA")  # keep only positive values - positive means younger attacking older
  for.tendf_neg[[t]] <- for.tendf[[t]] %>%  filter(age_diff < 0 | age_diff == 0 & direction == "AB")  # keep only negative values - negative means older attacking younger
  
  m1[[t]] <- bamlss(value ~ te(age_diff, weight_tot, by=phase, k=5) + s(brood_ID,bs="re"), data=for.tendf_pos[[t]])
  m2[[t]] <- bamlss(value ~ te(age_diff, weight_tot, by=phase, k=5) + s(brood_ID,bs="re"), data=for.tendf_neg[[t]])
  
  # extract predicted values for positive age difference
  
  nd_pos[[t]] <- expand.grid(age_diff=seq(min(for.tendf_pos[[t]]$age_diff,na.rm=T),max(for.tendf_pos[[t]]$age_diff,na.rm=T),l=100), weight_tot=seq(min(for.tendf_pos[[t]]$weight_tot,na.rm=T), max(for.tendf_pos[[t]]$weight_tot,na.rm = T), l=100), brood_ID=c("7","207"), phase=c("early","late")) %>% 
    filter(brood_ID=="7")
  
  nd_pos[[t]]$pred <- predict(m1[[t]], newdata = nd_pos[[t]], type = "parameter", FUN = c95)$mu$Mean
  
  mylist_pos[[t]] <- nd_pos[[t]] %>% 
    group_by(phase) %>% 
    pivot_wider(names_from=age_diff, values_from=pred)  %>% 
    dplyr::select(!c(brood_ID)) %>% 
    mutate(category="youngtold")
  
  # extract predicted values for negative age difference
  
  nd_neg[[t]] <- expand.grid(age_diff=seq(min(for.tendf_neg[[t]]$age_diff,na.rm=T),max(for.tendf_neg[[t]]$age_diff,na.rm=T),l=100), weight_tot=seq(min(for.tendf_neg[[t]]$weight_tot,na.rm=T), max(for.tendf_neg[[t]]$weight_tot,na.rm = T), l=100), brood_ID=c("7","207"), phase=c("early","late")) %>% 
    filter(brood_ID=="7")
  
  nd_neg[[t]]$pred <- predict(m2[[t]], newdata = nd_neg[[t]], type = "parameter", FUN = c95)$mu$Mean
  
  mylist_neg[[t]] <- nd_neg[[t]] %>% 
    group_by(phase) %>% 
    pivot_wider(names_from=age_diff, values_from=pred) %>% 
    dplyr::select(!c(brood_ID)) %>% 
    mutate(category="oldtoyoung")
  
  return(c(mylist_pos, mylist_neg))
  
  
} #end of iterations

t <- 50

mylist_neg <- list()
mylist_pos <- list()

parallel_df <- foreach(z=1:t, .packages = c("dplyr","magrittr","bamlss","lubridate","tidyverse","reshape")) %dopar%  loop_perm(peck_tot, mylist_pos, mylist_neg) # this will take a while!

# 3. extract predicted values at the minimum, mean and max food provisioning 

flat <- flatten_dfr(parallel_df) %>% 
  filter(weight_tot == min(weight_tot) | abs(weight_tot - mean(weight_tot)) == min(abs(weight_tot - mean(weight_tot)))| weight_tot == max(weight_tot)) %>% 
  filter(weight_tot %in% c(unique(weight_tot)[c(1,2,4)]))

# 4. Calculate the mean predicted values across all bootstrap repetitions

para_mean <- flat %>%
  group_by(category, phase, weight_tot) %>%
  summarise_if(is.numeric, mean, na.rm=T)  %>% 
  pivot_longer(!c(category, weight_tot, phase), names_to=c("age_diff"), values_to="mean_pred")%>%
  drop_na(mean_pred)

# 5. Calculate the standard deviation of predicted values across all bootstrap repetitions

para_sd <- flat %>%
  group_by(category, phase, weight_tot) %>%
  summarise_if(is.numeric, sd, na.rm=T)  %>% 
  pivot_longer(!c(category, weight_tot, phase), names_to=c("age_diff"), values_to="sd_pred")%>%
  drop_na(sd_pred) %>%
  left_join(para_mean)

# 6. Extract 95 % CI

para_tot <- para_sd %>%
  mutate(se_pred = sd_pred / sqrt(t),
         lower.ci = mean_pred - qt(1 - (0.05 / 2), t - 1) * se_pred,
         upper.ci = mean_pred + qt(1 - (0.05 / 2), t - 1) * se_pred) %>%
  mutate(age_diff=as.numeric(age_diff))

# 7. Plot

tog <- ggplot(para_tot, aes(x=age_diff, y=mean_pred, group=as.factor(weight_tot))) + 
  facet_wrap(phase~category, scales="free") +
  geom_line(size=0.5, aes(y=0), linetype="dashed", col="darkred")+
  geom_ribbon(aes(ymin=lower.ci, ymax=upper.ci, fill=as.factor(weight_tot)),alpha=0.7) +
  geom_line(size=.6, aes(col=as.factor(weight_tot))) + 
  scale_fill_manual(values=c("#e8c599","#bc6022","#b72818"),name="food delivered") +
  scale_color_manual(values=c("#e8c599","#bc6022","#b72818"),name="food delivered") +
  theme_bw()+
  theme(strip.text.x =element_blank(),
        axis.line= element_line(size=0.05),
        axis.title = element_text(size = 14),
        plot.title = element_blank(),
        strip.background = element_blank(),
        legend.position = "none",
  ) +
  labs(x="",y="")

tog

ylims = list( c(-50, 50), c(-3, 3), c(-4,4), c(-2,2))

scale_inidividual_facet_y_axes = function(plot, ylims) {
  init_scales_orig = plot$facet$init_scales
  
  init_scales_new = function(...) {
    r = init_scales_orig(...)
    # Extract the Y Scale Limits
    y = r$y
    # If this is not the y axis, then return the original values
    if(is.null(y)) return(r)
    # If these are the y axis limits, then we iterate over them, replacing them as specified by our ylims parameter
    for (i in seq(1, length(y))) {
      ylim = ylims[[i]]
      if(!is.null(ylim)) {
        y[[i]]$limits = ylim
      }
    }
    # Now we reattach the modified Y axis limit list to the original return object
    r$y = y
    return(r)
  }
  
  plot$facet$init_scales = init_scales_new
  
  return(plot)
} # graphical function to adjust the figure so that I can set the ylim for every facet grid

tog <- scale_inidividual_facet_y_axes(tog, ylims = ylims)

tog

tiff(filename = "./Plots/Fig2.tiff",width = 7, height = 7, units = "in", res=600,compression = "lzw")

tog

annotate_figure(tog, bottom = textGrob("Expected rank difference", gp = gpar(cex = 1)))

dev.off()

# 7.1 Export only the legend -- I'll put the plots together with some annotations in an external software

testplot <- ggplot(para_tot, aes(x=age_diff, y=mean_pred, group=as.factor(weight_tot))) + 
  facet_wrap(phase~category, scales="free") +
  geom_line(size=0.5, aes(y=0), linetype="dashed", col="darkred")+
  geom_ribbon(aes(ymin=lower.ci, ymax=upper.ci, fill=as.factor(weight_tot)),alpha=0.7)+
  geom_line(size=.6, aes(col=as.factor(weight_tot))) + 
  scale_fill_manual(values=c("#e8c599","#bc6022","#b72818"),name="food conditions", labels=c("poor","average","favourable")) +
  scale_color_manual(values=c("#e8c599","#bc6022","#b72818"),name="food delivered", labels=c("poor","average","favourable")) +
  theme(legend.position="bottom")

legend <- get_legend(testplot)                    

# Create new plot window
grid.newpage()                              

# Draw Only legend 
grid.draw(legend) 

tiff(filename = "./Plots/legend_Fig2.tiff",width = 4, height = 1, units = "in", res=250,compression = "lzw")

grid.draw(legend)

dev.off()


# ======================= 3. Food distribution  (Fig. 3) ======================

# load specific dataset about feeding bouts received

food_model <- read_csv("./Data/food_df.csv")

str(food_model)

# multinomial model

f <- list(
  eater ~ s(weight_tot, k=4)  + s(Age_A, k=4) + s(brood_ID, bs="re"),
  ~  s(weight_tot, k=4)  + s(Age_A, k=4) + s(brood_ID, bs="re")) 

multmodel_biomasst_5 <- bamlss(f, data=food_model, family="multinomial", reference="A", n.iter = 10000, burnin = 2000, thin = 10)

summary(multmodel_biomasst_5)
plot(multmodel_biomasst_5)

# Predict and plot 

nd <- expand.grid("weight_tot"=seq(min(food_model$weight_tot,na.rm=T),max(food_model$weight_tot,na.rm=T),l=100),
                  "brood_ID"= unique(food_model$brood_ID),
                  "Age_A" = mean(food_model$Age_A))

p <- predict(multmodel_biomasst_5, newdata = nd, type = "parameter")

probs <- list()

for(j in names(p))
  probs[[j]] <- p[[j]] / (1 + rowSums(do.call("cbind", p)))
probs <- as.data.frame(probs)
probs[["A"]] <- 1 - rowSums(probs)

nd <- cbind(nd, probs)

newdA <- nd %>% group_by(weight_tot) %>% dplyr::summarise(Aav=mean(A)) %>% mutate(chick="A") # average across all broods
newdB <- nd %>% group_by(weight_tot) %>% dplyr::summarise(Aav=mean(B))%>% mutate(chick="B")
newdC <- nd %>% group_by(weight_tot) %>% dplyr::summarise(Aav=mean(C)) %>% mutate(chick="C")

newd <- rbind(newdA, newdB, newdC)

plot_mass <- ggplot(newd, aes(x=weight_tot, y=Aav, fill=chick)) +
  geom_area(alpha=0.8 , size=.5, colour="white") +
  scale_fill_manual(name = "ID",values=c("#613659","#211522","#D3B1C2")) +
  theme(  panel.background = element_blank(),
          panel.border     = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background  = element_blank(),
          axis.title = element_text(size = 14),
          plot.title = element_blank(),
          axis.line= element_line(size=0.4),
          strip.background = element_blank(),
          strip.text.x = element_blank()) + 
  xlab("Biomass delivered/day (g)") + 
  ylab("Predicted portion of biomass")




tiff(filename = "./Plots/biomass_distribution_single.tiff",width = 4.5, height = 4, units = "in", res=500,compression = "lzw")

plot_mass

dev.off()


