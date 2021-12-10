###################################################################################################################
##############################○○○○                                               ○○○○##############################
##################○○○○                                                                       ○○○○##################
###########○○○○                    Gippet JMW, Sherpa Z & Bertelsmeier C - 2022                     ○○○○###########
###########○○○○           Social media data predict the emergent global pet trade in ants           ○○○○###########
##################○○○○                                                                       ○○○○##################
##############################○○○○                                               ○○○○##############################
###################################################################################################################

# Glossary of abbreviations:
# IS = Internet sellers (online ant sellers)
# IG = Instagram users


# R version 4.0.2 (2020-06-22)

# load packages:
library(extrafont)
library(ape)
library(ggplot2)
library(ggtree)
library(treeio)
library(forcats)
library(glmmTMB)
library(DHARMa)
library(performance)
library(ggeffects)
library(viridis)
library(eulerr)
library(dplyr)
library(ggthemes)
library(sjPlot)
library(scales)
library(tmap)
library(sf)




## some options
# font_import()
# loadfonts(device = "win")
# options(max.print=1000000)
# fonttable()
# 
# options(scipen=999)

# colors
fill_cols  <- alpha(c("#b2b2b2ff", "#c02e1dff", "#6eb1f4ff"), alpha= 1)



###############load datasets##################
#################################

# 

# List of all ant species with information on trade (number of sellers and price for 2017 and 2020 surveys)
GSP_ants <- read.table("AntTrade_2017_2020.txt", h=T)
GSP_ants$InvasivenessStatus <- factor(GSP_ants$InvasivenessStatus, levels=c("Native","Alien","Invasive"))
head(GSP_ants)
dim(GSP_ants)
GSP_ants[GSP_ants$species=="Lasius_niger",]


# Chronogram
Phylotree_ants <- read.tree("Phylotree_ants_2020.nwk") # from timetree.org, accessed ? 2020
Phylotree_ants

# Data Instagram
Instagram_ants <- read.table("IGstats_antspecies_2020_v3.txt", sep="\t", row.names=1, h=T)
head(Instagram_ants)
dim(Instagram_ants)
# Instagram_ants$status <- NA
# for (i in 1: dim(Instagram_ants)[1]){
#   Instagram_ants$status[i] <- as.character(GSP_ants$InvasivenessStatus[which(GSP_ants$species==row.names(Instagram_ants)[i])])
# }
Instagram_ants$status <- as.factor(Instagram_ants$status)
Instagram_ants$status = factor(Instagram_ants$status,levels(Instagram_ants$status)[c(2,1)])

#################################
#################################






###################################################################################################################
######################################   >>>  PART 1  --  Instagram <-> Trade  <<<   ######################################
###################################################################################################################


#################################
####   Sub-families stats    ####
#################################

GSP_ants_v2 <- GSP_ants
## >> building a contingency table of subfamilies in global pool, traded ants and insta ants << ##
Subfamilies <- data.frame(sort(table(GSP_ants_v2$sub.family), decreasing = T))
colnames(Subfamilies) <- c("sub.family", "nb.species")
# traded species
Subfamilies$nb.traded.species = 0
traded.subfam <- table(GSP_ants_v2$sub.family[GSP_ants_v2$Traded2020==1])
for (sf in names(traded.subfam)){
  Subfamilies$nb.traded.species[Subfamilies$sub.family==sf] <- traded.subfam[sf]}
# instagram species
Subfamilies$nb.instagram.species = 0
instagram.subfam <- table(Instagram_ants$sub.family)
for (sf in names(instagram.subfam)){
  Subfamilies$nb.instagram.species[Subfamilies$sub.family==sf] <- instagram.subfam[sf]}

Subfamilies <- Subfamilies[order(Subfamilies$nb.species, Subfamilies$nb.traded.species, Subfamilies$nb.instagram.species, decreasing=T ) ,]

# Axis treated as discrete variable
tabSF <- data.frame(SF=rep(Subfamilies$sub.family, each=3),
                    group=rep(c("Total", "Traded", "Instagram"),18))
tabSF$nb.species = 0
tabSF$nb.species[tabSF$group=="Total"] <- Subfamilies$nb.species
tabSF$nb.species[tabSF$group=="Traded"] <- Subfamilies$nb.traded.species
tabSF$nb.species[tabSF$group=="Instagram"] <- Subfamilies$nb.instagram.species

tabSF$prop.species = 0
tabSF$prop.species[tabSF$group=="Total"] <- Subfamilies$nb.species/sum(Subfamilies$nb.species)
tabSF$prop.species[tabSF$group=="Traded"] <- Subfamilies$nb.traded.species/sum(Subfamilies$nb.traded.species)
tabSF$prop.species[tabSF$group=="Instagram"] <- Subfamilies$nb.instagram.species/sum(Subfamilies$nb.instagram.species)
# tabSF <- tabSF[c(1:24,35,36),]

tabSF$group <- as.factor(tabSF$group)
tabSF$group = factor(tabSF$group,levels(tabSF$group)[c(2,3,1)])
levels(tabSF$group)

levels(tabSF$SF)
tabSF$SF <- factor(tabSF$SF, levels = as.character(unique(tabSF$SF)))
tabSF <- tabSF[1:30,] # only the 10 main subfamilies

fig1b <-  ggplot(data=tabSF, aes(x=fct_rev(fct_infreq(SF)), y=prop.species, fill=fct_rev(fct_infreq(group)))) +
  geom_bar(stat="identity", color="black", position=position_dodge(), size=0.5, width=0.85) +
  theme_classic() +
  coord_flip() +
  theme(axis.title=element_text(size=12,face="bold"), 
        axis.text.x = element_text( face="bold", size=10), 
        axis.text.y = element_text( face="bold", size=10),
        legend.position=c(0.75,0.2),
        plot.margin=unit(c(1,1,1,1),"cm"),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 20))) +
  labs(title = "", 
       subtitle = NULL, 
       caption = NULL, 
       tag = "Figure 1", 
       y = "Proportion of species",
       x = "Sub-families",
       colour = "Gears") +
  scale_fill_manual(name="Group", 
                    labels=rev(c("All ant species (N=15,377)","Traded species (N=520)",  "Instagram species (N=714)")), 
                    values=rev(c('#d0d0d0ff','#7b7bc5ff', '#dfd05bff')))

fig1b

# pdf("figure1b_v1.pdf", height = 10, width = 7)
# print(fig1b)
# dev.off()



# proportion test
groupe1= 'Myrmicinae'
groupe2= 'Formicinae'
groupe3= 'Ponerinae'
groupe4= 'Dolichoderinae'
groupe5= 'Dorylinae'
groupe6= 'Ectatomminae'
groupe7= 'Pseudomyrmecinae'
groupe8= 'Proceratiinae'
groupe9= 'Amblyoponinae'
groupe10= 'Myrmeciinae'

groupe = groupe1
instagram_group = c(tabSF$nb.species[tabSF$SF==groupe & tabSF$group=="Instagram"], sum(tabSF$nb.species[tabSF$group=="Instagram"]))
traded_group = c(tabSF$nb.species[tabSF$SF==groupe & tabSF$group=="Traded"], sum(tabSF$nb.species[tabSF$group=="Traded"]))
total = c(tabSF$nb.species[tabSF$SF==groupe & tabSF$group=="Total"]/sum(tabSF$nb.species[tabSF$group=="Total"]), 1-tabSF$nb.species[tabSF$SF==groupe & tabSF$group=="Total"]/sum(tabSF$nb.species[tabSF$group=="Total"]) )

prop_trade <- chisq.test(traded_group, p = total) # test if same proportion than in global species pool
prop_instagram <- chisq.test(instagram_group, p = total) # test if same proportion than in global species pool

prop_trade$p.value*20
prop_instagram$p.value*20




#################################
####      Genera stats       ####
#################################

head(GSP_ants_v2)

dfIS <- GSP_ants_v2[GSP_ants_v2$Traded2020==1, c(3,2,9,10,4)]
row.names(dfIS) <- GSP_ants_v2$species[GSP_ants_v2$Traded2020==1]
head(dfIS)
dim(dfIS)

head(Instagram_ants)
dfIG <- Instagram_ants[, c(1,2,4,7,9,10)]
dfIG$PostsByUser <- dfIG$byPost/dfIG$byUser
head(dfIG)
dim(dfIG)




# building a circular phylogenetic tree and coloring branches based on commercial, sm variables
# https://yulab-smu.github.io/treedata-book/chapter4.html
# https://bioconductor.statistik.tu-dortmund.de/packages/3.1/bioc/vignettes/ggtree/inst/doc/ggtree.html
# https://yulab-smu.github.io/treedata-book/faq.html


Phylotree_ants
tree_ants <- Phylotree_ants

toRM <- c()
for (genus in tree_ants$tip.label){ 
  if(genus %in% GSP_ants_v2$genus){
  } else {toRM <-c(toRM, genus)}
} 

# Deleting genus not in dataset (Pyramica, Oligomyrmex, Phasmomyrmex...)
for (i in 1:length(Phylotree_ants$tip.label) ) {
  print(paste("doing",i))
  tree_ants <- drop.tip(tree_ants, toRM,trim.internal=TRUE) 
}

genera <- tree_ants$tip.label
allAntsGenera <- table(GSP_ants_v2$genus)

# put all in a df
dfAntsParam <- as.data.frame( matrix(data=NA, ncol=19, nrow=length(genera)))
colnames(dfAntsParam) = c( 'Nbspecies_IS', 'Nbspecies_IG', 'Propspecies_IS', 'Propspecies_IG', 
                           'SumNbS', 'MeanNbS', 'SeNbS', 
                           'MeanPrice', 'SePrice', 
                           'SumIGusers', 'MeanIGusers',
                           'SumNbPosts', 'MeanNbPosts',
                           'SumIGlikes', 'MedianIGlikes',
                           'SumIGcomments', 'MedianIGcomments',
                           'sub.family', 'NbAllSpecies')
row.names(dfAntsParam) = genera

head(dfAntsParam)
dim(GSP_ants_v2)
sort(GSP_ants_v2$genus)


for (genus in genera){
  print(genus)
  dfAntsParam[genus, 'sub.family'] = unique(GSP_ants_v2$sub.family[GSP_ants_v2$genus==genus])
  dfAntsParam[genus, 'NbAllSpecies'] = dim(GSP_ants[GSP_ants_v2$genus==genus,])[1]
  
  dfAntsParam[genus, 'Nbspecies_IS'] = 0
  dfAntsParam[genus, 'Nbspecies_IG'] = 0
  
  dfAntsParam[genus, 'Propspecies_IS'] = 0
  dfAntsParam[genus, 'Propspecies_IG'] = 0
  
  if (genus %in% dfIS$genus){
    df1 <- dfIS[dfIS$genus==genus,]
    
    dfAntsParam[genus, 'Nbspecies_IS'] = length(unique(row.names(df1)))
    dfAntsParam[genus, 'Propspecies_IS'] = length(unique(row.names(df1)))/allAntsGenera[names(allAntsGenera)==genus]
    
    dfAntsParam[genus, 'SumNbS'] = sum(df1$NbSellers, na.rm=T)
    dfAntsParam[genus, 'MeanNbS'] = mean(df1$NbSellers, na.rm=T)
    dfAntsParam[genus, 'SeNbS'] = sqrt(var(df1$NbSellers, na.rm=T)/length(which(!is.na(df1$NbSellers))))
    
    dfAntsParam[genus, 'MeanPrice'] = mean(df1$MeanPrice, na.rm=T)
    dfAntsParam[genus, 'SePrice'] = sqrt(var(df1$MeanPrice, na.rm=T)/length(which(!is.na(df1$MeanPrice))))
  }
  if (genus %in% dfIG$genus){
    df2 <- dfIG[dfIG$genus==genus,]
    dfAntsParam[genus, 'Nbspecies_IG'] = length(unique(row.names(df2)))
    dfAntsParam[genus, 'Propspecies_IG'] = length(unique(row.names(df2)))/allAntsGenera[names(allAntsGenera)==genus]
    
    dfAntsParam[genus, 'SumIGusers'] = sum(df2$byUser, na.rm=T)
    dfAntsParam[genus, 'MeanIGusers'] = mean(df2$byUser, na.rm=T)
    
    dfAntsParam[genus, 'SumNbPosts'] = sum(df2$byPost, na.rm=T)
    dfAntsParam[genus, 'MeanNbPosts'] = mean(df2$byPost, na.rm=T)
    #dfAntsParam[genus, 'SeIGusers'] = sqrt(var(df2$byUser, na.rm=T)/length(df2$byUser))
    
    dfAntsParam[genus, 'SumIGlikes'] = sum(df2$medianLikes, na.rm=T)
    dfAntsParam[genus, 'MedianIGlikes'] = mean(df2$medianLikes, na.rm=T)
    # if(is.na(var(df2$medianLikes))){
    # }else{dfAntsParam[genus, 'SeIGlikes'] = sqrt(var(df2$medianLikes,na.rm=T)/length(which(!is.na(df2$medianLikes))))}
    
    dfAntsParam[genus, 'SumIGcomments'] = sum(df2$medianComments, na.rm=T)
    dfAntsParam[genus, 'MedianIGcomments'] = mean(df2$medianComments, na.rm=T)
    # if(is.na(var(df2$medianComments))){
    # }else{dfAntsParam[genus, 'SeIGcomments'] = sqrt(var(df2$medianComments,na.rm=T)/length(which(!is.na(df2$medianComments))))}
    # dfAntsParam[genus, 'MedianIGlikes'] = mean(log(df2$medianLikes+1), na.rm=T)
    # dfAntsParam[genus, 'SeIGlikes'] = sqrt(var(log(df2$medianLikes+1), na.rm=T)/length(df2$medianLikes))
  }
}


dfAntsParam$SumNbS[is.na(dfAntsParam$SumNbS)] <- 0
dfAntsParam$MeanNbS[is.na(dfAntsParam$MeanNbS)] <- 0
dfAntsParam$SumIGusers[is.na(dfAntsParam$SumIGusers)] <- 0
dfAntsParam$MeanIGusers[is.na(dfAntsParam$MeanIGusers)] <- 0
dfAntsParam$SumNbPosts[is.na(dfAntsParam$SumNbPosts)] <- 0
dfAntsParam$MeanNbPosts[is.na(dfAntsParam$MeanNbPosts)] <- 0

head(dfAntsParam)
dim(dfAntsParam)


### Representing commercial and popularity indices on the ant genus tree ###
#dfAntsParam_v2 <- dfAntsParam[,c(1,2,3,4,5,6,8,9,10,11,13,14,15,16)]
dfAntsParam_v2 <- dfAntsParam
head(dfAntsParam_v2)
dfAntsParam_v2$Propspecies_IS <- car::logit(dfAntsParam_v2$Propspecies_IS, adjust=0.001)
dfAntsParam_v2$Propspecies_IG <- car::logit(dfAntsParam_v2$Propspecies_IG, adjust=0.001)
dim(dfAntsParam_v2)

##
dfAntsParam_v3 <- dfAntsParam_v2[(dfAntsParam_v2$Nbspecies_IS!=0 & dfAntsParam_v2$Propspecies_IG!=min(dfAntsParam_v2$Propspecies_IG)) | dfAntsParam_v2$Nbspecies_IS==0 & dfAntsParam_v2$Propspecies_IG==min(dfAntsParam_v2$Propspecies_IG),]
dfAntsParam_v3 <- dfAntsParam_v2[dfAntsParam_v2$Propspecies_IG!=min(dfAntsParam_v2$Propspecies_IG),]
dfAntsParam_v3 <- dfAntsParam_v2
head(dfAntsParam_v3)
dim(dfAntsParam_v3)

sort(dfAntsParam_v3$SumIGusers)

hist(dfAntsParam_v3$Propspecies_IS)
plot(Propspecies_IS ~ Propspecies_IG, data=dfAntsParam_v3)
dfAntsParam_v3$Propspecies_IS_raw <- dfAntsParam_v3$Nbspecies_IS / dfAntsParam_v3$NbAllSpecies

lm1 = glmmTMB(cbind(Nbspecies_IS, NbAllSpecies) ~ 1 +
                Propspecies_IG +
                (1|sub.family),
              data = dfAntsParam_v3, 
              family = binomial)

summary(lm1)
r2(lm1)

system.time(srlm1 <- simulateResiduals(lm1, n=1000))
testDispersion(simulationOutput = srlm1, alternative ="two.sided")
plot(simulateResiduals(lm1))



lm1_p <- ggemmeans(lm1, c("Propspecies_IG[all]"), type="fe") 
plot(lm1_p, line.size=1.75, dot.size=4, dodge=0.4) 

dfAntsParam_v3.2 <- dfAntsParam_v3#[dfAntsParam_v3$NbAllSpecies<12 & dfAntsParam_v3$NbAllSpecies>8,]

fig3b <- ggplot(data=dfAntsParam_v3.2, aes(x=(Propspecies_IG), y=(Propspecies_IS))) +
  theme_classic() +
  theme(plot.margin = unit(c(2, 2, 1, 1), "cm"),
        axis.title.x = element_text(size = 15, vjust = -2),
        axis.title.y = element_text(size = 15, vjust = 2),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14)) +
  geom_point(size=log(dfAntsParam_v3.2$NbAllSpecies+1)*3,
             alpha=0.2,
             shape = 21,
             colour = "black",
             fill = 'grey65',
             stroke = 0.5) +
  geom_line(data=lm1_p, 
            aes(x=(x), y=car::logit(predicted, adjust=0.001)), 
            size=1.25, 
            col='grey5') +
  geom_ribbon(data = lm1_p, aes(ymin = car::logit(conf.low, adjust=0.001), ymax = car::logit(conf.high, adjust=0.001), x = (x)),
              fill = 'grey35', alpha = 0.4, inherit.aes = FALSE) +
  scale_x_continuous(name="Proportion of species referenced by Instagram users",
                     breaks=car::logit(c(0, 0.01, 0.1, 0.5, 0.9, 0.99, 1), adjust=0.001),
                     labels=c(0, 0.01, 0.1, 0.5, 0.9, 0.99, 1)) +
  scale_y_continuous(name="Proportion of species traded in pet stores",
                     breaks=car::logit(c(0, 0.01, 0.1, 0.5, 0.9, 0.99, 1), adjust=0.001),
                     labels=c(0, 0.01, 0.1, 0.5, 0.9, 0.99, 1))
#geom_text(aes(x=Propspecies_IG, y=jitter(Propspecies_IS,1), label=rownames(dfAntsParam_v3.2)), dfAntsParam_v3.2)

fig3b

# genera in stores but not on IG
dfAntsParam[dfAntsParam$Propspecies_IG==0 & dfAntsParam$Propspecies_IS>0,]

# genera on IG  but not in stores
dfAntsParam[dfAntsParam$Propspecies_IG>0 & dfAntsParam$Propspecies_IS==0,]

# svg('Fig3plot_v1.svg', height = 10, width = 10)
# print(fig3b)
# dev.off()

dfAntsParam_v3["Cryptopone",]
dfAntsParam_v3["Strumigenys",]
dfAntsParam_v3["Lasius",]
dfAntsParam_v3["Wasmannia",]

# Drawing a pretty tree (not in the most elegant way though...)
dfAntsParam_v5 <- dfAntsParam
dfAntsParam_v5$Propspecies_IS <- car::logit(dfAntsParam_v5$Propspecies_IS, adjust=0.001)
dfAntsParam_v5$Propspecies_IG <- car::logit(dfAntsParam_v5$Propspecies_IG, adjust=0.001)
dfAntsParam_v5$Nbspecies_IS <- log(dfAntsParam_v5$Nbspecies_IS+1)
dfAntsParam_v5$Nbspecies_IG <- log(dfAntsParam_v5$Nbspecies_IG+1)
dfAntsParam_v5$NbAllSpecies_log <- log(dfAntsParam_v5$NbAllSpecies+1)
head(dfAntsParam_v5)

variable1 = 'NbAllSpecies_log'
variable2 = 'Propspecies_IS'
variable3 = 'Propspecies_IG'

var1 <- dfAntsParam_v5[,variable1, drop=F]
var1 <- as.matrix(var1)[,1]

var2 <- dfAntsParam_v5[,variable2, drop=F]
var2 <- as.matrix(var2)[,1]

var3 <- dfAntsParam_v5[,variable3, drop=F]
var3 <- as.matrix(var3)[,1]


# for (i in 1:length(var)){if (is.na(as.numeric(var[i]))){ var[i] = -999}}

fit <- phytools::fastAnc(tree_ants, var1, vars=TRUE, CI=TRUE)
td <- data.frame(node = nodeid(tree_ants, names(var1)), NbSpecies = var1)
nd <- data.frame(node = names(fit$ace), NbSpecies = fit$ace)
d <- rbind(td, nd)
d$node <- as.numeric(d$node)
tree <- full_join(tree_ants, d, by = 'node')

range(tree@data$NbSpecies)


fit2 <- phytools::fastAnc(tree_ants, var2, vars=TRUE, CI=TRUE)
td2 <- data.frame(node = nodeid(tree_ants, names(var2)), PropSpecies = var2)
nd2 <- data.frame(node = names(fit2$ace), PropSpecies = fit2$ace)
d2 <- rbind(td2, nd2)
d2$node <- as.numeric(d2$node)

tree@data$PropSpecies <- d2$PropSpecies + abs(min(d2$PropSpecies)) + max(tree@data$NbSpecies) + 2
range(tree@data$PropSpecies)


length(unique(tree@data$PropSpecies)) + length(unique(tree@data$NbSpecies))


fit3 <- phytools::fastAnc(tree_ants, var3, vars=TRUE, CI=TRUE)
td3 <- data.frame(node = nodeid(tree_ants, names(var3)), PropSpecies_IG = var3)
nd3 <- data.frame(node = names(fit3$ace), PropSpecies_IG = fit3$ace)
d3 <- rbind(td3, nd3)
d3$node <- as.numeric(d3$node)

tree@data$PropSpecies_IG <- d3$PropSpecies_IG + abs(min(d3$PropSpecies_IG)) + max(tree@data$PropSpecies) + 2

# range(tree@data$PropSpecies_IG) # >> 9.3 to 17 is the range of normal values, then there is a few large values big values

# for (i in 1:length(tree@data$trait)){
#   if (tree@data$trait[i]< (-998)){tree1@data$trait[i] <- 0}
# }
# tree@data$NbSpecies[tree@data$node %in% as.numeric(tree@phylo$node.label)] = 8.40488
# 

length(unique(tree@data$NbSpecies)) + length(unique(tree@data$PropSpecies)) + length(unique(tree@data$PropSpecies_IG))

tree@data
range(tree@data$NbSpecies)
range(tree@data$PropSpecies) # 125/413 NAs
range(tree@data$PropSpecies_IG) # 101/413 NAs

range_1 <- round((max(tree@data$NbSpecies) - min(tree@data$NbSpecies))*100)
range_2 <- round((max(tree@data$PropSpecies) - min(tree@data$PropSpecies))*100)
range_3 <- round((max(tree@data$PropSpecies_IG) - min(tree@data$PropSpecies_IG))*100)
gap <- round(2*100)

tree_vtemp <- tree
tree_vtemp@data$PropSpecies_size
tree_vtemp@data$PropSpecies_size <- tree_vtemp@data$PropSpecies
tree_vtemp@data$PropSpecies_IG_size <- tree_vtemp@data$PropSpecies_IG
sort(tree_vtemp@data$PropSpecies)
sort(tree_vtemp@data$PropSpecies_IG)

car::logit(0.5, adjust=0.001) + (abs(min(d2$PropSpecies)) + max(tree@data$NbSpecies) + 2) # value for 50%
boot::inv.logit(16.212 -(abs(min(d2$PropSpecies)) + max(tree@data$NbSpecies) + 2))
boot::inv.logit(17.06 -(abs(min(d2$PropSpecies)) + max(tree@data$NbSpecies) + 2))

car::logit(0.5, adjust=0.001) + (abs(min(d3$PropSpecies_IG)) + max(tree@data$PropSpecies) + 2) # value for 50%
boot::inv.logit(32.026 -(abs(min(d3$PropSpecies_IG)) + max(tree@data$PropSpecies) + 2))
boot::inv.logit(32.87 -(abs(min(d3$PropSpecies_IG)) + max(tree@data$PropSpecies) + 2))


tree_vtemp@data$PropSpecies_size[tree_vtemp@data$PropSpecies_size>16.212] = 17.06 # 16.212 is 0.5 (50%) for IS
tree_vtemp@data$PropSpecies_IG_size[tree_vtemp@data$PropSpecies_IG_size>32.026] = 32.87 # 32.026 is 0.5 (50%) for IG





p1 <- ggtree(tree_vtemp, 
             aes(color=NbSpecies), 
             layout = 'circular', 
             ladderize = TRUE, 
             continuous = F, 
             size=0.75) + #, alpha = 0.4
  xlim(-200, 250) +
  scale_colour_gradientn(colours = c(rev(magma(range_1+120)[1:range_1]), 
                                     rep('white', gap+2), 
                                     rep("#454583ff", range_2), 
                                     rep('white', gap), 
                                     rep("#a4951eff", range_3-6))) +
  scale_fill_gradientn(colours = c(rep('grey60', 5), 
                                   rep("#7b7bc5ff", range_2), 
                                   rep('grey60', gap+5), 
                                   rep("#dfd05bff", range_3))) + # careful: several little adjustments were needed to fit colors correctly
  geom_tiplab(offset = 50, 
              size=3) + 
  theme(legend.position = c(0.02, 0.75), # "none", #
        legend.title = element_text(color = "black", size = 8),
        legend.text = element_text(color = "black", size = 8)) +
  ggtitle(variable1) +
  scale_size_identity()

sizePts <- 0.05
decalage <- 0
alpha1 <- 0.15

p2 <- p1 + 
  geom_tippoint(aes(x=x+1, y=y+decalage, size=c(rep(sizePts,413))), shape=20, alpha = alpha1 ) +
  geom_tippoint(aes(x=x+3, y=y+decalage, size=c(rep(sizePts,413))), shape=20, alpha = alpha1 ) +
  geom_tippoint(aes(x=x+5, y=y+decalage, size=c(rep(sizePts,413))), shape=20, alpha = alpha1 ) +
  geom_tippoint(aes(x=x+7, y=y+decalage, size=c(rep(sizePts,413))), shape=20, alpha = alpha1 ) +
  geom_tippoint(aes(x=x+9, y=y+decalage, size=c(rep(sizePts,413))), shape=20, alpha = alpha1  ) +
  geom_tippoint(aes(x=x+11, y=y+decalage, size=c(rep(sizePts,413))), shape=20, alpha = alpha1  ) +
  geom_tippoint(aes(x=x+13, y=y+decalage, size=c(rep(sizePts,413))), shape=20, alpha = alpha1  ) +
  geom_tippoint(aes(x=x+15, y=y+decalage, size=c(rep(sizePts,413))), shape=20, alpha = alpha1  ) +
  geom_tippoint(aes(x=x+17, y=y+decalage, size=c(rep(sizePts,413))), shape=20, alpha = alpha1 ) +
  geom_tippoint(aes(x=x+19, y=y+decalage, size=c(rep(sizePts,413))), shape=20, alpha = alpha1 ) +
  geom_tippoint(aes(x=x+21, y=y+decalage, size=c(rep(sizePts,413))), shape=20, alpha = alpha1 ) +
  geom_tippoint(aes(x=x+23, y=y+decalage, size=c(rep(sizePts,413))), shape=20, alpha = alpha1 ) +
  geom_tippoint(aes(x=x+25, y=y+decalage, size=c(rep(sizePts,413))), shape=20, alpha = alpha1  ) +
  geom_tippoint(aes(x=x+27, y=y+decalage, size=c(rep(sizePts,413))), shape=20, alpha = alpha1  ) +
  geom_tippoint(aes(x=x+29, y=y+decalage, size=c(rep(sizePts,413))), shape=20, alpha = alpha1  ) +
  geom_tippoint(aes(x=x+31, y=y+decalage, size=c(rep(sizePts,413))), shape=20, alpha = alpha1  ) +
  geom_tippoint(aes(x=x+33, y=y+decalage, size=c(rep(sizePts,413))), shape=20, alpha = alpha1  ) +
  geom_tippoint(aes(x=x+35, y=y+decalage, size=c(rep(sizePts,413))), shape=20, alpha = alpha1 ) +
  geom_tippoint(aes(x=x+37, y=y+decalage, size=c(rep(sizePts,413))), shape=20, alpha = alpha1  ) +
  geom_tippoint(aes(x=x+39, y=y+decalage, size=c(rep(sizePts,413))), shape=20, alpha = alpha1  ) +
  geom_tippoint(aes(x=x+41, y=y+decalage, size=c(rep(sizePts,413))), shape=20, alpha = alpha1  ) +
  geom_tippoint(aes(x=x+43, y=y+decalage, size=c(rep(sizePts,413))), shape=20, alpha = alpha1  ) +
  geom_tippoint(aes(x=x+45, y=y+decalage, size=c(rep(sizePts,413))), shape=20, alpha = alpha1 ) +
  geom_tippoint(aes(x=x+47, y=y+decalage, size=c(rep(sizePts,413))), shape=20, alpha = alpha1  ) +
  geom_tippoint(aes(x=x+49, y=y+decalage, size=c(rep(sizePts,413))), shape=20, alpha = alpha1  )

p3 <- p2 + geom_tippoint(aes(x = x+14,
                             y = y+decalage,
                             size = scales::rescale(PropSpecies_size, to=c(0.75, 6.25)),
                             color = PropSpecies, 
                             fill = PropSpecies), 
                         shape = 21 )

p4 <- p3 + geom_tippoint(aes(x = x+36, 
                             y = y+decalage, 
                             size = scales::rescale(PropSpecies_IG_size, to=c(0.75, 6.25)), 
                             color = PropSpecies_IG, 
                             fill = PropSpecies_IG), 
                         shape = 21 )

fig3_tree <- p4 + geom_treescale(x=-50, offset=1, width=40)
fig3_tree

# svg('Fig3tree_v1.svg', height = 10, width = 10)
# print(fig3_tree)
# dev.off()





# legend plot for vizualizing the sub-families

rangeSpSF <- range(sort(table(GSP_ants_v2$sub.family)))
rangeSpSF_logged <- round(log(rangeSpSF+1)*100)

valuesSF_log <- rev(round(log(sort(table(GSP_ants_v2$sub.family), decreasing=T) + 1)*80)[1:10])

colSF_base <- rainbow(rangeSpSF_logged[2])
colSF <- colSF_base[valuesSF_log[1:10]]

plot((sort(table(GSP_ants_v2$sub.family), decreasing=T)[1:10]), col=colSF, lwd=10)

alpha0 <- 0.5
ext1 <- 10
names.sf <- unique(GSP_ants_v2$sub.family)[c(1,8,9,16,17,3,7,6,13,18)]

fig2b <- ggtree(tree, 
                layout = 'circular', 
                ladderize = TRUE, 
                continuous = F, 
                size=1) +
  xlim(-100, 250) + 
  geom_hilight(node=221, fill=colSF[1], extend=ext1+2, alpha=alpha0) + #1 Myrmicinae 
  geom_hilight(node=217, fill=colSF[6], extend=ext1-1, alpha=alpha0) + #6 Ectatamminae
  geom_hilight(node=294, fill=colSF[2], extend=ext1+2, alpha=alpha0) + #2 Formicinae
  geom_hilight(node=341, fill=colSF[10], extend=ext1-1, alpha=alpha0) + #3 Ponerinae
  geom_hilight(node=359, fill=colSF[8], extend=ext1+2, alpha=alpha0) + #8 Proceratiinae
  geom_hilight(node=362, fill=colSF[5], extend=ext1-1, alpha=alpha0) + #9 Amblyoponae
  geom_hilight(node=369, fill=colSF[9], extend=ext1+2, alpha=alpha0) + #5 Dorylinae
  geom_hilight(node=386, fill=colSF[4], extend=ext1-1, alpha=alpha0) + #4 Dolichoderinae
  geom_hilight(node=411, fill=colSF[3], extend=ext1+2, alpha=alpha0) + #10 Myrmeciinae
  geom_hilight(node=412, fill=colSF[7], extend=ext1-1, alpha=alpha0)  #7 Pseudomyrmicinae

fig2b

# pdf('Figure2b.pdf', height = 10, width = 10)
# print(fig2b)
# dev.off()








#################################
####     Species stats       ####
#################################

head(dfIS)
head(dfIG)
dim(dfIS)
dim(dfIG)

dfSpecies <- data.frame(unique(c(row.names(dfIS), row.names(dfIG))))
colnames(dfSpecies) <- "species"
rownames(dfSpecies) <- dfSpecies$species
dfSpecies$NbSellers <- 0
dfSpecies$MeanPrice <- NA
dfSpecies$NbIGUsers <- 0
dfSpecies$NbIGPosts <- 0
dfSpecies$NbIGPostsByUser <- 0
dfSpecies$NbLikes <- NA
dfSpecies$NbComments <- NA
dfSpecies$inIS <- 0
dfSpecies$inIG <- 0
dfSpecies$sub.family <- NA
dfSpecies$genus <- NA

dim(dfSpecies)
head(dfSpecies)
head(GSP_ants_v2)

for (i in dfSpecies$species){
  dfSpecies[i, "sub.family"] <- GSP_ants_v2[GSP_ants_v2$species== i, "sub.family"]
  dfSpecies[i, "genus"]  <- GSP_ants_v2[GSP_ants_v2$species== i, "genus"]
  if(i %in% row.names(dfIS)){
    dfSpecies[i, "NbSellers"] <- dfIS[i, "NbSellers_2020"]
    dfSpecies[i, "MeanPrice"] <- dfIS[i, "MeanPrice_2020"]
    dfSpecies[i, "inIS"] <- 1
  }
  if(i %in% row.names(dfIG)){
    dfSpecies[i, "NbIGUsers"] <- dfIG[i, "byUser"]
    dfSpecies[i, "NbIGPosts"] <- dfIG[i, "byPost"]
    dfSpecies[i, "NbIGPostsByUser"] <- dfIG[i, "PostsByUser"]
    dfSpecies[i, "NbLikes"] <- dfIG[i, "medianLikes"]
    dfSpecies[i, "NbComments"] <- dfIG[i, "medianComments"]
    dfSpecies[i, "inIG"] <- 1
  }
}

sum(dfSpecies$inIS)
sum(dfSpecies$inIG)

# add invasiveness status
dfSpecies$InvasivenessStatus <- NA
for (species in dfSpecies$species){
  dfSpecies[species, "InvasivenessStatus"] <- as.character(GSP_ants_v2$InvasivenessStatus[GSP_ants_v2$species==species])
}

## are online stores and IG identifying the same pet species?
head(GSP_ants_v2)
dim(GSP_ants_v2)
dim(dfSpecies)
TC1 <- table(dfSpecies[,c("inIS", "inIG")])
TC1[1,1] <- dim(GSP_ants_v2)[1] - dim(dfSpecies)[1]
chisq.test(TC1) # X-squared = 6249.8, df = 1, p-value < 0.00000000000000022

fill_cols  <- alpha(c('#7b7bc5ff', '#dfd05bff', '#d0d0d0ff'), alpha= 0.9)
ants_euler <- list(IS = dfSpecies$species[dfSpecies$inIS==1],
                   IG = dfSpecies$species[dfSpecies$inIG==1],
                   #alien = GSP_ants_v2$species[GSP_ants_v2$InvasivenessStatus !="Native"],
                   Global = GSP_ants_v2$species)
fig4a <- plot(euler(ants_euler, shape = "ellipse"), quantities = TRUE, fill = fill_cols, lwd=2.5)

fig4a

# pdf("figure4a_v1.pdf", width=8, height=8)
# fig4a
# dev.off()



dim(dfSpecies)
head(dfSpecies)


# species found in online pet stores only
dfISonly <- dfSpecies[dfSpecies$inIS==1,]
dfISonly$inIG <- as.factor(dfISonly$inIG)
head(dfISonly)

wilcox.test(dfISonly$NbSellers[dfISonly$inIG==0], dfISonly$NbSellers[dfISonly$inIG==1])

mean(dfISonly$NbSellers[dfISonly$inIG==1]) / mean(dfISonly$NbSellers[dfISonly$inIG==0])

sum(dfISonly$NbSellers[dfISonly$inIG==1]) / sum(dfISonly$NbSellers)


#svg('Fig4b_v1.svg', height = 8, width = 6)
dfISonly %>% 
  ggplot(aes(x=inIG, y=NbSellers)) +
  geom_boxplot(width=0.5,lwd=1.5, aes(fill=inIG, col=inIG)) +
  scale_fill_manual(values=alpha(c('#7b7bc5ff', '#42c27fff'), alpha= 0.8)) +
  geom_jitter(width=0.175, height=0.05, size=2.5) +
  scale_color_manual(values=alpha(c('#28285bff', '#1a5235ff'), alpha= 1)) +
  scale_y_continuous(name= "Number of online sellers", trans='log2', breaks=c(1,2,4,8,16,32,64)) +
  scale_x_discrete(name ="", labels=c("Not referenced on IG", "Referenced on IG")) +
  theme_base()
#dev.off()


# species on instagram only
dfIGonly <- dfSpecies[dfSpecies$inIG==1,]
dfIGonly$inIS <- factor(dfIGonly$inIS, levels=c("1","0"))

wilcox.test(dfIGonly$NbIGUsers[dfIGonly$inIS==0], dfIGonly$NbIGUsers[dfIGonly$inIS==1])

mean(dfIGonly$NbIGUsers[dfIGonly$inIS==1]) / mean(dfIGonly$NbIGUsers[dfIGonly$inIS==0])

sum(dfIGonly$NbIGUsers[dfIGonly$inIS==1])/sum(dfIGonly$NbIGUsers)


#svg('Fig4c_v1.svg', height = 8, width = 6)
dfIGonly %>% 
  ggplot(aes(x=inIS, y=NbIGUsers )) +
  geom_boxplot(width=0.5,lwd=1.5, aes(fill=inIS, col=inIS)) +
  scale_fill_manual(values=alpha(c('#42c27fff', '#dfd05bff'), alpha= 0.8)) +
  geom_jitter(width=0.175, height=0.05, size=2.5) +
  scale_color_manual(values=alpha(c('#1a5235ff', '#776c17ff'), alpha= 1)) +
  scale_y_continuous(name= "Number of users referencing the species as a pet on Instagram", trans='log2', breaks=c(1,2,4,8,16,32,64,128,256,512)) +
  scale_x_discrete(name ="", labels=c("Species traded", "Species not traded")) +
  theme_base()
#dev.off()

#

########## Can IG predict NbSellers? ##########
dftemp1 <- dfSpecies#[dfSpecies$inIG==1,] #  & dfSpecies$inIS==1
dftemp1$NbIGUsers_log <- log(dftemp1$NbIGUsers+1)
dftemp1$NbIGPosts_log <- log(dftemp1$NbIGPosts+1)
dftemp1$NbIGPostsByUser_log <- log(dftemp1$NbIGPostsByUser+1)
dftemp1$NbLikes_log <- log(dftemp1$NbLikes+1)
dftemp1$NbComments_log <- log(dftemp1$NbComments +1)
dftemp1$MeanPrice_log <- log(dftemp1$MeanPrice+1)
dftemp1$NbSellers_log <- log(dftemp1$NbSellers+1)
hist(dftemp1$NbSellers_log)
dftemp1$InvasivenessStatus <- factor(dftemp1$InvasivenessStatus, levels=c("Native","Alien","Invasive"))
#dftemp1$NBLikes[is.na(dftemp1$NbLikes)] <- 0
dim(dftemp1)
head(dftemp1)

dftemp1$group <- NA
for (i in 1:dim(dftemp1)[1]){
  if (dftemp1$inIS[i]==1 & dftemp1$inIG[i]==1){dftemp1$group[i] <-"IG and sellers"
  } else if (dftemp1$inIS[i]==1 & dftemp1$inIG[i]==0){dftemp1$group[i] <-"Sellers only"
  } else{dftemp1$group[i] <-"IG only"}
}
dftemp1$group <- as.factor(dftemp1$group)

try1 <- ggplot(dftemp1, aes(x=NbIGUsers_log, y=NbSellers, colour=factor(group))) +
  scale_colour_manual(values=alpha(c('#42c27fff', '#dfd05bff', '#7b7bc5ff'), alpha= 0.8)) +
  geom_jitter(width=0.005, size=2.5) +
  theme_base()
try1

corrplot::corrplot(cor(dftemp1[dfSpecies$inIG==1, c(14,16,17,18)]), method="number")


hist(dftemp1$NbSellers[dftemp1$inIG==1], nclass=100) # clearly zero-inflated and potentially overdispersed
table(dftemp1$NbSellers[dftemp1$inIG==1])

reg1 <- glmmTMB(NbSellers ~  1 +
                  NbIGUsers_log +
                  NbIGPostsByUser_log +
                  NbLikes_log +
                  (1|sub.family) +
                  (1|genus)
                
                ,
                ziformula = ~ 1 +
                  NbIGUsers_log +
                  NbIGPostsByUser_log +
                  NbLikes_log +
                  (1|sub.family) +
                  (1|genus)
                ,
                family=nbinom2,
                data=dftemp1[dftemp1$inIG==1,])

# drop1(reg1, test="Chisq")

summary(reg1)
r2_zeroinflated(reg1, method="correlation")
plot_model(reg1, transform = NULL)

# effectsize(reg1)

system.time(sr2 <- simulateResiduals(reg1, n=1000))
testDispersion(simulationOutput = sr2, alternative ="two.sided")
plot(simulateResiduals(reg1))


ef_reg1 <- ggpredict(reg1, c("NbIGUsers_log"), back.transform=TRUE, type="fe.zi") # , condition = list(NbAllSpecies_log =2)
plot(ef_reg1, line.size=1.75, dot.size=4, dodge=0.4) 


fig5d <- ggplot(data=dftemp1, aes(x=(NbIGUsers_log), y=NbSellers_log)) +
  theme_clean() +
  theme(plot.margin = unit(c(2, 2, 1, 1), "cm"),
        axis.title.x = element_text(size = 15, vjust = -2),
        axis.title.y = element_text(size = 15, vjust = 2),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14)) +
  geom_jitter(data= dftemp1, 
              aes(fill=factor(group), colour=factor(group), size=NbIGPostsByUser), #size=NbIGPostsByUser, 
              #alpha=0.5,
              #size=4,
              width=0.05,
              height = 0.05,
              stroke=1.25,
              shape=21) +
  # geom_jitter(data= dftemp1[dftemp1$InvasivenessStatus=="Native",], 
  #             aes(size=NbIGPostsByUser, fill=factor(group), colour=factor(group)),
  #             alpha=0.5,
  #             width=0.05,
  #             height = 0.05,
  #             stroke=1,
  #             shape=21) +
  # geom_jitter(data= dftemp1[dftemp1$InvasivenessStatus!="Native",], 
  #             aes(size=NbIGPostsByUser, fill=factor(group)),
  #             alpha=0.5,
  #             width=0.05,
#             height = 0.05,
#             stroke=1.25,
#             shape=22) +
scale_size_continuous(range = c(1.5, 10)) +
  #geom_text(aes(x=NbIGUsers_log, y=jitter(NbSellers_log,1), label=species), dftemp1) +
  geom_line(data=ef_reg1, 
            aes(x=(x), y=log(predicted+1)), 
            size=1.25, 
            col='grey55') +
  geom_ribbon(data = ef_reg1, aes(ymin = log(conf.low+1), ymax = log(conf.high+1), x = (x)),
              fill = 'grey35', alpha = 0.3, inherit.aes = FALSE) +
  scale_y_continuous(name="Number of sellers",
                     breaks=log(c(0, 1, 2, 5, 10, 20, 50, 100, 200)+1),
                     labels=c(0, 1, 2, 5, 10, 20, 50, 100, 200)) +
  scale_x_continuous(name="Number of Instagram users",
                     breaks=log(c(0, 1, 2, 5, 10, 20, 50, 100, 200, 500)+1),
                     labels=c(0, 1, 2, 5, 10, 20, 50, 100, 200, 500)) +
  scale_fill_manual(values=alpha(c('#42c27fff', '#dfd05bff', '#7b7bc5ff'), alpha= 0.4)) +
  scale_colour_manual(values=alpha(c('#42c27fff', '#dfd05bff', '#7b7bc5ff'), alpha= 0.8)) +
  theme(legend.title = element_text(size=8, color = "black", face="bold"),
        legend.justification=c(0,1), 
        legend.position=c(0.05, 0.95),
        legend.background = element_blank(),
        legend.key = element_blank())
fig5d



# pdf('Fig5_v2.pdf', height = 8, width = 8)
# fig5d
# dev.off()



