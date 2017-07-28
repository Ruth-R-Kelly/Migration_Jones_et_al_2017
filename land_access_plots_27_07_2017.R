### R script for summary tables and plot of landuse by site and migration status. 
### For paper "Human migration to the forest frontier: implications for conservation management in Madagascar"

### Script by Ruth Kelly - kellyr44@tcd.ie - 28-07-2017

library("dplyr")
library("ggplot2")
library("vegan")
library("reshape2")


#### Data preprocessing ####

## Load datasheet with information about land use for agri subset. 

land_use <- read.csv( "land_access_5_sites_17_07_2017.csv" )

### use decostand from package 'vegan' to turn dataset into 0's and 1's

pa_land_use <- decostand(land_use[,2:7], method = "pa")

head(pa_land_use)

land_use2 <- cbind(land_use$Resp_id, pa_land_use)

summary(land_use2)
## load info on Migration status

hhinfo <- read.csv("hhinfo2_02072017.csv")

core_info <- dplyr::select(hhinfo, resp_id, Mig_status, site, PA)

fdata <- merge(core_info, land_use2, by.x = "resp_id", by.y = "land_use$Resp_id", all = FALSE)

### Make Migrant status and site into 'factor' variables and give them labels/levels. 

fdata$MigrantF <- factor(fdata$Mig_status, levels = c(0,1), labels = c("Migrant", "Non-migrant"))

fdata$site  <- ordered(fdata$site, levels = c("Ampahitra" , "Sahavazina",  "Mantadia", "Zahamena" ,"Amporoforo"))

#####


####   Making data summary tables ####

sumA <- as.data.frame(summarise(group_by(fdata, site, MigrantF),
                                Inherited = sum(Inherited)/length(Inherited),
                                Rented = sum(Rented)/length(Rented),
                                Borrowed = sum(Borrowed)/length(Borrowed),
                                Cleared = sum(Cleared)/length(Cleared),
                                Bought = sum(Bought)/length(Bought)))
sumA

####
sum_site <- as.data.frame(summarise(group_by(fdata, site),
                                Inherited_prop = sum(Inherited)/length(Inherited),
                                Rented_prop = sum(Rented)/length(Rented),
                                Borrowed_prop = sum(Borrowed)/length(Borrowed),
                                Cleared_prop = sum(Cleared)/length(Cleared),
                                Bought_prop = sum(Bought)/length(Bought)))

sum_site

#####
sum_Mig <- as.data.frame(summarise(group_by(fdata, MigrantF),
                                    Inherited_prop = sum(Inherited)/length(Inherited),
                                    Rented_prop = sum(Rented)/length(Rented),
                                    Borrowed_prop = sum(Borrowed)/length(Borrowed),
                                    Cleared_prop = sum(Cleared)/length(Cleared),
                                    Bought_prop = sum(Bought)/length(Bought)))


### Average accross sites to get equal weighting per site rather than per household. 

Ave1 <- as.data.frame(summarise(group_by(sumA, MigrantF),
                                Inherited = sum(Inherited)/length(Inherited),
                                Rented = sum(Rented)/length(Rented),
                                Borrowed = sum(Borrowed)/length(Borrowed),
                                Cleared = sum(Cleared)/length(Cleared),
                                Bought = sum(Bought)/length(Bought)))


### processing to match colnames so that this can be bound to site summary dataset. 

names(sumA)[1:2] <- c("Site", "Migration")

Ave_Mig2 <- cbind("Average", Ave1 )

names(Ave_Mig2) <- names(sumA)

aves1 <- rbind(sumA, Ave_Mig2)

#### Use function 'melt' from package 'reshape2' to data format for plotting ####

stacked1 <- melt(aves1, id = c("Site", "Migration"), value.name = "Proportion")

# set factor levels with preferred order for plotting. 

stacked1$variable <- ordered(stacked1$variable, 
                      levels = c("Inherited" , "Cleared",  "Borrowed", "Bought" ,"Rented"))

stacked1$Migration <- ordered(stacked1$Migration, 
                             levels = c("Non-migrant" , "Migrant"))
stacked1


#### Draw plot using ggplot2. 

### 
#pdf("SuppA3_land_access.pdf", width = 7.5, height = 7.5)
ggplot(data=stacked1, aes(x=variable, y=Proportion, fill = variable )) +
  geom_bar(stat="identity", position=position_dodge()) + 
  facet_grid(Site ~ Migration) +
  theme_bw() + 
  scale_fill_hue(guide = guide_legend(title = NULL)) + 
  theme(axis.title = element_text(size = 11)) + 
  theme(panel.grid = element_blank()) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  ylab("Proportion of households with each land type") +
  xlab("") +
  theme(legend.position = "bottom") + 
  theme(axis.title.y = element_text(size = 11))
#dev.off()


####
