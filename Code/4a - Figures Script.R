# Figure making script
## This script produces figures 2, 3, and 4 for the seed dispersal ms

library(dplyr)
library(ggplot2)
library(ggpubr)
library(rstatix)
library(tidyr)
library(terra)
library(reshape2)
library(agricolae)
library(car)
library(statmod)
library(margins)
library(stringr)
library(visreg)
library(DescTools)
setwd("D:/Dryad_submission/Data/")

seedling_density_df <- read.csv("seedling_density_df_2-24-22.csv")
seedling_density_df <- seedling_density_df[,-1]

seedling_density_df$aspect_card <- NA
seedling_density_df$aspect_card[seedling_density_df$Aspect <= 45 | seedling_density_df$Aspect > 315] <- "North"
seedling_density_df$aspect_card[seedling_density_df$Aspect > 45 & seedling_density_df$Aspect <= 135] <- "East"
seedling_density_df$aspect_card[seedling_density_df$Aspect > 135 & seedling_density_df$Aspect <= 225] <- "South"
seedling_density_df$aspect_card[seedling_density_df$Aspect > 225 & seedling_density_df$Aspect <= 315] <- "West"

seedling_density_df$aspect_card <- factor(seedling_density_df$aspect_card, levels = c("North", "East", "South", "West"))

## I made a dataframe of patch characteristics to help with selecting pixels in patches >= 1 ha in size
patch_characteristics <- read.csv("patch_characteristics_df.csv")

seedling_density_df <- merge(seedling_density_df, patch_characteristics, by.x = "patch_no", by.y = "Id")

### Matt agreed that it'll be good to limit the patch sizes I use to 1 hectare (based on a threshold for dispersal from chambers et al of 50 m, so 100x100 m minimum size)
seedling_density_df <- seedling_density_df[seedling_density_df$Shape_Area > 10000,]

### Marginal means plot 
model_data <- read.csv("final_model_df.csv")
model_data$X <- NULL

check<- model_data %>% unique() %>% group_by(Fire, Patch, Transect, Prevailing_Wind) %>%  tally()

#### For the binomial part of the hurdle model
model_data$binom <- ifelse(model_data$Seedling_density > 0, 1, 0)

best_binom_model <- glm(binom ~ Combined_slope + log2(Distance) + Ponderosa_overstory_density + Cont_Prevailing_Wind + factor(Aspect), data = model_data, family = binomial())

summary(best_binom_model)

par(mfrow=c(1,1))
margins_mfx_binom <- margins(best_binom_model, data = model_data, type = "link")

margins_mfx_binom_summary <- summary(margins_mfx_binom)

margins_mfx_binom_summary$factor <- factor(margins_mfx_binom_summary$factor, levels = c("Cont_Prevailing_Wind","AspectN", "AspectS", "AspectW", "Ponderosa_overstory_density", "Distance", "Combined_slope"))

## Figure 2 (figure 1 of the map was made in arcmap)
ggplot(margins_mfx_binom_summary, aes(x = exp(AME), y = factor(factor))) + ## To exclude pipo density [margins_mfx_binom_summary$factor != "Ponderosa_overstory_density",]
  geom_point(size = 2) +
  geom_errorbar(aes(xmin = exp(lower), xmax = exp(upper)), width = 0, size = 1) +
  geom_vline(xintercept = 1, size = 1, linetype = "dashed") +
  xlab("Standardized coefficient") +
  ylab(NULL) +
  # scale_x_continuous(breaks = seq(0.5,3.5, 0.25)) +
  scale_y_discrete(labels = c("Prevailing Wind *", "North Aspect ***", "South Aspect **", "West Aspect", "Ponderosa overstory density ***", "Distance to seed source ***","Slope")) +
  # ggtitle("Marginal effects of predictor variables") +
  theme(text = element_text(size=20), 
        # axis.text.y = element_text(face = ifelse(margins_mfx_binom_summary$factor == "AspectN" | margins_mfx_binom_summary$factor == "Combined_slope" | 
        #                                            margins_mfx_binom_summary$factor == "Cont_Prevailing_Wind" | margins_mfx_binom_summary$factor =="Distance" |
        #                                            margins_mfx_binom_summary$factor =="Ponderosa_overstory_density", "bold", "plain")),
        panel.background = element_rect(fill = "transparent", color = "black"),
        panel.grid.major = element_line(color = "gray86"))

### Figure 2 inset
margins_mfx_binom_summary_dist <- summary(margins(best_binom_model, data = model_data, type = "link", variables = c("Combined_slope", "Distance", "Ponderosa_overstory_density")))

margins_mfx_binom_summary_dist$factor <- factor(margins_mfx_binom_summary_dist$factor, levels = c("Ponderosa_overstory_density",
                                                                                      "Distance", "Combined_slope"))

ggplot(margins_mfx_binom_summary_dist, aes(x = exp(AME), y = factor(factor))) + ## To exclude pipo density [margins_mfx_binom_summary$factor != "Ponderosa_overstory_density",]
  geom_point(size = 2) +
  geom_errorbar(aes(xmin = exp(lower), xmax = exp(upper)), width = 0, size = 1) +
  geom_vline(xintercept = 1, size = 1, linetype = "dashed") +
  xlab(NULL) +
  ylab(NULL) +
  #xlim(-0.028, 0.01) +
  scale_y_discrete(labels = c("PIPO overstory density","Distance to seed source","Slope")) +
  # ggtitle("Marginal effect of distance") +
  theme(text = element_text(size=30), 
        panel.background = element_rect(fill = "transparent", color = "black"),
        panel.grid.major = element_line(color = "gray86"))


#### make general linear model again
model_data <- model_data[model_data$binom > 0, ]
best_model <- glm(log(Seedling_density) ~ Combined_slope + Distance + poly(Ponderosa_overstory_density,2) + poly(Cont_Prevailing_Wind,2) + factor(Aspect), data = model_data)

summary(best_model)

### This is using the package margins (based on Stata which seems like a more well recognized piece of stat software)
margins_best_model <- glm(log(Seedling_density) ~ Combined_slope + Distance + stats::poly(Ponderosa_overstory_density,2) + stats::poly(Cont_Prevailing_Wind,2) + factor(Aspect), 
                          data = model_data)
summary(margins_best_model)

par(mfrow=c(1,1))
margins_mfx <- margins(margins_best_model, data = model_data, type = "link")

margins_mfx_summary <- summary(margins_mfx)

margins_mfx_summary$factor <- factor(margins_mfx_summary$factor, levels = c("Cont_Prevailing_Wind","AspectN", "AspectS", "AspectW", "Ponderosa_overstory_density",
                                                                            "Distance", "Combined_slope"))
## Figure 3 (figure 1 of the map was made in arcmap)
ggplot(margins_mfx_summary, aes(x = exp(AME), y = factor(factor))) + ## To exclude pipo density [margins_mfx_summary$factor != "Ponderosa_overstory_density",]
  geom_point(size = 2) +
  geom_errorbar(aes(xmin = exp(lower), xmax = exp(upper)), width = 0, size = 1) +
  geom_vline(xintercept = 1, size = 1, linetype = "dashed") +
  xlab("Standardized coefficient") +
  ylab(NULL) +
  scale_y_discrete(labels = c("Prevailing Wind ***", "North Aspect ***", "South Aspect", "West Aspect", "Ponderosa overstory density *", "Distance to seed source ***","Slope **")) +
  ggtitle("Marginal effects of predictor variables") +
  theme(text = element_text(size=20), 
        # axis.text.y = element_text(face = ifelse(margins_mfx_summary$factor == "AspectN" | margins_mfx_summary$factor == "Combined_slope" | 
        #                                            margins_mfx_summary$factor == "Cont_Prevailing_Wind" | margins_mfx_summary$factor =="Distance" |
        #                                            margins_mfx_summary$factor =="Ponderosa_overstory_density", "bold", "plain")),
        panel.background = element_rect(fill = "transparent", color = "black"),
        panel.grid.major = element_line(color = "gray86"))

### Figure 3 inset
margins_mfx_summary_dist <- summary(margins(margins_best_model, data = model_data, type = "link", variables = c("Combined_slope", "Distance", "Ponderosa_overstory_density")))

margins_mfx_summary_dist$factor <- factor(margins_mfx_summary_dist$factor, levels = c("Ponderosa_overstory_density",
                                                                            "Distance", "Combined_slope"))

ggplot(margins_mfx_summary_dist, aes(x = exp(AME), y = factor(factor))) + ## To exclude pipo density [margins_mfx_summary$factor != "Ponderosa_overstory_density",]
  geom_point(size = 2) +
  geom_errorbar(aes(xmin = exp(lower), xmax = exp(upper)), width = 0, size = 1) +
  geom_vline(xintercept = 1, size = 1, linetype = "dashed") +
  xlab(NULL) +
  ylab(NULL) +
  #xlim(-0.028, 0.01) +
  scale_y_discrete(labels = c("PIPO overstory density","Distance to seed source","Slope")) +
  # ggtitle("Marginal effect of distance") +
  theme(text = element_text(size=30), 
        panel.background = element_rect(fill = "transparent", color = "black"),
        panel.grid.major = element_line(color = "gray86"))


## Figure 4 - Distance only model comparison
### Prep the full model data with null model data for determining differences in regen
#### Just to note, this dataframe is already subsetted for pixels within patches > 1ha in size
seedling_density_df <- read.csv("seedling_presence_density_fullmodel_distancemodel_1-8-24.csv")

names(seedling_density_df)[names(seedling_density_df) == 'Aspct'] <- 'Aspect'

seedling_density_df$aspect_card <- NA
seedling_density_df$aspect_card[seedling_density_df$Aspect <= 45 | seedling_density_df$Aspect > 315] <- "North"
seedling_density_df$aspect_card[seedling_density_df$Aspect > 45 & seedling_density_df$Aspect <= 135] <- "East"
seedling_density_df$aspect_card[seedling_density_df$Aspect > 135 & seedling_density_df$Aspect <= 225] <- "South"
seedling_density_df$aspect_card[seedling_density_df$Aspect > 225 & seedling_density_df$Aspect <= 315] <- "West"

seedling_density_df$slope_pos <- ifelse(seedling_density_df$slope<0, "downslope", "upslope")
seedling_density_df$wind_pos <- ifelse(seedling_density_df$wnd_0>0, "downwind", "upwind")
# seedling_density_df$wind_pos <- factor(seedling_density_df$wind_pos, levels = c("Upwind", "Downwind"))

seedling_density_df$aspect_card <- factor(seedling_density_df$aspect_card, levels = c("North", "East", "South", "West"))

### Calculate regen estimates based on hurdle model, using a threshold value of 0.4 for presence absence 
#### I decided upon a value of 0.4 because that is the threshold to have favorable odds that regeneration would occur given the 40% chance of regeneration occurring within the 736 binds (288 bins had regeneration, so 40% frequency). I felt comfortable converting 40% frequency to a 40% chance because this value is very similar to the frequency found in Rother and Veblen 2016 and Haffey et al. 2018 for ponderosa pine within 150 m of intact forest.
seedling_density_df$Full_model_density <- ifelse(seedling_density_df$seedling_presence_windslope > 0.4, seedling_density_df$seedling_density_windslope, 0)

seedling_density_df$Distance_only_density <- ifelse(seedling_density_df$seedling_presence_distance > 0.4, seedling_density_df$seedling_density_distance, 0)

seedling_density_df$wind_slope <- paste0("Burned area ", seedling_density_df$wind_pos, "-", seedling_density_df$slope_pos)
seedling_density_df$wind_slope <- factor(seedling_density_df$wind_slope, levels = c("Burned area upwind-upslope", 
                                                                                    "Burned area downwind-upslope",
                                                                                    "Burned area upwind-downslope", 
                                                                                    "Burned area downwind-downslope"))

seedling_density_df$density_diff <- seedling_density_df$Full_model_density - seedling_density_df$Distance_only_density
seedling_density_df$aspect_card <- factor(seedling_density_df$aspect_card, levels = c("North","East", "South", "West"))


## If I was to resample
# seedling_density_df_50 <- seedling_density_df %>% 
#   group_by(wind_slope, aspect_card) %>% 
#   sample_frac(.5)

Aspect_95CI <- seedling_density_df %>% 
  group_by(wind_slope, aspect_card, bin) %>% 
  summarize(min = mean(density_diff) - sd(density_diff),
            max = mean(density_diff) + sd(density_diff),
            mean =mean(density_diff))

aspect_ribbon_gg <- ggplot() +
  stat_smooth(Aspect_95CI, mapping =aes(x = bin, color = aspect_card, y = min), linetype = "dashed", method = "lm", se = FALSE) +
  stat_smooth(Aspect_95CI, mapping =aes(x = bin, color = aspect_card, y = max), linetype = "dashed", method = "lm", se = FALSE) +
  #geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1.1) +
  geom_smooth(Aspect_95CI, method = lm, mapping = aes(x = bin, color = aspect_card, y = mean), se = F)+
  scale_color_manual(name = "Aspect", values = c(North = "#440154FF", East = "#31688EFF", South = "#35B779FF", 
                                                 West = "orange"),
                     labels = c("North", "East", "South", "West")) +
  # scale_color_viridis(name = "Aspect", labels = c("North", "East", "South", "West", "X = Y"), discrete = T) +
  facet_wrap(~wind_slope, ncol = 2) +
  xlab("Distance from intact forest (m)") +
  ylab("") +
  scale_y_continuous(breaks = seq(-500, 2500, by = 500)) +
  theme_classic() +
  theme(text = element_text(size=24)) #axis.title.y=element_blank(), axis.text.y = element_blank(),

aspect_ribbon_gg <- ggplot_build(aspect_ribbon_gg)

df2_aspect_ribbon <- data.frame(x = aspect_ribbon_gg$data[[1]]$x,
                                CI_min = aspect_ribbon_gg$data[[1]]$y,
                                CI_max = aspect_ribbon_gg$data[[2]]$y,
                                aspect_card = aspect_ribbon_gg$data[[1]]$group,
                                wind_slope = aspect_ribbon_gg$data[[1]]$PANEL)

df2_aspect_ribbon$aspect_card <- as.character(df2_aspect_ribbon$aspect_card)

df2_aspect_ribbon$aspect_card[df2_aspect_ribbon$aspect_card == 1] <- "North"
df2_aspect_ribbon$aspect_card[df2_aspect_ribbon$aspect_card == 2] <- "East"
df2_aspect_ribbon$aspect_card[df2_aspect_ribbon$aspect_card == 3] <- "South"
df2_aspect_ribbon$aspect_card[df2_aspect_ribbon$aspect_card == 4] <- "West"

df2_aspect_ribbon$wind_slope <- as.character(df2_aspect_ribbon$wind_slope)

df2_aspect_ribbon$wind_slope[df2_aspect_ribbon$wind_slope == 1] <- "Burned area upwind-upslope"
df2_aspect_ribbon$wind_slope[df2_aspect_ribbon$wind_slope == 2] <- "Burned area downwind-upslope"
df2_aspect_ribbon$wind_slope[df2_aspect_ribbon$wind_slope == 3] <- "Burned area upwind-downslope"
df2_aspect_ribbon$wind_slope[df2_aspect_ribbon$wind_slope == 4] <- "Burned area downwind-downslope"

df2_aspect_ribbon$wind_slope <- factor(df2_aspect_ribbon$wind_slope, levels = c("Burned area upwind-upslope",
                                                                                "Burned area downwind-upslope",
                                                                                "Burned area upwind-downslope", 
                                                                                "Burned area downwind-downslope"))

fig_4 <- ggplot() +
  geom_ribbon(data = df2_aspect_ribbon, aes(x = x, ymin = CI_min, ymax = CI_max, fill = aspect_card), alpha = 0.3, show.legend = F) +
  # geom_point(seedling_density_df, mapping = aes(x = bin, color = aspect_card, y = density_diff), alpha = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1.1) +
  geom_smooth(Aspect_95CI, method = lm, mapping = aes(x = bin, color = aspect_card, y = mean), se = F)+
  scale_color_manual(name = "Aspect", values = c(North = "#440154FF", East = "#31688EFF", South = "#35B779FF", 
                                                 West = "orange"),
                     labels = c("North", "East", "South", "West")) +
  scale_fill_manual(name = "Aspect", values = c(North = "#440154FF", East = "#31688EFF", South = "#35B779FF", 
                                                West = "orange")) +
  # scale_color_viridis(name = "Aspect", labels = c("North", "East", "South", "West", "X = Y"), discrete = T) +
  facet_wrap(~wind_slope, ncol = 2) +
  xlab("Distance from intact forest (m)") +
  ylab(bquote("Change in regeneration " (ha^-1))) +
  scale_y_continuous(breaks = seq(-500, 2500, by = 500)) +
  theme_classic() +
  theme(text = element_text(size=24)) #axis.title.y=element_blank(), axis.text.y = element_blank(),

tag_facet <- function(p, open = "", close = ")", tag_pool = letters, x = Inf, y = Inf, 
                      hjust = 1.5, vjust = 1.5, fontface = 2, size = 6, family = "", ...) {
  
  gb <- ggplot_build(p)
  lay <- gb$layout$layout
  tags <- cbind(lay, label = paste0(open, tag_pool[lay$PANEL], close), x = x, y = y)
  p + geom_text(data = tags, aes_string(x = "x", y = "y", label = "label"), ..., hjust = hjust, 
                vjust = vjust, fontface = fontface, family = family, inherit.aes = FALSE, size = size) 
}

tag_facet(fig_4)

