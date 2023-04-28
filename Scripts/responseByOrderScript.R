library(dplyr)
library(lme4)
library(tidyr)
library(stringr)
library(ggplot2)
library(lmerTest)
library(sjPlot)
library(MuMIn)


#Script to run continued models
mdf <- read.csv("data/modelData_allSpecies.csv")

mdf_scaled <- mdf %>% 
    mutate(Dev_1 = scale(Dev_1),
           Dev_10 = scale(Dev_10),
           temp_niche = scale(temp_niche),
           meanLight = scale(log(meanLight + 1)),
           mean_temp = scale((mean_temp - 1.02) * -1),
           BodySize = scale(BodySize))

traits <- read.csv("data/TraitData.csv")

mdf_scaled <- mdf_scaled %>% left_join(traits, by = c("scientific_name" = "Species"))

ggplot(mdf_scaled, mapping = aes(x = Dev_10, y = abundance, color = scientific_name, fill = scientific_name)) +
    geom_point() +
    geom_smooth(method = "lm", alpha = 0.15, linewidth = 0.4) +
    geom_text(mapping = aes(label = scientific_name)) +
    theme_classic() +
    theme(legend.position = "none") +
    scale_color_manual(values = rep("black", 43)) +
    scale_fill_manual(values = rep("black", 43)) +
    facet_wrap(~Order, scales = "free") 

ggsave(filename = "Figures/responseByOrder.png", width = 12, height = 6)

ggplot(mdf_scaled, mapping = aes(x = Dev_10, y = abundance, color = scientific_name, fill = scientific_name)) +
    geom_point() +
    geom_smooth(method = "lm") +
    theme_bw() +
    theme(legend.position = "none") +
    scale_y_log10() +
    facet_wrap(~NativeStatus, scales = "free") 

head(mdf_scaled)


model<- glmer.nb(formula = abundance ~ Dev_10 +
                               (1 | scientific_name),
                           na.action = "na.fail", family = poisson,
                           control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),
                           data = mdf_scaled)
summary(model)

model_order <- glmer.nb(formula = abundance ~ Dev_10 * Order +
                     (1 | scientific_name),
                 na.action = "na.fail", family = poisson,
                 control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),
                 data = mdf_scaled)
summary(model_order)

plot_model(model = model, terms = c("Dev_10", "Order"), type = "pred") +
    scale_y_continuous(limits = c(0, 10))


model_nativeStatus <- glmer.nb(formula = abundance ~ Dev_10 * NativeStatus +
                            (1 | scientific_name),
                        na.action = "na.fail", family = poisson,
                        control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),
                        data = mdf_scaled)
summary(model_nativeStatus)
plot_model(model = model_nativeStatus, terms = c("Dev_10", "NativeStatus"), type = "pred") 

