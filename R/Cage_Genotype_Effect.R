
## Testing for Cage and Genotype Effects
# Creates table for each

library(dplyr)
library(lmerTest)
library(emmeans)
library(ggplot2)

response_data <- read.csv("data/response_data.csv")
explanatory_variables <- read.csv("data/explanatory_variables.csv")
all_nut <- read.csv("data/All_Nutrients_Processed.csv")


model_data <- left_join(response_data, explanatory_variables, by = join_by(CowTagID))
model_data$Genotype <- as.factor(model_data$Genotype)

### Cage Effect
species <- unique(model_data$Species)
responses <- c("Percent_Change", "Chl_ug.cm.2", "FSC.Events_per_cm_2")
cage_models <- list()
cage_models_table <- data.frame()

for (sp in species) {
  for (response in responses) {
    formula <- as.formula(paste(response, "~ Cage_Uncaged + (1|Genotype) + (1|Pin_Number)"))
    model <- lmer(formula = formula, data = model_data %>% filter(Species == sp))
    cage_models[[response]] <- model
    anova_results <- anova(model)
    p_value <- anova_results$`Pr(>F)`[1]
    lm_models_table_bind <- data.frame(Species = sp, response_variable = response, p_value = p_value)
    cage_models_table <- rbind(cage_models_table, lm_models_table_bind)
}
}

cage_models_table

## Different version
# Cage Effect
species <- unique(model_data$Species)
responses <- c("Percent_Change", "Chl_ug.cm.2", "FSC.Events_per_cm_2")
cage_models <- list()
cage_models_table <- data.frame()

for (sp in species) {
  for (response in responses) {
    formula <- as.formula(paste(response, "~ Cage_Uncaged + (1|Genotype) + (1|Pin_Number)"))
    data_sub <- model_data %>% filter(Species == sp)
    model <- lmer(formula = formula, data = data_sub)
    
    # Store model by species + response
    cage_models[[paste(sp, response, sep = "_")]] <- model
    
    anova_results <- anova(model)
    p_value <- anova_results$`Pr(>F)`[1]
    r2 <- performance::r2(model)$R2_marginal
    
    lm_models_table_bind <- data.frame(Species = sp, response_variable = response,
                                       p_value = p_value, Marginal_R2 = r2)
    cage_models_table <- rbind(cage_models_table, lm_models_table_bind)
  }
}

# Pull out model for Porites rus, total chlorophyll
chlorophyll_model <- cage_models[["Porites rus_Chl_ug.cm.2"]]

# Run pairwise emmeans comparison
pairwise_results <- emmeans(chlorophyll_model, pairwise ~ Cage_Uncaged)
summary(pairwise_results$contrasts)



### Genotype Effect
species <- unique(model_data$Species)
responses <- c("Percent_Change", "Chl_ug.cm.2", "FSC.Events_per_cm_2")
genotype_models <- list()
genotype_models_table <- data.frame()

for (sp in species) {
  genotype_models[[sp]] <- list()
  for (response in responses) {
    formula <- as.formula(paste(response, "~ Genotype + (1|Cage_Uncaged) + (1|Pin_Number)"))
    model <- lmer(formula = formula, data = model_data %>% filter(Species == sp))
    genotype_models[[sp]][[response]] <- model
    anova_results <- anova(model)
    p_value <- anova_results$`Pr(>F)`[1]
    lm_models_table_bind <- data.frame(Species = sp, response_variable = response, p_value = p_value)
    genotype_models_table <- rbind(genotype_models_table, lm_models_table_bind)
  }
}

genotype_models_table

#Since two are significant, let's look at what is different from each other:
pairwise <- emmeans(genotype_models[["Pocillopora acuta"]][["Percent_Change"]], pairwise ~ Genotype) #pull out significant model
pairwisedf <- as.data.frame(pairwise$contrasts) #make the p.values a data frame
pairwisedf %>% filter(p.value < 0.05) #filter for only the significant ones

pairwise <- emmeans(genotype_models[["Pocillopora acuta"]][["Chl_ug.cm.2"]], pairwise ~ Genotype) #pull out significant model
pairwisedf <- as.data.frame(pairwise$contrasts) #make the p.values a data frame
pairwisedf %>% filter(p.value < 0.05) #filter for only the significant ones

# For pru chla
pruchla_model <- lmer(Chl_ug.cm.2 ~ Cage_Uncaged + (1|Genotype) + (1|Pin_Number), data = model_data %>% filter(Species == "Porites rus"))
pairwise <- emmeans(pruchla_model, pairwise ~ Cage_Uncaged)
pairwise

ggplot(model_data %>% filter(Species == "Porites rus"), aes(x = Cage_Uncaged, y = Chl_ug.cm.2)) +
  geom_boxplot(fill = "lightblue", alpha = 0.6) +  # Boxplot with transparency
  geom_jitter(width = 0.2, alpha = 0.5, color = "black") +  # Add points for better visualization
  labs(x = "Cage Treatment", y = "Chlorophyll (ug/cm²)", title = "Effect of Cage Treatment on Chlorophyll") +
  theme_minimal()
