
## Testing for Cage and Genotype Effects
# Creates table for each

library(dplyr)
library(emmeans)

response_data <- read.csv("data/T1_response_data.csv")
explanatory_variables <- read.csv("data/explanatory_variables.csv")
all_nut <- read.csv("data/All_Nutrients_Processed.csv")


model_data <- left_join(response_data, explanatory_variables, by = join_by(CowTagID))

### Cage Effect
species <- unique(model_data$Species)
responses <- c("Percent_Change", "Chl_ug.cm.2", "FSC.Events_per_cm_2")
cage_models <- list()
cage_models_table <- data.frame()

for (sp in species) {
  for (response in responses) {
    formula <- as.formula(paste(response, "~ Cage_Uncaged"))
    model <- lm(formula = formula, data = model_data %>% filter(Species == sp))
    cage_models[[response]] <- model
    anova_results <- anova(model)
    p_value <- anova_results$`Pr(>F)`[1]
    lm_models_table_bind <- data.frame(Species = sp, response_variable = response, p_value = p_value)
    cage_models_table <- rbind(cage_models_table, lm_models_table_bind)
}
}

cage_models_table

### Genotype Effect
species <- unique(model_data$Species)
responses <- c("Percent_Change", "Chl_ug.cm.2", "FSC.Events_per_cm_2")
genotype_models <- list()
genotype_models_table <- data.frame()

for (sp in species) {
  genotype_models[[sp]] <- list()
  for (response in responses) {
    formula <- as.formula(paste(response, "~ Genotype"))
    model <- lm(formula = formula, data = model_data %>% filter(Species == sp))
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
