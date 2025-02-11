###### Creating Explanatory Variables Sheet

all_nut <- read.csv("data/All_Nutrients_Processed.csv") %>% 
  filter(!CowTagID == "VSEEP") %>% 
  mutate(Pin_Number = as.numeric(str_sub(all_nut$CowTagID,2)))

depth_dist <- read.csv("data/distance_depth.csv") %>% 
  filter(!Location == "Cabral") %>% 
  filter(!CowTagID == "VSEEP")

explanatory_variables_all <- left_join(all_nut, depth_dist, by = "CowTagID")
write_csv(explanatory_variables_all, "data/explanatory_variables_all.csv")
