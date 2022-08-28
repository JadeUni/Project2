Potential_New_Resistance <- readRDS("Potential_New_Resistance_TM.rds")
Testing_Dataset_Outcome_Trimethoprim <- write.csv(Potential_New_Resistance, "Trimethoprim_2019_Test_Outcome.csv")

Potential_New_Resistance <- readRDS("Potential_New_Resistance_TC.rds")
Testing_Dataset_Outcome_Trimethoprim <- write.csv(Potential_New_Resistance, "Tetracyclines_2019_Test_Outcome.csv")

Potential_New_Resistance <- readRDS("Potential_New_Resistance_SP.rds")
Testing_Dataset_Outcome_Trimethoprim <- write.csv(Potential_New_Resistance, "Sulphonamides_2019_Test_Outcome.csv")

Potential_New_Resistance <- readRDS("Potential_New_Resistance_Lactamase.rds")
Testing_Dataset_Outcome_Trimethoprim <- write.csv(Potential_New_Resistance, "Lactamase_2019_Test_Outcome.csv")

Potential_New_Resistance <- readRDS("Potential_New_Resistance_FM.rds")
Testing_Dataset_Outcome_Trimethoprim <- write.csv(Potential_New_Resistance, "Fosfomycin_2019_Test_Outcome.csv")

Potential_New_Resistance <- readRDS("Potential_New_Resistance_FQ.rds")
Testing_Dataset_Outcome_Trimethoprim <- write.csv(Potential_New_Resistance, "Fluoroquinolones_2019_Test_Outcome.csv")

Potential_New_Resistance <- readRDS("Potential_New_Resistance_CP.rds")
Testing_Dataset_Outcome_Trimethoprim <- write.csv(Potential_New_Resistance, "Chloramphenicol_2019_Test_Outcome.csv")



