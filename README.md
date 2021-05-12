# SAM_sAA_Cortisol
Analyses of saliva Alpha-Amylase and Cortisol in SAM study, as described in:
- Sep, M.S.C., van Ast, V.A., Gorter, R., Joëls, M., & Geuze, E. (2019) Time-dependent effects of psychosocial stress on the contextualization of neutral memories. Psychoneuroendocrinology, 108, 140–149. https://doi.org/10.1016/j.psyneuen.2019.06.021
- Sep, M.S.C., Gorter, R., van Ast, V.A., Joëls, M., & Geuze, E. (2019) No Time-Dependent Effects of Psychosocial Stress on Fear Contextualization and Generalization: A Randomized-Controlled Study With Healthy Participants. Chronic Stress, 3, 247054701989654. https://doi.org/10.1177/2470547019896547

#### Step 1: Data and Preprocessing
Datasets (will be available on Dataverse):
- List with subject numers and experimental conditions: `SAM_Experimental_Conditions.csv`
- Cortisol values: `SAM_sCortisol.csv`
- alpha-amylase values: `SAM_sAA.csv`

Data can be loaded and preprocessed for analysis using the script (in the 'R' folder):
- `Load_sAA_sCORT_Lab_Results.R`
This script returns the following R-dataset in the folder 'processed_data':
- `sAA_sCORT_condition_dataset.rds`

#### Step 2: Check Assumptions linear mixed effect models (LMM)
- `LMM_Assumptions.r`

#### Step 3: Perform LMM analyses
- `sAA_sCORT_Analysis.R`
Note, the script sources `LMM_Results_to_Word.R` and `LMM_Assumptions.R` (available in 'R' folder). The datasets `SAM_MCT.csv` or `SAM_FGT.csv` (also available from Dataverse) can be used to perform the analyses for subjects that completed the MCT or FGT tasks respectively.

#### Step 4: Export LMM results to tables and figures
- Tables: `LMM_Results_to_Word.R`
- Figures: `Saliva_Results_Design_to_Plot.R`
