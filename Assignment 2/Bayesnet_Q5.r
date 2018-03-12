source("BayesianNetworks-template.r");

file = read.csv("RiskFactors.csv", header = TRUE);
#defining in order of the definitions
variables = c("income", "exercise", "smoke", "bmi", "bp", "cholesterol", "angina", "stroke", "attack", "diabetes");

# Indexing used to populate different probability tables. Kind of like a map from integer to string. 
# 
#1 -> income
#2 -> exercise
#3 -> smoke
#4 -> bmi
#5 -> bp
#6 -> cholesterol
#7 -> angina
#8 -> stroke
#9 -> attack
#10 ->diabetes
#

# Using level order traversal to populate individual conditional probability tables.
#
# Naming Convention:
# Example: Variables are A, B, C.
# 1) A = createCPTfromData(...) -> creates the probability distribution of A from the given csv file
# 2) A_B = createCPTfromData(...) -> creates the probability distribution of A|B from the given csv file
# 3) A_B_C = createCPTfromData(...) -> creates the probability distribution of A|B,C from the given csv file
# And as follows
# 

income = createCPTfromData(x = file, varnames = c("income"));
smoke_income = createCPTfromData(x = file[, c(3,1)], varnames = c("smoke", "income"));
bmi_income_excercise = createCPTfromData(x = file[, c(4,1,2)], varnames = c("bmi", "income", "exercise"));
exercise_income = createCPTfromData(x = file[, c(2,1)], varnames = c("exercise", "income"));
bp_income_exercise_smoke = createCPTfromData(x = file[, c(5,1,2,3)], varnames = c("bp", "income", "exercise", "smoke"));
cholesterol_income_exercise_smoke = createCPTfromData(x = file[, c(6,1,2,3)], varnames = c("cholesterol", "income", "exercise", "smoke"));
diabetes_bmi_exercise_smoke = createCPTfromData(x = file[, c(10,4,2,3)], varnames = c("diabetes","bmi", "exercise", "smoke"));
stroke_bmi_bp_cholesterol_exercise_smoke_diabetes = createCPTfromData(x = file[, c(8,4,5,6,2,3,10)], varnames = c("stroke", "bmi", "bp", "cholesterol", "exercise", "smoke", "diabetes"));
attack_bmi_bp_cholesterol_exercise_smoke = createCPTfromData(x = file[, c(9,4,5,6,2,3)], varnames = c("attack", "bmi", "bp", "cholesterol", "exercise", "smoke"));
angina_bmi_bp_cholesterol_exercise_smoke = createCPTfromData(x = file[, c(7,4,5,6,2,3)], varnames = c("angina", "bmi", "bp", "cholesterol", "exercise", "smoke"));

#create bayesnet with number to name mapping
bayesnet = list("1" = income, "2" = smoke_income, "3" = bmi_income_excercise, "4" = exercise_income, "5" = bp_income_exercise_smoke, 
                "6" = cholesterol_income_exercise_smoke, "7" = diabetes_bmi_exercise_smoke, "8" = stroke_bmi_bp_cholesterol_exercise_smoke_diabetes, 
                "9" = attack_bmi_bp_cholesterol_exercise_smoke, "10" = angina_bmi_bp_cholesterol_exercise_smoke);
