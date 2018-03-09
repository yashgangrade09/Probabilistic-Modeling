source("BayesianNetworks-template.r");

file = read.csv("RiskFactors.csv", header = TRUE);
#defining in order of the definitions
variables = c("income", "exercise", "smoke", "bmi", "bp", "cholesterol", "angina", "stroke", "attack", "diabetes");

#numbering
#-------------------------------------------------------
#1: income
#2: exercise
#3: smoke
#4: bmi
#5: bp
#6: cholesterol
#7: angina
#8: stroke
#9: attack
#10:diabetes
#-------------------------------------------------------

#using level order traversal to populate individual conditional probability tables
income = createCPTfromData(x = file, varnames = c("income"));
smoke_income = createCPTfromData(x = file[, c(3,1)], varnames = c("smoke", "income"));
bmi_income_excercise = createCPTfromData(x = file[, c(4,1,2)], varnames = c("bmi", "income", "exercise"));
exercise_income = createCPTfromData(x = file[, c(2,1)], varnames = c("exercise", "income"));
bp_income_exercise_smoke = createCPTfromData(x = file[, c(5,1,2,3)], varnames = c("bp", "income", "exercise", "smoke"));
cholesterol_income_exercise_smoke = createCPTfromData(x = file[, c(6,1,2,3)], varnames = c("cholesterol", "income", "exercise", "smoke"));
diabetes_bmi = createCPTfromData(x = file[, c(10,4)], varnames = c("diabetes","bmi"));
stroke_bmi_bp_cholesterol = createCPTfromData(x = file[, c(8,4,5,6)], varnames = c("stroke", "bmi", "bp", "cholesterol"));
attack_bmi_bp_cholesterol = createCPTfromData(x = file[, c(9,4,5,6)], varnames = c("attack", "bmi", "bp", "cholesterol"));
angina_bmi_bp_cholesterol = createCPTfromData(x = file[, c(7,4,5,6)], varnames = c("angina", "bmi", "bp", "cholesterol"));

#create bayesnet with number to name mapping
bayesnet = list("1" = income, "2" = smoke_income, "3" = bmi_income_excercise, "4" = exercise_income, "5" = bp_income_exercise_smoke, 
                "6" = cholesterol_income_exercise_smoke, "7" = diabetes_bmi, "8" = stroke_bmi_bp_cholesterol, "9" = attack_bmi_bp_cholesterol, 
                "10" = angina_bmi_bp_cholesterol);

l = 0;
for(i in 1:10){
  l = l + nrow(bayesnet[[i]]);
}
#(bayesnet[[2]])
#((nrow(bayesnet[[2]])))
(l)
