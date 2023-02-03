# Pesquisa-Liberdade-Expressao
Fisher’s exact test and Chi-Square test

Survey data: dados.csv
University demographics data: Idade_Pop.csv, Sexo_Pop.csv

Clean blanks.R replaces blank cells with NA and then saves the clean data as dados_NA.Rda (run this first)

RepresentatividadeAmostra.R performs a one-sample Chi-Square test to see if sample proportions are not significantly different than population proportions for select demographics.

Scripts Class.R, DenomPolitica.R, Idade.R, Orisex.R, Raca.R, Religiao.R, Sexo.R, areadosaber.R can be run in any order and perform Fisher's exact test to see if the categories in each of these demographic variables are independent of reluctance of expression in seven different topics: politics, race, sex, gender, religion, university administration and syllabus content.
