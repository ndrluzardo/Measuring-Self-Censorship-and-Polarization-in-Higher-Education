# Tests for representativeness of sample using one-sample Chi-Square and data from
# population provided by the university. 
# Tests for age and sex (study area has cells with only 1 element).

# creates dataframes with university demographics
Sexo_pop <- read.csv('Sexo_Pop.csv')
Idade_pop <- read.csv('Idade_Pop.csv')

# loads the dataframe "dados_clean"
load("dados_NA.Rda")

# turns 'Prefiro nao responder' to NA
dados_clean$sexo[dados_clean$sexo=="Prefiro não responder"]<-NA

# creates a table with sample sex data
Sexo_samp <- table(dados_clean$sexo)

# Chi Squared for Sex
# Creates the vector of probabilities for sex
Sexo <- c(Sexo_pop$População[Sexo_pop$Sexo=='Feminino'],Sexo_pop$População[Sexo_pop$Sexo=='Masculino'])
Sexo_p <- Sexo/sum(Sexo)

# Applies the test
Sexo_test <- chisq.test(Sexo_samp,Sexo_p,simulate.p.value = TRUE, B = 10000)
Sexo_test

# Chi Squared for Age
# categorize the ages in the sample (creates a factor)
Idade_cat = cut(dados_clean$idade, c(16, 18, 25, 30, 40, Inf), c("16-18", "19-25", "26-30","31-40", ">40"), include.lowest=TRUE)

# creates a table with sample age data
Idade_samp <- table(Idade_cat)

# Creates the vector of probabilities for sex
Idade <- Idade_pop$População
Idade_p <- Idade/sum(Idade)

# Applies the test
Idade_test <- chisq.test(Idade_samp,Idade_p)
Idade_test
