# Replaces blanks with NA in the main dataframe
dados_clean = read.csv("dados.csv")  # read csv file

dados_clean[dados_clean == ""] <- NA  # Replace blank by NA

save(dados_clean,file="dados_NA.Rda") # save the data
