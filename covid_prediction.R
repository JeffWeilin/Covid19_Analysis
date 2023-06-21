library(readr)

pth = "https://raw.github.sydney.edu.au/thol6150/data3888c2/master/covidC2DataFinal.csv?token=AAABQQGMWSUPADVARQGKMNLCRW3HO"

data = read_csv(pth)

modello = lm(overallScore ~ population + human_development_index, data = data)

v = as.vector(modello$coefficients)
wakanda = c(120000, 0.67)
wakanda_prediction = v[1] + v[2] * wakanda[1] + v[3] * wakanda[2]

aus = c(24000000, 0.944)
aus_prediction = v[1] + v[2] * aus[1] + v[3] * aus[2]
