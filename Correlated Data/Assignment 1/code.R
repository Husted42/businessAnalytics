setwd("C:/Users/huste/Documents/Github/businessAnalytics/Correlated Data/Assignment 1")
getwd()

list.files()

TV <- read.table("TV2.txt", header = 1)

par(mfrow = c(2, 2))  # 2x2 grid of plots

for (v in vars) {
  formula <- as.formula(paste("Coloursaturation ~", v))
  
  boxplot(formula, data = TV,
          main = paste("Colour Saturation by", v),
          xlab = v,
          ylab = "Colour Saturation",
          col = "lightblue")
}

par(mfrow = c(1, 1))  # reset plotting layout
