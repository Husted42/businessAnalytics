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


TV

vars <- c("TVset", "Assessor", "Repeat", "Picture")

par(mfrow = c(2, 2))

for (v in vars) {
  formula <- as.formula(paste("Coloursaturation ~", v))
  
  boxplot(formula, data = TV,
          main = paste("Colour Saturation by", v),
          xlab = v,
          ylab = "Colour Saturation",
          col = "lightblue")
  
  means <- tapply(TV$Coloursaturation, TV$TVset, mean)
  points(1:length(means), means, pch = 19, col = "black")
  
}

par(mfrow = c(1, 1))

# All pairwise interaction plots among these factors
vars <- c("TVset", "Repeat", "Picture")

op <- par(mfrow = c(2, 3), mar = c(4, 4, 3, 1))

pairs <- combn(vars, 2, simplify = FALSE)
for (p in pairs) {
  xfac <- p[[1]]
  trc  <- p[[2]]
  interaction.plot(
    x.factor   = TV[[xfac]],
    trace.factor = TV[[trc]],
    response   = TV$Coloursaturation,
    fun        = mean,
    type       = "b",
    pch        = 19,
    xlab       = xfac,
    ylab       = "Colour Saturation (mean)",
    legend     = TRUE,
    main       = paste("Interaction:", xfac, "×", trc)
  )
} 