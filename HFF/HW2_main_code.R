rm(list = ls())
install.packages('GARCHIto')
library(GARCHIto)
require(GARCHIto) 

setwd('/Users/imchaebin/Desktop/JupyterNotebookFiles/HFF/')
data <- read.csv('data_HW2_preprocessed.csv', head = T)


# Unified GARCH-Ito
Unified_GARCH_Ito_pred = c()
Unified_GARCH_Ito_pred_error = 0
for(i in 81:nrow(data)){
  pred = UnifiedEst(data$realized_vol[(i-80):(i-1)], data$daily_ret[(i-80):(i-1)])$pred
  Unified_GARCH_Ito_pred =c(Unified_GARCH_Ito_pred, pred)
  Unified_GARCH_Ito_pred_error = Unified_GARCH_Ito_pred_error + (pred-data$daily_ret[i])^2
}
Unified_GARCH_Ito_pred_error = Unified_GARCH_Ito_pred_error / (nrow(data)-80)


# Realized GARCH-Ito
Realized_Ito_pred = c()
Realized_Ito_pred_error = 0
for(i in 81:nrow(data)){
  pred = RealizedEst(data$realized_vol[(i-80):(i-1)])$pred
  Realized_Ito_pred = c(Realized_Ito_pred, pred)
  Realized_Ito_pred_error = Realized_Ito_pred_error + (pred-data$daily_ret[i])^2
}
Realized_Ito_pred_error = Realized_Ito_pred_error / (nrow(data)-80)

Unified_GARCH_Ito_pred_error
Realized_Ito_pred_error

df = data.frame(Unified_GARCH_Ito_pred, Realized_Ito_pred)
df
write.csv(df, 'GARCH_Itos.csv', row.names = FALSE)








