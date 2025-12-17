install.packages("reticulate")
library(reticulate)
use_condaenv("base", required = TRUE)
reticulate::use_condaenv("base", conda = "C:/Users/PC/anaconda3/Scripts/conda.exe",required = TRUE)##
optuna <- import("optuna")
optuna <- reticulate::import("optuna")##
print("Kết nối Optuna thành công!")##
reticulate::py_get_attr(optuna, "__version__")##
py_config()