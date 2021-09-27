


library(stats)
library(tidyverse)

library("readxl")


data <- read_excel("data/sample_frame_csa/selection.xlsx")




t.test(NHH ~ CC_Prgram, data = data, var.equal = TRUE)

# result: p > .05 so not statistically different

# Two Sample t-test
# 
# data:  NHH by CC_Prgram
# t = 1.5296, df = 168, p-value = 0.128
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -2.357643 18.579367
# sample estimates:
#   mean in group No mean in group Yes 
# 136.5319          128.4211 

# same for rural/urban

t.test(`Ur/Ru` ~ CC_Prgram, data = data, var.equal = TRUE)








#############
# Wilcox test - same but not for parametric data
# result: p > .05 so not statistically different

wilcox.test(NHH ~ CC_Prgram, data = data,
            exact = FALSE)

# Wilcoxon rank sum test with continuity correction
# 
# data:  NHH by CC_Prgram
# W = 3966, p-value = 0.2174
# alternative hypothesis: true location shift is not equal to 0
