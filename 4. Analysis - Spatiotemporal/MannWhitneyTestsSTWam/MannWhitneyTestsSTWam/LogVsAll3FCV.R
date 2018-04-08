##################################################################################
#Heath Yates                                                                     #
#Date: March 31, 2018                                                            # 
#Description: Mann-Whitney test to compare LR against others                     #
##################################################################################



#Step 0: Define the accuracy of logistic, rf, svm, and mlp 
logisticAccuracy <- c(0.747096093, 0.786612984, 0.789153573, 0.756436663, 0.783315845, 0.760858189)
rfAccuracy <- c(0.787250996, 0.813586098, 0.842647059, 0.8005997, 0.701064701, 0.814385151, 0.780506523, 0.720247295, 0.751299183)
svmAccuracy <- c(0.694820717, 0.740126382, 0.717647059, 0.671664168, 0.625716626, 0.702242846, 0.676899463, 0.658423493, 0.700074239)
mlpAccuracy <- c(0.74501992, 0.801737757, 0.807352941, 0.787856072, 0.774774775, 0.856148492, 0.804297774, 0.714837713, 0.740163326)
naiveNAccuracy <- c(0.46374502, 0.52685624, 0.607352941, 0.592953523, 0.622440622, 0.610208817, 0.428242517, 0.377897991, 0.634743875)
naivePAccuracy <- c(0.53625498, 0.47314376, 0.392647059, 0.407046477, 0.377559378, 0.389791183, 0.571757483, 0.622102009, 0.365256125)


#Step 2: Conduct a test Man-Whitney tests comparing logistic to other algorithms 
wilcox.test(logisticAccuracy, rfAccuracy) #0.52870
wilcox.test(logisticAccuracy, svmAccuracy) #0.0003996
wilcox.test(jitter(logisticAccuracy), mlpAccuracy) #0.6070
wilcox.test(logisticAccuracy, naiveNAccuracy) #0.0003996
wilcox.test(logisticAccuracy, naivePAccuracy) #0.0003996
