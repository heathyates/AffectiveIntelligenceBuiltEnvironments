##################################################################################
#Heath Yates                                                                     #
#Date: March 31, 2018                                                            # 
#Description: Mann-Whitney test to compare LR against others                     #
##################################################################################



#Step 0: Define the accuracy of logistic, rf, svm, and mlp 
logisticAccuracy <- c(0.758565737, 0.758293839, 0.794117647, 0.793853073, 0.738738739, 0.802010828, 0.753645434, 0.669242658,
                      0.741648107) 

rfAccuracy <- c(0.787250996, 0.813586098, 0.842647059, 0.8005997, 0.701064701, 0.814385151, 0.780506523, 0.720247295, 0.751299183)


svmAccuracy <- c(0.46374502, 0.547393365, 0.681617647, 0.673163418, 0.66011466, 0.784996133, 0.428242517, 0.377897991, 
                 0.634743875)


mlpAccuracy <- c(0.74501992, 0.801737757, 0.807352941, 0.787856072, 0.774774775, 0.856148492, 0.804297774, 0.714837713, 0.740163326)
 



naiveNAccuracy <- c(0.46374502, 0.52685624, 0.607352941, 0.592953523, 0.622440622, 0.610208817, 0.428242517, 0.377897991, 0.634743875)

naivePAccuracy <- c(0.53625498, 0.47314376, 0.392647059, 0.407046477, 0.377559378, 0.389791183, 0.571757483, 0.622102009, 0.365256125)



#Step 2: Conduct a test Man-Whitney tests comparing logistic to other algorithms 
wilcox.test(logisticAccuracy, rfAccuracy) #0.3401
wilcox.test(logisticAccuracy, svmAccuracy) #0.002756
wilcox.test(jitter(logisticAccuracy), mlpAccuracy) #0.2973 
wilcox.test(logisticAccuracy, naiveNAccuracy) #4.114e-5 => 0
wilcox.test(logisticAccuracy, naivePAccuracy) #4.114e-5 => 0
