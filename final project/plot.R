library(ggplot2)
library(tidyr)

# oringinal model
# loss is manually coded because it has the same name and the model is not fitted again
accuracy
# 0.9186
loss <- 0.1872


# l1
accuracy_l1_kernel
# [1]  0.87716120 0.87994981 0.75920248 0.02007808 0.02007808
loss_l1_kernel <- c(0.3501,0.4803,1.0504,3.7696,8.5653)

accuracy_l1_bias
# [1] 0.9203848 0.9223369 0.9277747 0.9189905 0.9355828
loss_l1_bias <- c(0.1710, 0.1844, 0.1700,0.1799, 0.1764)

accuracy_l1_activity
# [1] 0.92805356 0.92916900 0.93600112 0.02007808 0.02007808
loss_l1_activity <- c(0.1543,0.1536,0.2549, 3.2475,3.3339)


accuracy_l1 <- data.frame(l1 = l1,l1_kernel = accuracy_l1_kernel,l1_bias = accuracy_l1_bias, l1_activity =accuracy_l1_activity)
loss_l1 <- data.frame(l1 = l1,l1_kernel = loss_l1_kernel,l1_bias = loss_l1_bias, l1_activity = loss_l1_activity)

accu_l1 <- accuracy_l1 %>%
  select(l1,l1_kernel,l1_bias, l1_activity) %>%
  gather(key = "regularizer", value = "accuracy", -l1)
lo_l1 <- loss_l1 %>%
  select(l1,l1_kernel,l1_bias, l1_activity) %>%
  gather(key = "regularizer", value = "loss", -l1)


ggplot(data = accu_l1, aes(x = log(l1), y = accuracy)) +
  geom_line(aes(color = regularizer))+
  geom_abline(intercept = accuracy, slope = 0, lty = 2) +
  ggtitle("accuracy vs. log(l1) for regularizers") +
  xlab("log(l1)") + ylab("accuracy on test data")

ggplot(data = lo_l1, aes(x = log(l1), y = loss)) +
  geom_line(aes(color = regularizer))+
  geom_abline(intercept = loss, slope = 0, lty = 2) +
  ggtitle("loss vs. log(l1) for regularizers") +
  xlab("log(l1)") + ylab("loss on test data")


##### l2

accuracy_l2_kernel
# [1] 0.93377024 0.83742332 0.81720579 0.84104854 0.02007808
loss_l2_kernel <- c(0.1562,0.3182,0.5075,1.1839,3.2208)

accuracy_l2_bias
# [1] 0.9277747 0.9233129 0.9254044 0.9276353 0.9266592
#loss
loss_l2_bias <- c(0.1769,0.1779,0.1365,0.1573,0.1619)
accuracy_l2_activity
# [1] 0.9344674 0.9351645 0.9431121 0.9610987 0.9313999
#loss
loss_l2_activity <- c(0.1236,0.1239,0.1252,0.1872,0.3767)

accuracy_l2 <- data.frame(l2 = l2,l2_kernel = accuracy_l2_kernel,l2_bias = accuracy_l2_bias, l2_activity =accuracy_l2_activity)
loss_l2 <- data.frame(l2 = l2,l2_kernel = loss_l2_kernel,l2_bias = loss_l2_bias, l2_activity = loss_l2_activity)

accu_l2 <- accuracy_l2 %>%
  select(l2,l2_kernel,l2_bias, l2_activity) %>%
  gather(key = "regularizer", value = "accuracy", -l2)
lo_l2 <- loss_l2 %>%
  select(l2,l2_kernel,l2_bias, l2_activity) %>%
  gather(key = "regularizer", value = "loss", -l2)

ggplot(data = accu_l2, aes(x = log(l2), y = accuracy)) +
  geom_line(aes(color = regularizer))+
  geom_abline(intercept = accuracy, slope = 0, lty = 2) +
  ggtitle("accuracy vs. log(l2) for regularizers") +
  xlab("log(l2)") + ylab("accuracy on test data")

ggplot(data = lo_l2, aes(x = log(l2), y = loss)) +
  geom_line(aes(color = regularizer))+
  geom_abline(intercept = loss, slope = 0, lty = 2) +
  ggtitle("loss vs. log(l2) for regularizers") +
  xlab("log(l2)") + ylab("loss on test data")


### l1l2
accuracy_l1l2_kernel
#[1] 0.88789737 0.92275518 0.89500839 0.85206360 0.02007808 0.91313440 0.90616286 0.87395430 0.83463466 0.03722811 0.62353599 0.54489684
#[13] 0.60694367 0.45566091 0.02007808 0.02007808 0.02007808 0.02007808 0.02007808 0.02007808 0.02007808 0.02007808 0.02007808 0.02007808
#[25] 0.02007808

# loss
loss_l1l2_kernel <- c(0.3399,0.2398,0.4914,1.2699,3.2324,
  0.3993,0.3912,0.5290,1.1830,3.2556,
  2.1049,1.3591,1.3272,1.7297,3.2972,
  3.7686,3.7674,3.7555,3.7776,3.7937,
  8.5710,8.5793,8.5603,8.4378,8.6505)

accuracy_l1l2_bias
#[1] 0.9056051 0.9401841 0.9205242 0.9203848 0.9090909 0.9227552 0.9079754 0.9348857 0.9354434 0.9135527 0.9192694 0.9162019 0.9281930
#[14] 0.9281930 0.9104852 0.9224763 0.9148076 0.9104852 0.9273564 0.9184328 0.9259621 0.9153653 0.9134133 0.9185722 0.9160625
loss_l1l2_bias <- c(0.2151,0.1390,0.1727,0.1559,0.2069,
  0.2118,0.1885,0.1521,0.1494,0.1690,
  0.1593,0.1800,0.1695,0.1699,0.2112,
  0.1810,0.2116,0.2062,0.1658,0.1917,
  0.1762,0.2392,0.2086,0.2049,0.2138)

accuracy_l1l2_activity
# [1] 0.92902958 0.93725598 0.94715559 0.95022309 0.94408810 0.94617957 0.94924706 0.95064139 0.94590074 0.94088119 0.94116008 0.94171780
#[13] 0.95482433 0.94701618 0.93753487 0.02007808 0.02007808 0.02007808 0.02007808 0.02007808 0.02007808 0.02007808 0.02007808 0.02007808
#[25] 0.02007808
#loss_l1l2_activity <- 
#c(0.2060658, 0.2053927, 0.1812005, 0.2450324, 0.4619605,
#  0.1981976, 0.1913493, 0.1987953, 0.2412422, 0.4312160,
#  0.2550016, 0.2781447, 0.2222318, 0.2847969, 0.4631776,
#  3.2205356, 3.2201279, 3.2198598, 3.2189500, 3.2235874,
#  3.3102249, 3.3095958, 3.3101640, 3.3095292, 3.3140455)

loss_l1l2_activity <- 
  c(0.1154, 0.1197, 0.1040, 0.1682, 0.3966,
    0.1402, 0.1255, 0.1421, 0.1706, 0.4090,
    0.1777, 0.1970, 0.1993, 0.2309, 0.3630,
    3.2491, 3.2477, 3.2452, 3.2427, 3.2479,
    3.3350, 3.3333, 3.3343, 3.3333, 3.3393)

accuracy_l1l2 <- data.frame(l1 = l1seq, l2 = l2seq,l1l2_kernel = accuracy_l1l2_kernel,
                            l1l2_bias = accuracy_l1l2_bias, l1l2_activity =accuracy_l1l2_activity)
loss_l1l2 <- data.frame(l1 = l1seq, l2 = l2seq,l1l2_kernel = loss_l1l2_kernel,
                        l1l2_bias = loss_l1l2_bias, l1l2_activity = loss_l1l2_activity)

accu_l1l2 <- accuracy_l1l2 %>%
  select(l1,l2,l1l2_kernel,l1l2_bias, l1l2_activity) %>%
  gather(key = "regularizer", value = "accuracy", -l1,-l2)
lo_l1l2 <- loss_l1l2 %>%
  select(l1,l2,l1l2_kernel,l1l2_bias, l1l2_activity) %>%
  gather(key = "regularizer", value = "loss",-l1,-l2)

accu_l1l2_select <- accu_l1l2 %>%
  filter(l1 == l2) %>%
  mutate(l = l1) %>%
  select(l,accuracy,regularizer)

lo_l1l2_select <- lo_l1l2 %>%
  filter(l1 == l2) %>%
  mutate(l = l1) %>%
  select(l,loss,regularizer)

ggplot(data = accu_l1l2, aes(x = as.factor(round(log(l1), digits = 2)), y = accuracy)) +
  geom_point(aes(color = regularizer, shape = as.factor(l2)))+
  geom_abline(intercept = accuracy, slope = 0, lty = 2) +
  ggtitle("accuracy vs. log(l1) for elastic net regularizers") +
  xlab("log(l1)") + ylab("accuracy on test data") + labs(shape = "l2") +
  theme(legend.title = element_text(size = 8),legend.text = element_text(size = 8))

ggplot(data = lo_l1l2, aes(x = as.factor(round(log(l1), digits = 2)), y = loss)) +
  geom_point(aes(color = regularizer, shape = as.factor(l2)))+
  geom_abline(intercept = loss, slope = 0, lty = 2) +
  ggtitle("loss vs. log(l1) for elastic net regularizers") +
  xlab("log(l1)") + ylab("loss on test data") + labs(shape = "l2") +
  theme(legend.title = element_text(size = 8),legend.text = element_text(size = 8))

ggplot(data = accu_l1l2, aes(x = as.factor(round(log(l2), digits = 2)), y = accuracy)) +
  geom_point(aes(color = regularizer, shape = as.factor(l2)))+
  geom_abline(intercept = accuracy, slope = 0, lty = 2) +
  ggtitle("accuracy vs. log(l2) for elastic net regularizers") +
  xlab("log(l2)") + ylab("accuracy on test data") + labs(shape = "l1") +
  theme(legend.title = element_text(size = 8),legend.text = element_text(size = 8))

ggplot(data = lo_l1l2, aes(x = as.factor(round(log(l2), digits = 2)), y = loss)) +
  geom_point(aes(color = regularizer, shape = as.factor(l2)))+
  geom_abline(intercept = loss, slope = 0, lty = 2) +
  ggtitle("loss vs. log(l2) for elastic net regularizers") +
  xlab("log(l2)") + ylab("loss on test data") + labs(shape = "l1") +
  theme(legend.title = element_text(size = 8),legend.text = element_text(size = 8))

ggplot(data = accu_l1l2_select, aes(x = log(l), y = accuracy)) +
  geom_line(aes(color = regularizer))+
  geom_abline(intercept = accuracy, slope = 0, lty = 2) +
  ggtitle("accuracy vs. log(l1l2) for regularizers") +
  xlab("log(l1l2)") + ylab("accuracy on test data")

ggplot(data = lo_l1l2_select, aes(x = log(l), y = loss)) +
  geom_line(aes(color = regularizer))+
  geom_abline(intercept = loss, slope = 0, lty = 2) +
  ggtitle("loss vs. log(l1l2) for regularizers") +
  xlab("log(l1l2)") + ylab("loss on test data")


