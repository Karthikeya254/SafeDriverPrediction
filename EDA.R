library(dplyr)
library(ggplot2)

#Data Processing===========================================================
train = as.data.frame(fread("data/train.csv", na.strings = c("-1", "-1.0")))

par(mfrow=c(2,4))
p1 <- train %>%
  group_by(ps_ind_06_bin, target) %>%
  count() %>%
  spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
  ) %>%
  ggplot(aes(ps_ind_06_bin, frac_claim, fill = factor(ps_ind_06_bin))) +
  geom_col() +
  theme(legend.position = "none") +
  labs(y = "Claim Rate")
p1 <- p1 + scale_fill_brewer(palette = "Paired") + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

p2 <- train %>%
  group_by(ps_ind_07_bin, target) %>%
  count() %>%
  spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
  ) %>%
  ggplot(aes(ps_ind_07_bin, frac_claim, fill = factor(ps_ind_07_bin))) +
  geom_col() +
  theme(legend.position = "none") +
  labs(y = "Claim Rate")

p2<- p2 + scale_fill_brewer(palette = "Paired") + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


p3 <- train %>%
  group_by(ps_ind_08_bin, target) %>%
  count() %>%
  spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
  ) %>%
  ggplot(aes(ps_ind_08_bin, frac_claim, fill = factor(ps_ind_08_bin))) +
  geom_col() +
  theme(legend.position = "none") +
  labs(y = "Claim Rate")
p3<- p3 + scale_fill_brewer(palette = "Paired") + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


p4 <- train %>%
  group_by(ps_ind_09_bin, target) %>%
  count() %>%
  spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
  ) %>%
  ggplot(aes(ps_ind_09_bin, frac_claim, fill = factor(ps_ind_09_bin))) +
  geom_col() +
  theme(legend.position = "none") +
  labs(y = "Claim Rate")
p4<- p4 + scale_fill_brewer(palette = "Paired") +  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


p5 <- train %>%
  group_by(ps_ind_10_bin, target) %>%
  count() %>%
  spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
  ) %>%
  ggplot(aes(ps_ind_10_bin, frac_claim, fill = factor(ps_ind_10_bin))) +
  geom_col() +
  theme(legend.position = "none") +
  labs(y = "Claim Rate")
p5<- p5 + scale_fill_brewer(palette = "Paired") +theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


p6 <- train %>%
  group_by(ps_ind_11_bin, target) %>%
  count() %>%
  spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
  ) %>%
  ggplot(aes(ps_ind_11_bin, frac_claim, fill = factor(ps_ind_11_bin))) +
  geom_col() +
  theme(legend.position = "none") +
  labs(y = "Claim Rate")
p6 <- p6 + scale_fill_brewer(palette = "Paired") + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


p7 <- train %>%
  group_by(ps_ind_12_bin, target) %>%
  count() %>%
  spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
  ) %>%
  ggplot(aes(ps_ind_12_bin, frac_claim, fill = factor(ps_ind_12_bin))) +
  geom_col() +
  theme(legend.position = "none") +
  labs(y = "Claim Rate")
p7 <- p7 + scale_fill_brewer(palette = "Paired") +theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

p8 <- train %>%
  group_by(ps_ind_13_bin, target) %>%
  count() %>%
  spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
  ) %>%
  ggplot(aes(ps_ind_13_bin, frac_claim, fill = factor(ps_ind_13_bin))) +
  geom_col() +
  theme(legend.position = "none") +
  labs(y = "Claim Rate")
p8 <- p8 + scale_fill_brewer(palette = "Paired") + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

grid.arrange(p1, p2,p3, p4, p5, p6, p7, p8, nrow=2, ncol=4)


p1 <- train %>%
  group_by(ps_ind_16_bin, target) %>%
  count() %>%
  spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
  ) %>%
  ggplot(aes(ps_ind_16_bin, frac_claim, fill = factor(ps_ind_16_bin))) +
  geom_col() +
  theme(legend.position = "none") +
  labs(y = "Claim Rate")
p1 <- p1 + scale_fill_brewer(palette = "Paired") + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


p2 <- train %>%
  group_by(ps_ind_17_bin, target) %>%
  count() %>%
  spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
  ) %>%
  ggplot(aes(ps_ind_17_bin, frac_claim, fill = factor(ps_ind_17_bin))) +
  geom_col() +
  theme(legend.position = "none") +
  labs(y = "Claim Rate")
p2 <- p2 + scale_fill_brewer(palette = "Paired") + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


p3 <- train %>%
  group_by(ps_ind_18_bin, target) %>%
  count() %>%
  spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
  ) %>%
  ggplot(aes(ps_ind_18_bin, frac_claim, fill = factor(ps_ind_18_bin))) +
  geom_col() +
  theme(legend.position = "none") +
  labs(y = "Claim Rate")
p3 <- p3 + scale_fill_brewer(palette = "Paired") + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


p4 <- train %>%
  group_by(ps_calc_15_bin, target) %>%
  count() %>%
  spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
  ) %>%
  ggplot(aes(ps_calc_15_bin, frac_claim, fill = factor(ps_calc_15_bin))) +
  geom_col() +
  theme(legend.position = "none") +
  labs(y = "Claim Rate")
p4 <- p4 + scale_fill_brewer(palette = "Paired") + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


p5 <- train %>%
  group_by(ps_calc_16_bin, target) %>%
  count() %>%
  spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
  ) %>%
  ggplot(aes(ps_calc_16_bin, frac_claim, fill = factor(ps_calc_16_bin))) +
  geom_col() +
  theme(legend.position = "none") +
  labs(y = "Claim Rate")
p5 <- p5 + scale_fill_brewer(palette = "Paired") + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


p6 <- train %>%
  group_by(ps_calc_17_bin, target) %>%
  count() %>%
  spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
  ) %>%
  ggplot(aes(ps_calc_17_bin, frac_claim, fill = factor(ps_calc_17_bin))) +
  geom_col() +
  theme(legend.position = "none") +
  labs(y = "Claim Rate")
p6 <- p6 + scale_fill_brewer(palette = "Paired") + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

p7 <- train %>%
  group_by(ps_calc_18_bin, target) %>%
  count() %>%
  spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
  ) %>%
  ggplot(aes(ps_calc_18_bin, frac_claim, fill = factor(ps_calc_18_bin))) +
  geom_col() +
  theme(legend.position = "none") +
  labs(y = "Claim Rate")
p7 <- p7 + scale_fill_brewer(palette = "Paired") + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


p8 <- train %>%
  group_by(ps_calc_19_bin, target) %>%
  count() %>%
  spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
  ) %>%
  ggplot(aes(ps_calc_19_bin, frac_claim, fill = factor(ps_calc_19_bin))) +
  geom_col() +
  theme(legend.position = "none") +
  labs(y = "Claim Rate")
p8 <- p8 + scale_fill_brewer(palette = "Paired") + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


p9 <- train %>%
  group_by(ps_calc_20_bin, target) %>%
  count() %>%
  spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
  ) %>%
  ggplot(aes(ps_calc_20_bin, frac_claim, fill = factor(ps_calc_20_bin))) +
  geom_col() +
  theme(legend.position = "none") +
  labs(y = "Claim Rate")
p9 <- p9 + scale_fill_brewer(palette = "Paired") + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
grid.arrange(p1, p2,p3, p4, p5, p6, p7, p8,p9, nrow=2, ncol=5)


##### Categorical Features
p1 <- trn_cln %>%
  group_by(ps_ind_02_cat, target) %>%
  count() %>%
  spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
  ) %>%
  ggplot(aes(reorder(ps_ind_02_cat, -frac_claim, FUN = max), frac_claim, fill = factor(ps_ind_02_cat))) +
  geom_col() +
  theme(legend.position = "none") +
  labs(x = "ps_ind_02_cat", y = "Claim Rate")
p1 <- p1 + scale_fill_brewer(palette = "Paired") + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                           panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

p2 <- trn_cln %>%
  group_by(ps_ind_04_cat, target) %>%
  count() %>%
  spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
  ) %>%
  ggplot(aes(reorder(ps_ind_04_cat, -frac_claim, FUN = max), frac_claim, fill = factor(ps_ind_04_cat))) +
  geom_col() +
  theme(legend.position = "none") +
  labs(x = "ps_ind_04_cat", y = "Claim Rate")
p2 <- p2 + scale_fill_brewer(palette = "Paired") + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

p3 <- trn_cln %>%
  group_by(ps_ind_05_cat, target) %>%
  count() %>%
  spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
  ) %>%
  ggplot(aes(reorder(ps_ind_05_cat, -frac_claim, FUN = max), frac_claim, fill = factor(ps_ind_05_cat))) +
  geom_col() +
  theme(legend.position = "none") +
  labs(x = "ps_ind_05_cat", y = "Claim Rate")
p3 <- p3 + scale_fill_brewer(palette = "Paired") + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

p4 <- trn_cln %>%
  group_by(ps_car_01_cat, target) %>%
  count() %>%
  spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
  ) %>%
  ggplot(aes(reorder(ps_car_01_cat, -frac_claim, FUN = max), frac_claim, fill = factor(ps_car_01_cat))) +
  geom_col() +
  theme(legend.position = "none") +
  labs(x = "ps_car_01_cat", y = "Claim Rate")
p4 <- p4 + scale_fill_brewer(palette = "Paired") + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

p5 <- trn_cln %>%
  group_by(ps_car_02_cat, target) %>%
  count() %>%
  spread(target, n, fill = 0) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
  ) %>%
  ggplot(aes(reorder(ps_car_02_cat, -frac_claim, FUN = max), frac_claim, fill = factor(ps_car_02_cat))) +
  geom_col() +
  theme(legend.position = "none") +
  labs(x = "ps_car_02_cat", y = "Claim Rate")
p5 <- p5 + scale_fill_brewer(palette = "Paired") + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

p6 <- trn_cln %>%
  group_by(ps_car_03_cat, target) %>%
  count() %>%
  spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
  ) %>%
  ggplot(aes(reorder(ps_car_03_cat, -frac_claim, FUN = max), frac_claim, fill = ps_car_03_cat)) +
  geom_col() +
  theme(legend.position = "none") +
  labs(x = "ps_car_03_cat", y = "Claim Rate")
p6 <- p6 + scale_fill_brewer(palette = "Paired") + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

grid.arrange(p1, p2,p3, p4, p5, nrow=2, ncol=3)

p1 <- train %>%
  group_by(ps_car_04_cat, target) %>%
  count() %>%
  spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
  ) %>%
  ggplot(aes(reorder(ps_car_04_cat, -frac_claim, FUN = max), frac_claim, fill = factor(ps_car_04_cat))) +
  geom_col() +
  theme(legend.position = "none") +
  labs(x = "ps_car_04_cat", y = "Claim Rate")
p1 <- p1 + scale_fill_brewer(palette = "Paired") + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

p2 <- trn_cln %>%
  group_by(ps_car_05_cat, target) %>%
  count() %>%
  spread(target, n, fill = 0) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
  ) %>%
  ggplot(aes(reorder(ps_car_05_cat, -frac_claim, FUN = max), frac_claim, fill = factor(ps_car_05_cat))) +
  geom_col() +
  theme(legend.position = "none") +
  labs(x = "ps_car_05_cat", y = "Claim Rate")
p2 <- p2 + scale_fill_brewer(palette = "Paired") + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

p3 <- train %>%
  group_by(ps_car_06_cat, target) %>%
  count() %>%
  spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
  ) %>%
  ggplot(aes(reorder(ps_car_06_cat, -frac_claim, FUN = max), frac_claim, fill = factor(ps_car_06_cat))) +
  geom_col() +  scale_fill_manual(values=c("dodgerblue1", "cornsilk", "coral", "cornflowerblue","deepskyblue3", "khaki4", "maroon1", "seagreen4","lightsalmon2","lightskyblue", "mediumpurple", "firebrick3", "lightblue4","goldenrod", "slateblue3", "slategray3", "skyblue2","seashell3")) +
  theme(legend.position = "none") +
  labs(x = "ps_car_06_cat", y = "Claim Rate")
p3 <- p3 +  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

p4 <- trn_cln %>%
  group_by(ps_car_07_cat, target) %>%
  count() %>%
  spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
  ) %>%
  ggplot(aes(reorder(ps_car_07_cat, -frac_claim, FUN = max), frac_claim, fill = factor(ps_car_07_cat))) +
  geom_col() +
  theme(legend.position = "none") +
  labs(x = "ps_car_07_cat", y = "Claim Rate")
p4 <- p4 + scale_fill_brewer(palette = "Paired")+ theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


p5 <- train %>%
  group_by(ps_car_08_cat, target) %>%
  count() %>%
  spread(target, n, fill = 0) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
  ) %>%
  ggplot(aes(reorder(ps_car_08_cat, -frac_claim, FUN = max), frac_claim, fill = factor(ps_car_08_cat))) +
  geom_col() +
  theme(legend.position = "none") +
  labs(x = "ps_car_08_cat", y = "Claim Rate")
p5 <- p5 + scale_fill_brewer(palette = "Paired")+ theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


p6 <- trn_cln %>%
  group_by(ps_car_09_cat, target) %>%
  count() %>%
  spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
  ) %>%
  ggplot(aes(reorder(ps_car_09_cat, -frac_claim, FUN = max), frac_claim, fill = factor(ps_car_09_cat))) +
  geom_col() +
  theme(legend.position = "none") +
  labs(x = "ps_car_09_cat", y = "Claim Rate")

p6 <- p6 + scale_fill_brewer(palette = "Paired")+ theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


p7 <- train %>%
  group_by(ps_car_10_cat, target) %>%
  count() %>%
  spread(target, n, fill = 0) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
  ) %>%
  ggplot(aes(reorder(ps_car_10_cat, -frac_claim, FUN = max), frac_claim, fill = factor(ps_car_10_cat))) +
  geom_col() +
  theme(legend.position = "none") +
  labs(x = "ps_car_10_cat", y = "Claim Rate")
p7 <- p7 + scale_fill_brewer(palette = "Paired")+ theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


p8 <- train %>%
  group_by(ps_car_11_cat, target) %>%
  count() %>%
  spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
  ) %>%
  ggplot(aes(reorder(ps_car_11_cat, -frac_claim, FUN = max), frac_claim, fill = factor(ps_car_11_cat))) +
  geom_col() +
  theme(legend.position = "none") +
  labs(x = "ps_car_11_cat", y = "Claim Rate")
p8 <- p8 + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

layout <- matrix(c(1,1,2,2,3,3,4,4,5,5,6,6),3,4,byrow=TRUE)
multiplot(p1, p3,p4, p5, p6, p7 ,layout=layout)

####Integer Features###
p1 <- train %>%
  group_by(ps_ind_01, target) %>%
  count() %>%
  spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
  ) %>%
  ggplot(aes(ps_ind_01, frac_claim)) +
  geom_point(color = "steelblue4", size = 3) +
  geom_line(aes(ps_ind_01, frac_claim), col = 'cyan', size = 1) +
  theme(legend.position = "none") +
  labs(x = "ps_ind_01", y = "Claim Rate")
p1 <- p1+theme(panel.border = element_blank(), panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


p2 <- train %>%
  group_by(ps_ind_03, target) %>%
  count() %>%
  spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
  ) %>%
  ggplot(aes(ps_ind_03, frac_claim)) +
  geom_point(color = "steelblue4", size = 3) +
  geom_line(aes(ps_ind_03, frac_claim), col = 'cyan', size = 1) +
  theme(legend.position = "none") +
  labs(x = "ps_ind_03", y = "Claim Rate")
p2 <- p2+theme(panel.border = element_blank(), panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

p3 <- train %>%
  group_by(ps_ind_14, target) %>%
  count() %>%
  spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
  ) %>%
  ggplot(aes(ps_ind_14, frac_claim)) +
  geom_point(color = "steelblue4", size = 3) +
  geom_line(aes(ps_ind_14, frac_claim), col = 'cyan', size = 1) +
  theme(legend.position = "none") +
  labs(x = "ps_ind_14", y = "Claim Rate")

p3 <- p3+theme(panel.border = element_blank(), panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

p4 <- train %>%
  group_by(ps_ind_15, target) %>%
  count() %>%
  spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
  ) %>%
  ggplot(aes(ps_ind_15, frac_claim)) +
  geom_point(color = "steelblue4", size = 3) +
  geom_line(aes(ps_ind_15, frac_claim), col = 'cyan', size = 1) +
  theme(legend.position = "none") +
  labs(x = "ps_ind_15", y = "Claim Rate")

p4 <- p4+theme(panel.border = element_blank(), panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

p5 <- train %>%
  filter(!is.na(ps_car_11)) %>%
  group_by(ps_car_11, target) %>%
  count() %>%
  spread(target, n, fill = 0) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
  ) %>%
  ggplot(aes(ps_car_11, frac_claim)) +
  geom_point(color = "steelblue4", size = 3) +
  geom_line(aes(ps_car_11, frac_claim), col = 'cyan', size = 1) +
  theme(legend.position = "none") +
  labs(x = "ps_car_11", y = "Claim Rate")

p5 <- p5+theme(panel.border = element_blank(), panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

layout <- matrix(c(1,1,2,2,3,4,4,5),2,4,byrow=TRUE)
multiplot(p1, p2, p3, p4, p5, layout=layout)


p1 <- train %>%
  group_by(ps_calc_04, target) %>%
  count() %>%
  spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
  ) %>%
  ggplot(aes(ps_calc_04, frac_claim)) +
  geom_point(color = "steelblue4", size = 3) +
  geom_line(aes(ps_calc_04, frac_claim), col = 'cyan', size = 1) +
  theme(legend.position = "none") +
  labs(x = "ps_calc_04", y = "Claim Rate")
p1 <- p1+theme(panel.border = element_blank(), panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

p2 <- train %>%
  filter(ps_calc_05 < 6) %>%
  group_by(ps_calc_05, target) %>%
  count() %>%
  spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
  ) %>%
  ggplot(aes(ps_calc_05, frac_claim)) +
  geom_point(color = "steelblue4", size = 3) +
  geom_line(aes(ps_calc_05, frac_claim), col = 'cyan', size = 1) +
  theme(legend.position = "none") +
  labs(x = "ps_calc_05", y = "Claim Rate")
p2 <- p2+theme(panel.border = element_blank(), panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

p3 <- train %>%
  filter(ps_calc_06 > 2) %>%
  group_by(ps_calc_06, target) %>%
  count() %>%
  spread(target, n, fill = 0) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
  ) %>%
  ggplot(aes(ps_calc_06, frac_claim)) +
  geom_point(color = "steelblue4", size = 3) +
  geom_line(aes(ps_calc_06, frac_claim), col = 'cyan', size = 1) +
  theme(legend.position = "none") +
  labs(x = "ps_calc_06", y = "Claim Rate")
p3 <- p3+theme(panel.border = element_blank(), panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

p4 <- train %>%
  filter(ps_calc_07 < 8) %>%
  group_by(ps_calc_07, target) %>%
  count() %>%
  spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
  ) %>%
  ggplot(aes(ps_calc_07, frac_claim)) +
  geom_point(color = "steelblue4", size = 3) +
  geom_line(aes(ps_calc_07, frac_claim), col = 'cyan', size = 1) +
  theme(legend.position = "none") +
  labs(x = "ps_calc_07", y = "Claim Rate")
p4 <- p4+theme(panel.border = element_blank(), panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

p5 <- train %>%
  filter(ps_calc_08 > 2) %>%
  group_by(ps_calc_08, target) %>%
  count() %>%
  spread(target, n, fill = 0) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
  ) %>%
  ggplot(aes(ps_calc_08, frac_claim)) +
  geom_point(color = "steelblue4", size = 3) +
  geom_line(aes(ps_calc_08, frac_claim), col = 'cyan', size = 1) +
  theme(legend.position = "none") +
  labs(x = "ps_calc_08", y = "Claim Rate")
p5 <- p5+theme(panel.border = element_blank(), panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

p6 <- train %>%
  group_by(ps_calc_09, target) %>%
  count() %>%
  spread(target, n, fill = 0) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
  ) %>%
  ggplot(aes(ps_calc_09, frac_claim)) +
  geom_point(color = "steelblue4", size = 3) +
  geom_line(aes(ps_calc_09, frac_claim), col = 'cyan', size = 1) +
  theme(legend.position = "none") +
  labs(x = "ps_calc_09", y = "Claim Rate")
p6 <- p6+theme(panel.border = element_blank(), panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

p7 <- train %>%
  group_by(ps_calc_10, target) %>%
  count() %>%
  spread(target, n, fill = 0) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
  ) %>%
  ggplot(aes(ps_calc_10, frac_claim)) +
  geom_point(color = "steelblue4", size = 3) +
  geom_line(aes(ps_calc_10, frac_claim), col = 'cyan', size = 1) +
  theme(legend.position = "none") +
  labs(x = "ps_calc_10", y = "Claim Rate")
p7 <- p7+theme(panel.border = element_blank(), panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

p8 <- train %>%
  group_by(ps_calc_11, target) %>%
  count() %>%
  spread(target, n, fill = 0) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
  ) %>%
  ggplot(aes(ps_calc_11, frac_claim)) +
  geom_point(color = "steelblue4", size = 3) +
  geom_line(aes(ps_calc_11, frac_claim), col = 'cyan', size = 1) +
  theme(legend.position = "none") +
  labs(x = "ps_calc_11", y = "Claim Rate")
p8 <- p8+theme(panel.border = element_blank(), panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
               
p9 <- train %>%
  filter(ps_calc_12 < 9) %>%
  group_by(ps_calc_12, target) %>%
  count() %>%
  spread(target, n, fill = 0) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
  ) %>%
  ggplot(aes(ps_calc_12, frac_claim)) +
  geom_point(color = "steelblue4", size = 3) +
  geom_line(aes(ps_calc_12, frac_claim), col = 'cyan', size = 1) +
  theme(legend.position = "none") +
  labs(x = "ps_calc_12", y = "Claim Rate")

p9 <- p9+theme(panel.border = element_blank(), panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

p10 <- train %>%
  filter(ps_calc_13 < 12) %>%
  group_by(ps_calc_13, target) %>%
  count() %>%
  spread(target, n, fill = 0) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
  ) %>%
  ggplot(aes(ps_calc_13, frac_claim)) +
  geom_point(color = "steelblue4", size = 3) +
  geom_line(aes(ps_calc_13, frac_claim), col = 'cyan', size = 1) +
  theme(legend.position = "none") +
  labs(x = "ps_calc_13", y = "Claim Rate")

p10 <- p10+theme(panel.border = element_blank(), panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


p11 <- train %>%
  filter(ps_calc_14 < 12) %>%
  group_by(ps_calc_14, target) %>%
  count() %>%
  spread(target, n, fill = 0) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
  ) %>%
  ggplot(aes(ps_calc_14, frac_claim)) +
  geom_point(color = "steelblue4", size = 3) +
  geom_line(aes(ps_calc_14, frac_claim), col = 'cyan', size = 1) +
  theme(legend.position = "none") +
  labs(x = "ps_calc_14", y = "Claim Rate")

p11 <- p11+theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

layout <- matrix(c(1,2,3,4,5,6,7,7,8,8,9,10,10,11,11),3,5,byrow=TRUE)
multiplot(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, layout=layout)


p1 <- train %>%
  ggplot(train$ps_reg_01) +
  geom_density(alpha = 0.5, bw = 0.05, color = target) +
  theme(legend.position = "none")
p1 <- p1+scale_fill_brewer("Paired") 

ggplot(barley) + geom_density(aes(x = yield, color = site))

p1 <- plot(density(train$ps_reg_01, bw=0.05))
p2 <- train %>%
  ggplot(aes(ps_reg_02, fill = target)) +
  geom_density(alpha = 0.5, bw = 0.05) +
  theme(legend.position = "none")

p3 <- train %>%
  ggplot(aes(ps_reg_03, fill = target)) +
  geom_density(alpha = 0.5, bw = 0.05) +
  theme(legend.position = "none")

p4 <- train %>%
  ggplot(aes(ps_calc_01, fill = target)) +
  geom_density(alpha = 0.5, bw = 0.05) +
  theme(legend.position = "none")

p5 <- train %>%
  ggplot(aes(ps_calc_02, fill = target)) +
  geom_density(alpha = 0.5, bw = 0.05) +
  theme(legend.position = "none")

p6 <- train %>%
  ggplot(aes(ps_calc_03, fill = target)) +
  geom_density(alpha = 0.5, bw = 0.05)

layout <- matrix(c(1,2,3,4,5,6),2,3,byrow=TRUE)
multiplot(p1, p2, p3, p4, p5, p6, layout=layout)