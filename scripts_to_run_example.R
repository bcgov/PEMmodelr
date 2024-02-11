require(data.table)
require(tidyverse)

acc_all1 <- fread("./30_model_final/ESSFmc/all_results_summary.csv")
acc_all2 <- fread("./30_model_final/ESSFmcw/all_results_summary.csv")
acc_all3 <- fread("./30_model_final/SBSmc2/all_results_summary.csv")
acc_all <- rbind(acc_all1, acc_all2, acc_all3) %>% mutate(type = ifelse(is.na(type_sa), "0_machine", type)) %>% filter(!grepl(".5", accuracy_type))
acc_all2 <- acc_all %>% filter(!(type_sa == "proportional" & type == "primary"), !(type_sa == "proportional" & type == "3_fuzzy_p+a"), !acc_type == "rebalanced") %>%
  mutate(type = ifelse(type_sa == "proportional", "4_aspatial", type)) %>%
  mutate(type = ifelse(is.na(type_sa), "0_machine", type))

###Create ggplot graphic
overall_acc <- ggplot(aes(y = value, x = accuracy_type, fill = acc_type), data = acc_all2 ) + # fill = accuracy_type2
  geom_boxplot() +
  #scale_fill_brewer(type = "qual") +
  facet_grid(bgc ~ type, labeller = labeller(type =
                                            c("0_machine" = "machine",
                                              "1_primary" = "primary",
                                              "2_p+alternate" = "alternate",
                                              "3_fuzzy_p+a" = "fuzzy",
                                              "4_aspatial" = "proportional (p+a)")), scales = "free_x")+
  #facet_wrap(~acc_type)+
  #geom_jitter(position=position_jitter(width=.1), colour = "grey", alpha = 0.8) +
  geom_hline(yintercept = 65,linetype ="dashed", color = "black") +
  theme_pem_facet() +
  scale_y_continuous(breaks=seq(30,100,10))+
  scale_fill_manual(values=c("grey100", "grey50", "grey25"))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position="none") +
  xlab("Metric") + ylab("Accuracy") #+
  #ylim(25, 100)


overall_acc

finalise_facet_plot(overall_acc, "./PEM_standards_manuscripts/outputs/accuracy_raw_vs_balance_no_extras_25limit.png", width_pixels=480,
                    height_pixels=480)

##Build table improvements by adjustment type

acc_dif <- acc_all %>% filter(!type == "0_machine", !type_sa == "proportional") %>% group_by(slice, bgc, type, acc_type, type_sa, accuracy_type) %>%
  mutate(mean_acc = mean(value)) %>% ungroup()
acc_dif <- acc_dif %>% select(-value) %>% distinct()
raw_acc <- acc_dif %>% filter(acc_type == "raw", type == "1_primary") %>% mutate(raw_acc = mean_acc) %>% select(-acc_type, -mean_acc, -type)
#acc_dif <- acc_dif %>% filter(!acc_type == "raw")
acc_dif2 <- left_join(acc_dif,raw_acc, by= c("slice", "accuracy_type","type_sa","bgc")) %>% mutate(improve = mean_acc - raw_acc) %>%
  mutate(acctype2 = paste(type, acc_type)) %>% filter(!type == "4_aspatial")

acc_sum <- acc_dif2 %>%  group_by(acctype2, accuracy_type) %>% mutate(meanimprove = mean(improve))%>% select(accuracy_type,acctype2, meanimprove) %>% distinct()
acc_sum2 <- pivot_wider(acc_sum, id_cols = accuracy_type, names_from = acctype2, values_from = meanimprove)
acc_sum_final <- acc_sum2 %>%
  select(accuracy_type, `2_p+alternate raw`, `3_fuzzy_p+a raw`, `1_primary raw_neighbours`, `1_primary rebalanced`, `3_fuzzy_p+a rebalanced_neighbours`) %>%
  mutate_if(is.numeric, round, digits = 2) %>%
  rename(Theta = accuracy_type, Alternate = `2_p+alternate raw`, Fuzzy = `3_fuzzy_p+a raw`,
         `Best Neighbour` = `1_primary raw_neighbours`, `Best Balance`= `1_primary rebalanced`, All = `3_fuzzy_p+a rebalanced_neighbours`)

#init_flextable_defaults()
acc_adj_flex <- flextable(acc_sum_final) %>% flextable::width(width = 1.5) %>% fit_to_width(max_width = 7, inc = 1L, max_iter = 20, unit = "in")
flextable_dim(acc_adj_flex)
plot(acc_adj_flex)
save_as_docx(acc_adj_flex , path = "Acc_Improve.docx")

