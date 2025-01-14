## ----setup, include=FALSE, message=FALSE-------------------------------------------------------------------------------------------------------------------------------
library(pacman)

`%notin%` <- Negate(`%in%`)

p_load(
  #--- Packages to Fit Models
  MASS,
  #--- Packages to Produce Tables
  gtsummary, flextable, janitor, broom, officer, kableExtra, reactable, 
  #--- Packages to Produce Figures
  crayon, ggsci, ggridges, ggthemes, ggforce, ggpubr, patchwork, grid, gridExtra, ggstance,  survminer, viridis, ggridges, hrbrthemes, latex2exp, scales, glue, 
  #--- Packages for Data Retrieval & Pre-Processing
  readxl, here, lubridate, zoo, tidyverse, purrr, data.table, stringr, tidyr
)

## reading in output data
op_files <-list.files(file.path(here(), "2023_bka", "code", "simulation_coverage"), pattern = "csv")
op <- op_files %>% map_dfc(~read_csv(.x))
colnames(op) <- c(paste(c("1"), c(0, 0.1, 0.15, 0.20, 0.25), sep = "_"), 
                  paste(c("2"), c(0, 0.1, 0.15, 0.20, 0.25), sep = "_"),
                  paste(c("3"), c(0, 0.1, 0.15, 0.20, 0.25), sep = "_"),
                  paste(c("4"), c(0, 0.05, 0.1, 0.15, 0.20), sep = "_"),
                  paste(c("5"), c(0, 0.05, 0.1, 0.15, 0.20), sep = "_"),
                  paste(c("6"), c(0, 0.05, 0.1, 0.15, 0.20), sep = "_"))
p1 <- op %>% 
  mutate(id = seq_along('1_0.00')) %>% 
  pivot_longer(cols = -id) %>% 
  select(-id) %>% 
  rowwise() %>% 
  mutate(case = unlist(str_split(name, "_"))[1], 
         eps = (unlist(str_split(name, "_"))[2])) %>% 
  filter(case == 3) %>% 
  mutate(eps = factor(eps, 
                      levels = c("0", "0.1", "0.15", "0.2", "0.25"),
                         labels = c("0.00", "0.10", "0.15", "0.20", "0.25"))) %>% 
  ggplot(aes(x = value, y = eps, group = eps)) + 
  stat_density_ridges(aes(fill = eps), bandwidth = 0.018, scale = 2, 
                      quantile_lines = FALSE, quantiles = c(0.025, 0.5, 0.975), rel_min_height = 0.005, 
                      size = 1, alpha = 0.15) + 
  stat_density_ridges(aes(fill = eps), bandwidth = 0.018, scale = 2, 
                      quantile_lines = TRUE, quantiles = c(0.025, 0.5, 0.975), 
                      linetype = "dashed",
                      rel_min_height = 0.005, alpha = 0.15) +
  scale_fill_aaas() +
  theme_ridges() + 
  theme(legend.position = "bottom", 
        strip.background = element_rect(fill = "black"), 
         plot.title = element_text(face = "bold", size = 20, hjust = 0.5), 
         plot.caption = element_text(size = 20, hjust = 0, face = "italic"), 
        strip.text = element_text(size = 20, color = "white", face = "bold"), 
        axis.title = element_text(size = 20), 
         axis.text = element_text(size = 20), 
         legend.text = element_text(size = 20),
        legend.title = element_text(size = 20, face = "bold")) +
  scale_fill_viridis_d() + 
  labs(x = TeX("\\textbf{Value of } ${C}_{X >Y}$"), 
       fill = TeX("$\\sigma$"), 
       y = TeX("\\textbf{Contaminant strength} $\\sigma$"), 
       title = TeX("\\textbf{Smoothed density estimate of ${C}_{X >Y}$ when $Y = X^{2} + \\sqrt{\\sigma} \\epsilon$.}")
       ) 

p2 <- op %>% 
  mutate(id = seq_along('1_0.00')) %>% 
  pivot_longer(cols = -id) %>% 
  select(-id) %>% 
  rowwise() %>% 
  mutate(case = unlist(str_split(name, "_"))[1], 
         eps = (unlist(str_split(name, "_"))[2])) %>% 
  filter(case == 2) %>% 
  mutate(eps = factor(eps, 
                      levels = c("0", "0.1", "0.15", "0.2", "0.25"),
                      labels = c("0.00", "0.10", "0.15", "0.20", "0.25"))) %>% 
  ggplot(aes(x = value, y = eps, group = eps)) + 
  stat_density_ridges(aes(fill = eps), bandwidth = 0.018, scale = 2, 
                      quantile_lines = FALSE, quantiles = c(0.025, 0.5, 0.975), rel_min_height = 0.005, 
                      size = 1, alpha = 0.15) + 
  stat_density_ridges(aes(fill = eps), bandwidth = 0.018, scale = 2, 
                      quantile_lines = TRUE, quantiles = c(0.025, 0.5, 0.975), 
                      linetype = "dashed",
                      rel_min_height = 0.005, alpha = 0.15) +
  scale_fill_aaas() +
  theme_ridges() + 
  scale_y_discrete(labels = (c(TeX("\\sigma = 0.00"), TeX("\\sigma = 0.10"), 
                               TeX("\\sigma = 0.15"), TeX("\\sigma = 0.20"),
                               TeX("\\sigma = 0.25"))))+ 
  theme(legend.position = "bottom", 
        # strip.background = element_rect(fill = "black"), 
        plot.title = element_text(face = "bold", size = 20), 
        plot.caption = element_text(size = 20, hjust = 0, face = "italic"), 
        # strip.text = element_text(size = 20, color = "white", face = "bold"), 
        axis.title = element_text(size = 20, hjust= 0), 
        axis.text = element_text(size = 20), 
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20, face = "bold")) +
  scale_fill_viridis_d() + 
  labs(x = TeX("\\textbf{Value of } ${C}_{X >Y}$"), 
       fill = TeX("$\\sigma$"), 
       y = TeX("Standard deviation ($\\sigma$) of $\\epsilon$"), 
       title = TeX("Behaviour of ${C}_{X >Y}$ in the GEM model $Y = X^{1/2} + \\epsilon$, where $X \\sim U(0, 1)$ and $\\epsilon \\sim N(0, \\sigma^2)$."),
       subtitle = TeX("Smoothed density estimates of ${C}_{X >Y}$ are stratified by standard deviation ($\\sigma$) of contaminating noise $\\epsilon$.")
  ) 


p3 <- op %>% 
  mutate(id = seq_along('1_0.00')) %>% 
  pivot_longer(cols = -id) %>% 
  select(-id) %>% 
  rowwise() %>% 
  mutate(case = unlist(str_split(name, "_"))[1], 
         eps = (unlist(str_split(name, "_"))[2])) %>% 
  filter(case == 3) %>% 
  mutate(eps = factor(eps, 
                      levels = c("0", "0.1", "0.15", "0.2", "0.25"),
                      labels = c("0.00", "0.10", "0.15", "0.20", "0.25"))) %>% 
  ggplot(aes(x = value, y = eps, group = eps)) + 
  stat_density_ridges(aes(fill = eps), bandwidth = 0.018, scale = 2, 
                      quantile_lines = FALSE, quantiles = c(0.025, 0.5, 0.975), rel_min_height = 0.005, 
                      size = 1, alpha = 0.15) + 
  stat_density_ridges(aes(fill = eps), bandwidth = 0.018, scale = 2, 
                      quantile_lines = TRUE, quantiles = c(0.025, 0.5, 0.975), 
                      linetype = "dashed",
                      rel_min_height = 0.005, alpha = 0.15) +
  scale_fill_aaas() +
  theme_ridges() + 
  scale_y_discrete(labels = (c(TeX("\\sigma = 0.00"), TeX("\\sigma = 0.10"), 
                               TeX("\\sigma = 0.15"), TeX("\\sigma = 0.20"),
                               TeX("\\sigma = 0.25"))))+ 
  theme(legend.position = "bottom", 
        # strip.background = element_rect(fill = "black"), 
        plot.title = element_text(face = "bold", size = 20), 
        plot.caption = element_text(size = 20, hjust = 0, face = "italic"), 
        # strip.text = element_text(size = 20, color = "white", face = "bold"), 
        axis.title = element_text(size = 20, hjust= 0), 
        axis.text = element_text(size = 20), 
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20, face = "bold")) +
  scale_fill_viridis_d() + 
  labs(x = TeX("\\textbf{Value of } ${C}_{X >Y}$"), 
       fill = TeX("$\\sigma$"), 
       y = TeX("Standard deviation ($\\sigma$) of $\\epsilon$"), 
       title = TeX("Behaviour of ${C}_{X >Y}$ in the GEM model $Y = X^{2} + \\epsilon$, where $X \\sim U(0, 1)$ and $\\epsilon \\sim N(0, \\sigma^2)$."),
       subtitle = TeX("Smoothed density estimates of ${C}_{X >Y}$ are stratified by standard deviation ($\\sigma$) of contaminating noise $\\epsilon$.")
  ) 

p4 <- op %>% 
  mutate(id = seq_along('1_0.00')) %>% 
  pivot_longer(cols = -id) %>% 
  select(-id) %>% 
  rowwise() %>% 
  mutate(case = unlist(str_split(name, "_"))[1], 
         eps = (unlist(str_split(name, "_"))[2])) %>% 
  filter(case == 4) %>% 
  mutate(eps = factor(eps, 
                      levels = c("0", "0.05", "0.1", "0.15", "0.2"),
                      labels = c("0.00", "0.05", "0.10", "0.15", "0.20"))) %>% 
  ggplot(aes(x = value, y = eps, group = eps)) + 
  stat_density_ridges(aes(fill = eps), bandwidth = 0.025, scale = 2, 
                      quantile_lines = FALSE, quantiles = c(0.025, 0.5, 0.975), rel_min_height = 0.005, 
                      size = 1, alpha = 0.15) + 
  stat_density_ridges(aes(fill = eps), bandwidth = 0.025, scale = 2, 
                      quantile_lines = TRUE, quantiles = c(0.025, 0.5, 0.975), 
                      linetype = "dashed",
                      rel_min_height = 0.005, alpha = 0.15) +
  scale_fill_aaas() +
  theme_ridges() + 
  scale_y_discrete(labels = (c(TeX("\\sigma = 0.00"), TeX("\\sigma = 0.05"), 
                               TeX("\\sigma = 0.10"), TeX("\\sigma = 0.15"),
                               TeX("\\sigma = 0.20"))))+ 
  theme(legend.position = "bottom", 
        # strip.background = element_rect(fill = "black"), 
        plot.title = element_text(face = "bold", size = 20), 
        plot.caption = element_text(size = 20, hjust = 0, face = "italic"), 
        # strip.text = element_text(size = 20, color = "white", face = "bold"), 
        axis.title = element_text(size = 20, hjust= 0), 
        axis.text = element_text(size = 20), 
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20, face = "bold")) +
  scale_fill_viridis_d() + 
  labs(x = TeX("\\textbf{Value of } ${C}_{X >Y}$"), 
       fill = TeX("$\\sigma$"), 
       y = TeX("Standard deviation ($\\sigma$) of $\\epsilon$"), 
       title = TeX("Behaviour of ${C}_{X >Y}$ in the GEM model $Y = sin(\\pi X / 2) + \\epsilon$, where $X \\sim U(0, 1)$ and $\\epsilon \\sim N(0, \\sigma^2)$."),
       subtitle = TeX("Smoothed density estimates of ${C}_{X >Y}$ are stratified by standard deviation ($\\sigma$) of contaminating noise $\\epsilon$.")
  ) 



p5 <- op %>% 
  mutate(id = seq_along('1_0.00')) %>% 
  pivot_longer(cols = -id) %>% 
  select(-id) %>% 
  rowwise() %>% 
  mutate(case = unlist(str_split(name, "_"))[1], 
         eps = (unlist(str_split(name, "_"))[2])) %>% 
  filter(case == 5) %>% 
  mutate(eps = factor(eps, 
                      levels = c("0", "0.05", "0.1", "0.15", "0.2"),
                      labels = c("0.00", "0.05", "0.10", "0.15", "0.20"))) %>% 
  ggplot(aes(x = value, y = eps, group = eps)) + 
  stat_density_ridges(aes(fill = eps), bandwidth = 0.025, scale = 2, 
                      quantile_lines = FALSE, quantiles = c(0.025, 0.5, 0.975), rel_min_height = 0.005, 
                      size = 1, alpha = 0.15) + 
  stat_density_ridges(aes(fill = eps), bandwidth = 0.025, scale = 2, 
                      quantile_lines = TRUE, quantiles = c(0.025, 0.5, 0.975), 
                      linetype = "dashed",
                      rel_min_height = 0.005, alpha = 0.15) +
  scale_fill_aaas() +
  theme_ridges() + 
  scale_y_discrete(labels = (c(TeX("\\sigma = 0.00"), TeX("\\sigma = 0.05"), 
                               TeX("\\sigma = 0.10"), TeX("\\sigma = 0.15"),
                               TeX("\\sigma = 0.20"))))+ 
  theme(legend.position = "bottom", 
        # strip.background = element_rect(fill = "black"), 
        plot.title = element_text(face = "bold", size = 20), 
        plot.caption = element_text(size = 20, hjust = 0, face = "italic"), 
        # strip.text = element_text(size = 20, color = "white", face = "bold"), 
        axis.title = element_text(size = 20, hjust= 0), 
        axis.text = element_text(size = 20), 
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20, face = "bold")) +
  scale_fill_viridis_d() + 
  labs(x = TeX("\\textbf{Value of } ${C}_{X >Y}$"), 
       fill = TeX("$\\sigma$"), 
       y = TeX("Standard deviation ($\\sigma$) of $\\epsilon$"), 
       title = TeX("Behaviour of ${C}_{X >Y}$ in the GEM model $Y = \\exp(X) + \\epsilon$, where $X \\sim U(0, 1)$ and $\\epsilon \\sim N(0, \\sigma^2)$."),
       subtitle = TeX("Smoothed density estimates of ${C}_{X >Y}$ are stratified by standard deviation ($\\sigma$) of contaminating noise $\\epsilon$.")
  ) 


p6 <- op %>% 
  mutate(id = seq_along('1_0.00')) %>% 
  pivot_longer(cols = -id) %>% 
  select(-id) %>% 
  rowwise() %>% 
  mutate(case = unlist(str_split(name, "_"))[1], 
         eps = (unlist(str_split(name, "_"))[2])) %>% 
  filter(case == 6) %>% 
  mutate(eps = factor(eps, 
                      levels = c("0", "0.05", "0.1", "0.15", "0.2"),
                      labels = c("0.00", "0.05", "0.10", "0.15", "0.20"))) %>% 
  ggplot(aes(x = value, y = eps, group = eps)) + 
  stat_density_ridges(aes(fill = eps), bandwidth = 0.030, scale = 2, 
                      quantile_lines = FALSE, quantiles = c(0.025, 0.5, 0.975), rel_min_height = 0.005, 
                      size = 1, alpha = 0.15) + 
  stat_density_ridges(aes(fill = eps), bandwidth = 0.030, scale = 2, 
                      quantile_lines = TRUE, quantiles = c(0.025, 0.5, 0.975), 
                      linetype = "dashed",
                      rel_min_height = 0.005, alpha = 0.15) +
  scale_fill_aaas() +
  theme_ridges() + 
  scale_y_discrete(labels = (c(TeX("\\sigma = 0.00"), TeX("\\sigma = 0.05"), 
                               TeX("\\sigma = 0.10"), TeX("\\sigma = 0.15"),
                               TeX("\\sigma = 0.20"))))+ 
  theme(legend.position = "bottom", 
        # strip.background = element_rect(fill = "black"), 
        plot.title = element_text(face = "bold", size = 20), 
        plot.caption = element_text(size = 20, hjust = 0, face = "italic"), 
        # strip.text = element_text(size = 20, color = "white", face = "bold"), 
        axis.title = element_text(size = 20, hjust= 0), 
        axis.text = element_text(size = 20), 
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20, face = "bold")) +
  scale_fill_viridis_d() + 
  labs(x = TeX("\\textbf{Value of } ${C}_{X >Y}$"), 
       fill = TeX("$\\sigma$"), 
       y = TeX("Standard deviation ($\\sigma$) of $\\epsilon$"), 
       title = TeX("Behaviour of ${C}_{X >Y}$ in the GEM model $Y = \\log(X) + \\epsilon$, where $X \\sim U(0, 1)$ and $\\epsilon \\sim N(0, \\sigma^2)$."),
       subtitle = TeX("Smoothed density estimates of ${C}_{X >Y}$ are stratified by standard deviation ($\\sigma$) of contaminating noise $\\epsilon$.")
  ) 


((p1 + labs(title = NULL, subtitle = NULL)) + 
  (p2 + labs(title = NULL, subtitle = NULL)) +
  (p3 + labs(title = NULL, subtitle = NULL)))/((p4 + labs(title = NULL, subtitle = NULL)) + 
                                                 (p5 + labs(title = NULL, subtitle = NULL)) +
                                                 (p6 + labs(title = NULL, subtitle = NULL))) + 
  plot_annotation(title = TeX("Behaviour of ${C}_{X >Y}$ in the GEM model $Y = f(X) + \\epsilon$, where $X \\sim U(0, 1)$ and $\\epsilon \\sim N(0, \\sigma^2)$. Smoothed density estimates of ${C}_{X >Y}$ are stratified by standard deviation ($\\sigma$) of contaminating noise $\\epsilon$."),
                  subtitle = TeX("Case I: $f(x) = x^{1/3}$, II: $f(x) = x^{1/2}$, III: $f(x) = x^{2}$, IV: $f(x) = x^{3}$, V: $f(x) = \\exp(x)$, VI: $f(x) = \\sin(x)$. Dashed vertical lines in each density plot correspond to 2.5th, 50th and 97.5th percentiles of each distribution."),
                  tag_levels = "I") 
c(8.5, 11)*1.45
#(p1 + p2)/(p3 + p4)


text <- op %>% 
  mutate(id = seq_along('1_0.00')) %>% 
  pivot_longer(cols = -id) %>% 
  select(-id) %>% 
  rowwise() %>% 
  mutate(case = unlist(str_split(name, "_"))[1], 
         eps = (unlist(str_split(name, "_"))[2])) %>%
  group_by(case, eps) %>% 
  summarise(q1 = round(quantile(value, 0.025), 2),
            q2 = round(mean(value), 2),
            q3 = round(quantile(value, 0.975), 2)) %>% 
  mutate(text = paste0(q2, " (", q1, ", ", q3, ")")) %>% 
  ungroup() %>% 
  select(-contains("q")) %>% 
  pivot_wider(names_from = case, values_from = text) %>% 
  filter(eps %in% c("0.1", "0.2"))
