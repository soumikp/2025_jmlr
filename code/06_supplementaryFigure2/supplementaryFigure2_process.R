pacman::p_load(tidyverse, here, scales, latex2exp, patchwork)


## figure 1
temp <- read_csv(file.path("/home/soumikp/2023_bka", "output", "sim3_nor_nor.csv"), col_names = FALSE) %>% 
  rename(a = X1, b = X2) %>% 
  mutate(text = paste0(round(X3, 2), "\n(", round(X4, 2), ", ", round(X5, 2), ")"), 
         value = round(X3, 2)) %>% 
  select(c(a, b, value, text))

xlabel <- "N(0, $\\sigma^2$)"
ylabel <- "N(0, $\\sigma^2$)"

fig1 <- temp %>%
  ggplot(aes(x = a, y = b)) + 
  geom_tile(aes(fill = value)) + 
  geom_text(aes(label = text), size = 5.5) + 
  scale_fill_gradient2(high="#00274C",mid="white", low="#FFD700", midpoint = 0) + 
  scale_x_continuous(breaks = temp %>% pull(a) %>% unique() %>% sort(),
                     labels = label_number(accuracy = 0.01),
                     expand=c(0,0)) +
  scale_y_continuous(breaks = temp %>% pull(b) %>% unique() %>% sort(),
                     labels = label_number(accuracy = 0.01),
                     expand=c(0,0)) + 
  theme_bw() + 
  theme(text = element_text(face = "bold", size = 14),
        plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(face = "bold", size = 16),
        axis.text = element_text(face = "bold", size = 14),
        axis.title = element_text(face = "bold", size = 18), 
        legend.position = "none") +
  labs(subtitle = TeX(paste0("(I) Gaussian copula ($\\rho=$0.25): $f_X$ ~ ", xlabel, ", $f_Y$ ~ ", ylabel, "."))) +
  xlab(TeX("$\\sigma$")) + 
  ylab(TeX("$\\sigma$"))


## figure 2
temp <- read_csv(file.path("/home/soumikp/2023_bka", "output", "sim3_nor_exp.csv"), col_names = FALSE) %>% 
  rename(a = X1, b = X2) %>% 
  mutate(text = paste0(round(-X3, 2), "\n(", round(-X5, 2), ", ", round(-X4, 2), ")"), 
         value = round(-X3, 2)) %>% 
  select(c(a, b, value, text))

xlabel <- "N(0, $\\sigma^2$)"
ylabel <- "Exp($\\lambda$)"

fig2 <- temp %>%
  ggplot(aes(x = b, y = a)) + 
  geom_tile(aes(fill = value)) + 
  geom_text(aes(label = text), size = 5.5) + 
  scale_fill_gradient2(high="#00274C",mid="white", low="#FFD700", midpoint = 0) + 
  scale_x_continuous(breaks = temp %>% pull(b) %>% unique() %>% sort(),
                     labels = label_number(accuracy = 0.01),
                     expand=c(0,0)) +
  scale_y_continuous(breaks = temp %>% pull(a) %>% unique() %>% sort(),
                     labels = label_number(accuracy = 0.01),
                     expand=c(0,0)) + 
  theme_bw() + 
  theme(text = element_text(face = "bold", size = 14),
        plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(face = "bold", size = 16),
        axis.text = element_text(face = "bold", size = 14),
        axis.title = element_text(face = "bold", size = 18), 
        legend.position = "none") +
  labs(subtitle = TeX(paste0("(II) Gaussian copula ($\\rho=$0.25): $f_X$ ~ ", ylabel, ", $f_Y$ ~ ", xlabel, "."))) +
  ylab(TeX("$\\sigma$")) + 
  xlab(TeX("$\\lambda$"))


## figure 3
temp <- read_csv(file.path("/home/soumikp/2023_bka", "output", "sim3_nor_log.csv"), col_names = FALSE) %>% 
  rename(a = X1, b = X2) %>% 
  mutate(text = paste0(round(X3, 2), "\n(", round(X4, 2), ", ", round(X5, 2), ")"), 
         value = round(X3, 2)) %>% 
  select(c(a, b, value, text))

xlabel <- "N(0, $\\sigma^2$)"
ylabel <- "LN(0, $\\gamma$)"

fig3 <- temp %>%
  ggplot(aes(x = a, y = b)) + 
  geom_tile(aes(fill = value)) + 
  geom_text(aes(label = text), size = 5.5) + 
  scale_fill_gradient2(high="#00274C",mid="white", low="#FFD700", midpoint = 0) + 
  scale_x_continuous(breaks = temp %>% pull(a) %>% unique() %>% sort(),
                     labels = label_number(accuracy = 0.01),
                     expand=c(0,0)) +
  scale_y_continuous(breaks = temp %>% pull(b) %>% unique() %>% sort(),
                     labels = label_number(accuracy = 0.01),
                     expand=c(0,0)) + 
  theme_bw() + 
  theme(text = element_text(face = "bold", size = 14),
        plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(face = "bold", size = 16),
        axis.text = element_text(face = "bold", size = 14),
        axis.title = element_text(face = "bold", size = 18), 
        legend.position = "none") +
  labs(subtitle = TeX(paste0("(III) Gaussian copula ($\\rho=$0.25): $f_X$ ~ ", xlabel, ", $f_Y$ ~ ", ylabel, "."))) +
  xlab(TeX("$\\sigma$")) + 
  ylab(TeX("$\\gamma$"))


## figure 4
temp <- read_csv(file.path("/home/soumikp/2023_bka", "output", "sim3_exp_log.csv"), col_names = FALSE) %>% 
  rename(a = X1, b = X2) %>% 
  mutate(text = paste0(round(X3, 2), "\n(", round(X4, 2), ", ", round(X5, 2), ")"), 
         value = round(X3, 2)) %>% 
  select(c(a, b, value, text))

xlabel <- "Exp($\\lambda$)"
ylabel <- "LN(0, $\\gamma$)"

fig4 <- temp %>%
  ggplot(aes(x = a, y = b)) + 
  geom_tile(aes(fill = value)) + 
  geom_text(aes(label = text), size = 5.5) + 
  scale_fill_gradient2(high="#00274C",mid="white", low="#FFD700", midpoint = 0) + 
  scale_x_continuous(breaks = temp %>% pull(a) %>% unique() %>% sort(),
                     labels = label_number(accuracy = 0.01),
                     expand=c(0,0)) +
  scale_y_continuous(breaks = temp %>% pull(b) %>% unique() %>% sort(),
                     labels = label_number(accuracy = 0.01),
                     expand=c(0,0)) + 
  theme_bw() + 
  theme(text = element_text(face = "bold", size = 14),
        plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(face = "bold", size = 16),
        axis.text = element_text(face = "bold", size = 14),
        axis.title = element_text(face = "bold", size = 18), 
        legend.position = "none") +
  labs(subtitle = TeX(paste0("(IV) Gaussian copula ($\\rho=$0.25): $f_X$ ~ ", xlabel, ", $f_Y$ ~ ", ylabel, "."))) +
  xlab(TeX("$\\lambda$")) + 
  ylab(TeX("$\\gamma$"))



## saving
patchwork <- (fig1 + fig2)/(fig3 + fig4)
p <- patchwork + 
  plot_annotation(title = TeX(paste0("$$\\bf{Joint distribution is specified by Gaussian copula with parameter \\rho = 0.25.}$$")), 
                  subtitle = TeX("$$\\bf{For each marginal parameter combination, we report \\hat{C} (95\\% CI)}.$$")) &
  theme(plot.title = element_text(face = "bold", size = 22),
        plot.subtitle = element_text(size = 16))
ggsave(file.path('/home/soumikp/2023_bka', 'simulation2.pdf'), 
       p, 
       device = "pdf", 
       dpi = 300, 
       height = 1.25*11.5, 
       width = 2*8, 
       units = "in")
