library(ggplot2)

# Definições para gerar os gráficos
seta <- grid::arrow(length = grid::unit(0.2, 'cm'), type = 'open')

default_theme <- function (base_size = 14, base_family = 'Arial') {
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(axis.ticks = element_blank(),
          axis.line = element_line(arrow = seta, color = 'gray20'),
          legend.background = element_blank(),
          legend.key = element_blank(),
          panel.background = element_blank(),
          panel.border = element_blank(),
          strip.background = element_blank(),
          plot.background = element_blank(),
          plot.title = element_text(hjust = 1),
          complete = TRUE)
}

ggplot(nb_fit) + 
  default_theme() +
  labs(y = 'Acurácia (%)', 
       x = 'Ajuste do kernel (adjust)') +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), 
                     limits = c(0.680, 0.692), 
                     breaks = seq(0.680, 0.692, 0.002)) +
  scale_x_continuous(limits = c(0, 4), breaks = seq(0, 4, 1))

ggplot(knn_fit) + 
  default_theme() +
  geom_line(color = '#319795') + 
  geom_point(color = '#319795') +
  labs(y = 'Acurácia (%)', 
       x = 'Nº de vizinhos (k)') +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), 
                     limits = c(0.716, 0.746), 
                     breaks = seq(0.716, 0.746, 0.002)) +
  scale_x_continuous(limits = c(3, 43), breaks = seq(3, 43, 2))

ggplot(dt_fit) +
  default_theme() +
  labs(y = 'Acurácia (%)',
       x = 'Árvores paralelas (trials)') +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1),
                     limits = c(0.768, 0.784),
                     breaks = seq(0.768, 0.784, 0.002)) +
  scale_x_continuous(limits = c(1, 15), breaks = seq(1, 15, 2))

ggplot(rf_fit) + 
  default_theme() +
  labs(y = 'Acurácia (%)', 
       x = 'Nº de variáveis selecionadas aleatoriamente') +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), 
                     limits = c(0.770, 0.788), 
                     breaks = seq(0.770, 0.789, 0.002)) +
  scale_x_continuous(limits = c(0, 22), breaks = seq(0, 22, 4))
