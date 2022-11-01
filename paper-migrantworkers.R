library(tidyverse)
library(lubridate)

# Figure 1
f1 <- read_csv("fig1.csv")
f1.plot <- f1 %>%
  pivot_longer(-Year, names_to = "Country", values_to = "Value") %>%
  mutate(Year = make_date(Year)) %>%
  mutate(Country = factor(Country, levels = c("Philippines", "Thailand", "Vietnam", "Indonesia"))) %>%
  arrange(desc(Country)) %>%
  ggplot(aes(x = Year, y = Value, group = Country)) +
  geom_line(aes(linetype = Country)) +
  geom_point(aes(shape = Country),
    fill = "white",
    size = 5
  ) +
  labs(x = "\nYear", y = "Number\n") +
  scale_shape_manual(values = c(15, 25, 22, 10), name = "") +
  scale_linetype_manual(
    values = c("solid", "dotted", "longdash", "dotdash"),
    name = ""
  ) +
  scale_x_date(breaks = seq(as_date("1998-01-01"), as_date("2012-01-01"),
    by = "2 years"
  ), date_labels = "%Y") +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    axis.line = element_line(color = "black"),
    legend.key = element_blank(),
    legend.background = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(1, 0),
    legend.key.width = unit(3, "line"),
    text = element_text(size = 16)
  )
f1.plot
ggsave("fig1.emf", f1.plot, height = 7, width = 9)
ggsave("fig1.png", f1.plot, height = 7, width = 9)

# Figure 2
f2 <- read_csv("fig2.csv")
f2.plot <- f2 %>%
  setNames(c(
    "Year", "Manufacturing Workers",
    "Nursing Workers and Home Maids (Domestic Workers)"
  )) %>%
  pivot_longer(-Year, names_to = "Type", values_to = "Value") %>%
  mutate(Year = make_date(Year)) %>%
  ggplot(aes(x = Year, y = Value, group = Type)) +
  geom_line(aes(linetype = Type)) +
  geom_point(aes(shape = Type),
    fill = "white",
    size = 5
  ) +
  labs(x = "\nYear", y = "Number\n") +
  scale_shape_manual(values = c(15, 25), name = "") +
  scale_linetype_manual(values = c("solid", "dotted"), name = "") +
  scale_x_date(breaks = seq(as_date("1998-01-01"), as_date("2012-01-01"),
    by = "2 years"
  ), date_labels = "%Y") +
  scale_y_continuous(limits = c(50000, 250000)) +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    axis.line = element_line(color = "black"),
    legend.key = element_blank(),
    legend.background = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(1, 0),
    legend.key.width = unit(3, "line"),
    text = element_text(size = 16)
  )
f2.plot
ggsave("fig2.emf", f2.plot, height = 7, width = 9)

# Figure 3
f3 <- read_csv("fig3.csv")
f3.plot <- f3 %>%
  mutate(Industry = c(
    "Manufacturing",
    "Agriculture, Forestry, Fishing,\nAnimal Husbandry & Construction",
    "Human Health, Social Work\nServices & Other Services\n(Domestic Work)"
  )) %>%
  ggplot(aes(x = Industry, y = Number)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "\nIndustry", y = "Number\n") +
  scale_y_continuous(limits = c(0, max(f3$Number) + 1000), expand = c(0, 0)) +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    axis.line = element_line(color = "black"),
    text = element_text(size = 16)
  )
f3.plot
ggsave("fig3.emf", f3.plot, height = 7, width = 9)