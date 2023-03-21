library(tidyverse)
pmf_citations = matrix(data = c(2007, 2,
                                2008, 2,
                                2009, 39,
                                2010, 66,
                                2011, 94,
                                2012, 135,
                                2013, 220,
                                2014, 284,
                                2015, 371,
                                2016, 437,
                                2017, 451,
                                2018, 533,
                                2019, 644,
                                2020, 525,
                                2021, 573,
                                2022, 498), ncol = 2, byrow = T)
colnames(pmf_citations) = c("Year", "Citations")
pmf_citations
plot(pmf_citations)
pmf_citations = data.frame(pmf_citations, Paper = "PMF")
pmf_citations %>%
  ggplot(aes(x = Year, y = Citations)) + geom_point() + theme_bw()
cf_td_citations = matrix(data = c(2009, 15,
                                  2010, 87,
                                  2011, 113,
                                  2012, 132,
                                  2013, 152,
                                  2014, 185,
                                  2015, 201,
                                  2016, 260,
                                  2017, 255,
                                  2018, 242,
                                  2019, 224,
                                  2020, 201,
                                  2021, 214,
                                  2022, 162), ncol = 2, byrow = T)
colnames(cf_td_citations) = c("Year", "Citations")
cf_td_citations = data.frame(cf_td_citations, Paper = "CFTD")
cf_td_citations %>%
  ggplot(aes(x = Year, y = Citations)) + geom_point() + theme_bw()


bind_rows(pmf_citations, cf_td_citations) %>%
  ggplot(aes(x = Year, y = Citations, col = Paper)) +
  geom_point() + geom_line() + theme_bw() +
  scale_x_continuous(breaks = pmf_citations$Year) +
  ggtitle("Yearly Citations") +
  theme(axis.text.x=element_text(color = "black",
                                 size=11,
                                 angle=40, vjust=.8,
                                 hjust=0.8),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())


library(tikzDevice)
tikz(file = "citations_plot_cftd_pmf.tex", width = 10, height = 5)

plot = bind_rows(pmf_citations, cf_td_citations) %>%
  ggplot(aes(x = Year, y = Citations, col = Paper)) +
  geom_point() + geom_line() + theme_bw() +
  scale_x_continuous(breaks = pmf_citations$Year) +
  ggtitle("Yearly Citations") +
  theme(axis.text.x=element_text(color = "black",
                                 size=11,
                                 angle=40, vjust=.8,
                                 hjust=0.8),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())

print(plot)
dev.off()
