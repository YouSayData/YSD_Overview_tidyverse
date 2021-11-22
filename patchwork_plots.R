
# libraries ---------------------------------------------------------------

library(tidyverse)
library(patchwork)


# Easy layout -------------------------------------------------------------

p1 <- ggplot(mtcars) + geom_point(aes(mpg, disp))
p2 <- ggplot(mtcars) + geom_boxplot(aes(gear, disp, group = gear))
p3 <- ggplot(mtcars) + geom_smooth(aes(disp, qsec))
p4 <- ggplot(mtcars) + geom_bar(aes(carb))

(p1 + p2 + p3) / p4

# plot in plot ------------------------------------------------------------

p1 + inset_element(p2, left = 0.6, bottom = 0.6, right = 1, top = 1)

# style it up -------------------------------------------------------------

patchwork <- (p1 + p2) / p3
patchwork + plot_annotation(tag_levels = c('A', '1'), tag_prefix = 'Fig. ',
                            tag_sep = '.',
                            title = 'The surprising truth about mtcars',
                            subtitle = 'These 3 plots will reveal yet-untold secrets about our beloved data-set',
                            caption = 'Disclaimer: None of these plots are insightful',
                            theme = theme(plot.title = element_text(size = 18))) & 
  theme(text = element_text('mono'),
        plot.tag.position = c(0, 1),
        plot.tag = element_text(size = 8, hjust = 0, vjust = 0))

