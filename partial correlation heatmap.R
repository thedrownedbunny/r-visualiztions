# input data

af <- mtcars[,1:6]

# for this example I'm using only 6 variables from the mtcars dataset:
# mpg, cyl, disp, hp, drat, wt
# more information about the dataset itself and the variables used you can find
#in the documentation:

?mtcars
help(mtcars)

# I'm going to check partial correlation between the variables
# And then visualize the result by creating a heatmap

# Before anlasys I'm creating a function
# for deleting the upper half of the matrix

get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

# calculating partial correlation coefficients using a ocor() function

library(ppcor)
pcor_mat <- round(pcor(af)$estimate, 3)

# filling the upper half with NAs

pcor_mat <- get_upper_tri(pcor_mat)

# melting is needed in order to visualize the result

library(reshape2)
melted_pcor <- melt(pcor_mat, na.rm = T)

# plotting a heatmap for correlation coefficient

library(ggplot2)

ggplot(melted_pcor, aes(x = Var2, y = Var1, fill = value))+
  geom_tile(color = 'white') +
  scale_fill_gradient2(high = 'green4', mid = 'yellow1', low = 'orangered',
                       midpoint = 0, limit = c(-1,1), space = 'Lab',
                       name = 'Частный\nкоэффициент\nкорреляции')+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 15, hjust = 1),
        axis.text.y = element_text(size = 15)) +
  coord_fixed() +
  geom_text(aes(Var2, Var1, label = value), color = 'grey28', size = 6) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        legend.justification = c(1,0),
        legend.position = c(0.55, 0.7),
        legend.direction = 'horizontal') +
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = 'top', title.hjust = 0))
