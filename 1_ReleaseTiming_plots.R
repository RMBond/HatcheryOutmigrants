#Goal: Plot variability of hatchery coho smolt releases timing.
#Input file was created by WSP Yr26 members.

library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(lubridate)

####Read in data####
fish <- read.csv('data/Final_Data_All_Years20200306.csv',sep = ',') #18402 obs of 8 variables

#formatting
fish <- fish %>% 
  mutate(Release_Date = as.Date(Release_Date, format = '%m/%d/%Y'))  %>% 
  mutate(Date._of_Detect = as.Date(Date._of_Detect, format = '%m/%d/%Y'))

str(fish)

####Plot Setup Functions ####

#Custom tick mark labels (i.e. label every other one but tick mark each unit)
#https://stackoverflow.com/questions/34533472/insert-blanks-into-a-vector-for-e-g-minor-tick-labels-in-r/34533473#34533473

every_nth <- function(x, nth, empty = TRUE, inverse = FALSE) 
{
  if (!inverse) {
    if (empty) {
      x[1:nth == 1] <- ""
      x
    } else {
      x[1:nth != 1]
    }
  } else {
    if (empty) {
      x[1:nth != 1] <- ""
      x
    } else {
      x[1:nth == 1]
    }
  }
}

#Boxplot with wiskers being 10% and 90% pertentiles and outliers as open circles
# define the summary function
#https://stackoverflow.com/questions/4765482/changing-whisker-definition-in-geom-boxplot/4765608#4765608

#Function to change whiskers
stat_boxplot_custom <- function(mapping = NULL, data = NULL,
                                geom = "boxplot", position = "dodge",
                                ...,
                                qs = c(.05, .25, 0.5, 0.75, 0.95),
                                na.rm = FALSE,
                                show.legend = NA,
                                inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatBoxplotCustom,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      qs = qs,
      ...
    )
  )
}

StatBoxplotCustom <- ggproto("StatBoxplotCustom", Stat,
                             required_aes = c("x", "y"),
                             non_missing_aes = "weight",
                             
                             setup_params = function(data, params) {
                               params$width <- ggplot2:::"%||%"(
                                 params$width, (resolution(data$x) * 0.75)
                               )
                               
                               if (is.double(data$x) && !ggplot2:::has_groups(data) && any(data$x != data$x[1L])) {
                                 warning(
                                   "Continuous x aesthetic -- did you forget aes(group=...)?",
                                   call. = FALSE
                                 )
                               }
                               
                               params
                             },
                             
                             compute_group = function(data, scales, width = NULL, na.rm = FALSE, qs = c(.05, .25, 0.5, 0.75, 0.95)) {
                               
                               if (!is.null(data$weight)) {
                                 mod <- quantreg::rq(y ~ 1, weights = weight, data = data, tau = qs)
                                 stats <- as.numeric(stats::coef(mod))
                               } else {
                                 stats <- as.numeric(stats::quantile(data$y, qs))
                               }
                               names(stats) <- c("ymin", "lower", "middle", "upper", "ymax")
                               iqr <- diff(stats[c(2, 4)])
                               
                               outliers <- (data$y < stats[1]) | (data$y > stats[5])
                               
                               if (length(unique(data$x)) > 1)
                                 width <- diff(range(data$x)) * 0.9
                               
                               df <- as.data.frame(as.list(stats))
                               df$outliers <- list(data$y[outliers])
                               
                               if (is.null(data$weight)) {
                                 n <- sum(!is.na(data$y))
                               } else {
                                 # Sum up weights for non-NA positions of y and weight
                                 n <- sum(data$weight[!is.na(data$y) & !is.na(data$weight)])
                               }
                               
                               df$notchupper <- df$middle + 1.58 * iqr / sqrt(n)
                               df$notchlower <- df$middle - 1.58 * iqr / sqrt(n)
                               
                               df$x <- if (is.factor(data$x)) data$x[1] else mean(range(data$x))
                               df$width <- width
                               df$relvarwidth <- sqrt(n)
                               df
                             }
)





####Boxplot of movement ####
custom_breaks <- seq(1500,3500,250) #CUSTOM TICKMARKS

ggplot(fish, aes(x = Ordinal_Day_From_Release, y = log(Days_to_Detect), group = Ordinal_Day_From_Release)) +
  stat_boxplot_custom(qs = c(0.1, 0.25, 0.5, 0.75, 0.9), geom = 'errorbar', linetype = 1, width = 0.3) +
  stat_boxplot_custom(qs = c(0.1, 0.25, 0.5, 0.75, 0.9), outlier.shape = 1, outlier.colour = "gray", width = 0.5) +
  facet_wrap(Water_Year ~ ., ncol = 1) +
  # geom_point(data = Fish.Growth.ALL.SH.summary,aes(x = SamplingMonth.y, y = MSGR_mean), size = 2) + #add mean point for each month
  theme_classic() +
  # scale_y_continuous(limits = c(-6.9,11), breaks = custom_breaks, expand = c(0,0),
  #                    labels = every_nth(custom_breaks, 2, inverse = TRUE)) +
  ylab("Log(Days to Detection)") +
  xlab("Release Day") +
  theme(strip.background = element_blank(), strip.text.x = element_blank())
  




