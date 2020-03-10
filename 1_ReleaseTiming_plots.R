#Goal: Plot variability of hatchery coho smolt releases timing.
#Input file was created by WSP Yr26 members.

library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(lubridate)
library(viridis)

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
custom_breaks <- seq(0,100,10) #CUSTOM TICKMARKS

ggplot(fish, aes(x = Ordinal_Day_From_Release, y = Days_to_Detect, group = Ordinal_Day_From_Release)) +
  stat_boxplot_custom(qs = c(0.1, 0.25, 0.5, 0.75, 0.9), geom = 'errorbar', linetype = 1, width = 0.3) +
  stat_boxplot_custom(qs = c(0.1, 0.25, 0.5, 0.75, 0.9), outlier.shape = 1, outlier.colour = "gray", width = 0.5) +
  facet_wrap(Water_Year ~ ., ncol = 1) +
  # geom_point(data = Fish.Growth.ALL.SH.summary,aes(x = SamplingMonth.y, y = MSGR_mean), size = 2) + #add mean point for each month
  theme_classic() +
  scale_y_continuous(limits = c(0,100), breaks = custom_breaks,
                     labels = every_nth(custom_breaks, 2, inverse = TRUE)) +
  ylab("Days to Detection") +
  xlab("Release Day") +
  theme(strip.background = element_blank(), strip.text.x = element_blank())


#violin plot behind boxplot with color (no outliers).
ggplot(fish, aes(x = Ordinal_Day_From_Release, y = Days_to_Detect, fill = Ordinal_Day_From_Release, group = Ordinal_Day_From_Release)) +
  geom_violin(width = 8) +
  stat_boxplot_custom(qs = c(0.1, 0.25, 0.5, 0.75, 0.9), outlier.shape = NA, width = 0.5, color = "grey", alpha = 0.2) +
  facet_wrap(Water_Year ~ ., ncol = 1) +
  # geom_point(data = Fish.Growth.ALL.SH.summary,aes(x = SamplingMonth.y, y = MSGR_mean), size = 2) + #add mean point for each month
  theme_classic() +
  scale_fill_viridis() +
  scale_y_continuous(limits = c(0,60), breaks = custom_breaks,
                     labels = every_nth(custom_breaks, 2, inverse = TRUE)) +
  ylab("Days to Detection") +
  xlab("Release Day") +
  theme(strip.background = element_blank(), strip.text.x = element_blank()) +
  theme(legend.position =  "none", plot.title = element_text(size = 11)) 

#Violin Boxplot by year
ggplot(fish, aes(x = Ordinal_Day_From_Release, y = Days_to_Detect, fill = Water_Year)) +
  geom_violin() +
  stat_boxplot_custom(qs = c(0.1, 0.25, 0.5, 0.75, 0.9), outlier.shape = NA, width = 0.2, color = "grey", alpha = 0.2) +
  facet_wrap(Water_Year ~ ., ncol = 1) +
  # geom_point(data = Fish.Growth.ALL.SH.summary,aes(x = SamplingMonth.y, y = MSGR_mean), size = 2) + #add mean point for each month
  theme_classic() +
  scale_fill_viridis() +
  scale_y_continuous(limits = c(0,60), breaks = custom_breaks,
                     labels = every_nth(custom_breaks, 2, inverse = TRUE)) +
  ylab("Days to Detection") +
  xlab("Release Day") +
  theme(strip.background = element_blank(), strip.text.x = element_blank()) +
  theme(legend.position =  "none", plot.title = element_text(size = 11)) 


####Disribution next to boxplot

"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

geom_flat_violin <- function(mapping = NULL, data = NULL, stat = "ydensity",
                             position = "dodge", trim = TRUE, scale = "area",
                             show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomFlatViolin,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      trim = trim,
      scale = scale,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomFlatViolin <-
  ggproto("GeomFlatViolin", Geom,
          setup_data = function(data, params) {
            data$width <- data$width %||%
              params$width %||% (resolution(data$x, FALSE) * 0.9)
            
            # ymin, ymax, xmin, and xmax define the bounding rectangle for each group
            data %>%
              group_by(group) %>%
              mutate(ymin = min(y),
                     ymax = max(y),
                     xmin = x,
                     xmax = x + width / 2)
            
          },
          
          draw_group = function(data, panel_scales, coord) {
            # Find the points for the line to go all the way around
            data <- transform(data, xminv = x,
                              xmaxv = x + violinwidth * (xmax - x))
            
            # Make sure it's sorted properly to draw the outline
            newdata <- rbind(plyr::arrange(transform(data, x = xminv), y),
                             plyr::arrange(transform(data, x = xmaxv), -y))
            
            # Close the polygon: set first and last point the same
            # Needed for coord_polar and such
            newdata <- rbind(newdata, newdata[1,])
            
            ggplot2:::ggname("geom_flat_violin", GeomPolygon$draw_panel(newdata, panel_scales, coord))
          },
          
          draw_key = draw_key_polygon,
          
          default_aes = aes(weight = 1, colour = "grey20", fill = "white", size = 0.5,
                            alpha = NA, linetype = "solid"),
          
          required_aes = c("x", "y")
  )


# Final plot inspired from @jbburant: https://gist.github.com/jbburant/b3bd4961f3f5b03aeb542ed33a8fe062

ggplot(fish, aes(x = Ordinal_Day_From_Release, y = Days_to_Detect, fill = Ordinal_Day_From_Release, group = Ordinal_Day_From_Release)) + 
  facet_wrap(Water_Year ~ ., ncol = 1) +
  geom_flat_violin(scale = "count", trim = FALSE, width = 10, position = position_nudge(.3)) +
  scale_fill_viridis() +
  stat_boxplot_custom(qs = c(0.1, 0.25, 0.5, 0.75, 0.9), outlier.shape = NA, width = 0.5, color = "black", alpha = 0.25) +
  # stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "pointrange") +
  # geom_dotplot(binaxis = "y", dotsize = 0.8, stackdir = "down", binwidth = 0.3, position = position_nudge(-0.025)) + 
  theme_classic() +
  theme(legend.position = "none") + 
  scale_y_continuous(limits = c(0,100), breaks = custom_breaks,
                     labels = every_nth(custom_breaks, 2, inverse = TRUE)) +
  ylab("Days to Detection") +
  xlab("Release Day") +
  theme(strip.background = element_blank(), strip.text.x = element_blank())



