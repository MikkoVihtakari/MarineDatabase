#' @title Temperature-salinity (T-S) plot from a data frame using ggplot2
#' @description Makes a T-S plot from salinity and temperature data. Optimized for Kongsfjorden.
#' @param dt data frame.
#' @param temp_col character specifying the column name that contains temperature information. Make sure that the temperatures are potential temperatures (theta) instead of in-situ tempretatures.
#' @param sal_col character specifying the column name that contains salinity information. Make sure that the salinities are practical salinities (theta).
#' @param color character indicating the mapping variable for color. If \code{"watertype"} the cots are colored based on water type using a prededined palette.
#' @param zoom logical indicating whether the x- and y-axis should be limited to data. If \code{FALSE}, the entire water mass diagram from Cottier et al. is shown.
#' @param margin_distr logical indicating whether kernel density estimates of temperature and salinity should be added to margins of the plot.
#' @param nlevels Number of automatically-selected isopycnal levels. Set to 0 to remove isopycnals.
#' @references Cottier, F., Tverberg, V., Inall, M., Svendsen, H., Nilsen, F., Griffiths, C., 2005. Water mass modification in an Arctic fjord through cross-shelf exchange: The seasonal hydrography of Kongsfjorden, Svalbard. J. Geophys. Res. 110, C12005. doi:10.1029/2004JC002757
#' @seealso \code{\link{define_water_type}}
#' @import ggplot2 patchwork 
#' @export

#dt <- ctd; temp_col = "theta"; sal_col = "salinity"; xlim = NULL; ylim = NULL; color = "watertype"; zoom = TRUE; margin_distr = FALSE; nlevels = 4
ts_plot <- function(dt, temp_col = "theta", sal_col = "salinity", xlim = NULL, ylim = NULL, color = "watertype", zoom = TRUE, margin_distr = FALSE, nlevels = 6) {

## Water types ####

WM <- define_water_type(return_water_type_list = TRUE)
dt <- define_water_type(dt, temp_col = temp_col, sal_col = sal_col, bind = TRUE)

## Axis limits

if(is.null(xlim) & zoom) {
  xbreaks <- pretty(range(dt[[sal_col]]), n = nlevels)
  xlim <- range(xbreaks)
} else if(is.null(xlim)) {
  xbreaks <- pretty(range(c(dt[[sal_col]], c(32, 35))), n = nlevels)
  xlim <- range(xbreaks)
}

if(is.null(ylim) & zoom) {
  ybreaks <- pretty(range(dt[[temp_col]]), n = nlevels)
  ylim <- range(ybreaks)
} else if(is.null(ylim)) {
  ybreaks <- pretty(range(c(dt[[temp_col]], c(-2, 8))), n = nlevels)
  ylim <- range(ybreaks)
}

## Isopycnals

if(nlevels > 0) {
  
  if(zoom) {
    rho <- swRho(salinity = xlim, temperature = ylim, pressure = rep(1, length(xlim))) - 1000
    rho_breaks <- pretty(range(rho), n = nlevels-1)
  } else {
    rho_breaks <- seq(25, 28, length.out = nlevels)
  }
  
  temp_breaks <- seq(from = ylim[1], to = ylim[2], by = 0.1)

  isopycs <- lapply(seq_along(rho_breaks), function(i) {
    data.frame(temp = temp_breaks, rho = rho_breaks[i], sal = swSTrho(temperature = temp_breaks, density = rho_breaks[i], pressure = 1))
  })

  isopycs <- do.call(rbind, isopycs)

} else {
  isopycs <- data.frame(temp = NA, rho = NA, sal = NA)
}

## Water mass polygons and text


TMP <- lapply(unique(WM$Abb), function(j) {
  tmp <- subset(WM, Abb == j)
  
  if(j != "ArW") {
    poly <- data.frame(Type = tmp$Type, Abb = tmp$Abb, x = c(tmp$x.min, tmp$x.max, tmp$x.max, tmp$x.min), y = c(tmp$y.min, tmp$y.min, tmp$y.max, tmp$y.max))
  } else {
    poly <- data.frame(Type = tmp$Type, Abb = tmp$Abb, x = c(tmp$x.min, 34.4, 34.4, tmp$x.max, tmp$x.max, tmp$x.min), y = c(tmp$y.min, tmp$y.min, -0.5, -0.5, tmp$y.max, tmp$y.max))
  }
  
  text <- data.frame(Type = tmp$Type, Abb = tmp$Abb, x = tmp$x.min, y = tmp$y.max)
  
  list(poly = poly, text = text)
})

WMpoly <- do.call(rbind, lapply(TMP, function(j) j$poly))
WMtext <- do.call(rbind, lapply(TMP, function(j) j$text))

if(zoom) {

  WMpoly <- WMpoly[WMpoly$Abb %in% as.character(unique(dt$watertype)),]
  WMpoly <- droplevels(WMpoly)

  WMtext <- WMtext[WMtext$Abb %in% as.character(unique(dt$watertype)),]
  WMtext <- droplevels(WMtext)

  WMtext$y <- ifelse(WMtext$y > ylim[2], ylim[2], WMtext$y)
  WMtext$y <- ifelse(WMtext$y < ylim[1], ylim[1], WMtext$y)
  WMtext$x <- ifelse(WMtext$x > xlim[2], xlim[2], WMtext$x)
  WMtext$x <- ifelse(WMtext$x < xlim[1], xlim[1], WMtext$x)
}

## Main plot ####

if(nlevels > 0) {
  p <- ggplot() + 
    geom_line(data = isopycs, aes(x = sal, y = temp, group = rho), color = "grey60", size = LS(0.5)) +
    scale_x_continuous("Practical salinity", breaks = xbreaks, 
    sec.axis = sec_axis(~., breaks = isopycs[isopycs$temp == ylim[2], "sal"], labels = isopycs[isopycs$temp == ylim[2], "rho"], name = "Density"))
} else {
  p <- ggplot() + scale_x_continuous("Practical salinity", breaks = xbreaks)
}

polycol <- "grey30"

p <- p + 
  geom_polygon(data = WMpoly, aes(x = x, y = y, group = Abb), fill = NA, color = polycol, size = LS(0.5)) + 
  geom_point(data = dt, aes_string(x = sal_col, y = temp_col, color = color), shape=1, alpha = 0.6, size = 3, stroke = LS(1)) + 
  geom_text(data = WMtext, aes(x = x, y = y, label = Abb), size = FS(8), vjust = 1.2, hjust = -0.3, color = polycol) + 
  coord_cartesian(xlim = xlim, ylim = ylim, expand = FALSE) +
  scale_y_continuous(expression(paste("Potential temperature (", ~degree, "C", ")")), breaks = ybreaks) + 
  theme_classic(base_size = 10) + 
  theme(axis.line = element_line(size = LS(0.5)),
    axis.ticks = element_line(size = LS(0.5)), 
    panel.border = element_rect(color = "black", size = LS(1), fill = NA))

## Marginal plot for x-axis ####

px <- ggplot(data = dt, aes_string(x = sal_col, fill = color)) +
  geom_density(alpha = 0.5, size = 0.2) +
  coord_cartesian(xlim = xlim, expand = FALSE) + 
  theme_classic(base_size = 8) +
  theme(axis.title = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    legend.position = "none")

## Marginal plot for y-axis

py <- ggplot(data = dt, aes_string(x = temp_col, fill = color)) +
  geom_density(alpha = 0.5, size = 0.2) +
  coord_flip(xlim = ylim, expand = FALSE) + 
  theme_classic(base_size = 8) +
  theme(axis.title = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    legend.position = "none")

if(color == "watertype") {

WCOLS <- c("AWs" = "#D44F56", "SWs" = "#649971", "ArWs" = "#3881AC", "AW" = "#B6434A", "TAW" = "#FF5F68", "IW" = "#82C893", "SW" = "#517D5B", "WCW" = "#B27DA6", "ArW" = "#449BCF", "Other" = "grey50")

 p <- p + scale_color_manual(name = "Water type", values = WCOLS)

 if(margin_distr) {
 px <- px + scale_fill_manual(name = "Water type", values = WCOLS) 
 py <- py + scale_fill_manual(name = "Water type", values = WCOLS) 
   }
 
}

if(margin_distr) {
  p <- p + theme(legend.position = "none")
  px + plot_spacer() + p + py + plot_layout(ncol = 2, nrow = 2, heights = c(2,5), widths = c(5,1))
} else {
  p
}

}
