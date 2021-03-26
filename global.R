###############################################
# HD - Shiny App - global.R
###############################################

#===== LOAD PACKAGES -----------------------------------------------------------------------------------------------------------
pkg <- c(
    # SHINY
    'bsplus', 'colourpicker', 'shiny', 'shinycssloaders', 'shinyDND', 'shinyjqui', 'shinyjs', 'shinymaterial', 'shinythemes', 'shinyWidgets', 
    # DATA WRANGLING
    'data.table', 'fst', 'htmltools', 'scales',
    # DATA VIZ
    'bpexploder', 'Cairo', 'circlize', 'd3heatmap', 'DT', 'extrafont', 'geofacet', 'GGally', 'ggplot2', 'ggiraph', 'ggrepel', 'ggthemes', 'RColorBrewer'
)
invisible( lapply(pkg, require, char = TRUE) )

#===== GENERAL OPTIONS ----------------------------------------------------------------------------------------------------------
options(spinner.color = '#e5001a', spinner.size = 1, spinner.type = 4)
options(bitmapType = 'cairo', shiny.usecairo = TRUE)
options(knitr.table.format = 'html')
options(shiny.sanitize.errors = false) # rememeber to change it to TRUE when in production

#===== LOAD DATA ----------------------------------------------------------------------------------------------------------------
pub_path <- Sys.getenv('PUB_PATH')
data_path <- file.path(pub_path, 'shiny_apps', 'wd_hdi')
dts <- read_fst(file.path(data_path, 'dataset'), as.data.table = TRUE)
cnt <- read_fst(file.path(data_path, 'countries'), as.data.table = TRUE)
ind <- read_fst(file.path(data_path, 'indicators'), as.data.table = TRUE)
ftn <- read_fst(file.path(data_path, 'footnotes'), as.data.table = TRUE)
ind_ftn <- read_fst(file.path(data_path, 'ind_foot'), as.data.table = TRUE)

bnd <- readRDS('data/boundaries')
mpt

# List of palettes to be used with ColourBrewer package:  
palette.lst <- list(
    'SEQUENTIAL' = c( # ordinal data where (usually) low is less important and high is more important
        'Blues' = 'Blues', 'Blue-Green' = 'BuGn', 'Blue-Purple' = 'BuPu', 'Green-Blue' = 'GnBu', 'Greens' = 'Greens', 'Greys' = 'Greys',
        'Oranges' = 'Oranges', 'Orange-Red' = 'OrRd', 'Purple-Blue' = 'PuBu', 'Purple-Blue-Green' = 'PuBuGn', 'Purple-Red' = 'PuRd', 'Purples' = 'Purples',
        'Red-Purple' = 'RdPu', 'Reds' = 'Reds', 'Yellow-Green' = 'YlGn', 'Yellow-Green-Blue' = 'YlGnBu', 'Yellow-Orange-Brown' = 'YlOrBr',
        'Yellow-Orange-Red' = 'YlOrRd'
    ), 
    'DIVERGING' = c(  # ordinal data where both low and high are important (i.e. deviation from some reference "average" point)
        'Brown-Blue-Green' = 'BrBG', 'Pink-Blue-Green' = 'PiYG', 'Purple-Red-Green' = 'PRGn', 'Orange-Purple' = 'PuOr', 'Red-Blue' = 'RdBu', 'Red-Grey' = 'RdGy',
        'Red-Yellow-Blue' = 'RdYlBu', 'Red-Yellow-Green' = 'RdYlGn', 'Spectral' = 'Spectral'
    ),  
    'QUALITATIVE' = c(  # categorical/nominal data where there is no logical order
        'Accent' = 'Accent', 'Dark2' = 'Dark2', 'Paired' = 'Paired', 'Pastel1' = 'Pastel1', 'Pastel2' = 'Pastel2',
        'Set1' = 'Set1', 'Set2' = 'Set2', 'Set3' = 'Set3'
    )
)

# list of options for charts
point.shapes <- c('circle' = 21, 'square' = 22, 'diamond' = 23, 'triangle up' = 24, 'triangle down' = 25)
line.types <- c('dashed', 'dotted', 'solid', 'dotdash', 'longdash', 'twodash')
face.types <- c('plain', 'bold', 'italic', 'bold.italic')
val.lbl.pos <- list(
    'Inside'  = list('Vertical' = c(0.5,  1.5), 'Horizontal' = c( 1.2, 0.2) ),
    'Outside' = list('Vertical' = c(0.4, -0.3), 'Horizontal' = c(-0.2, 0.2) )
)
lbl.format <- function(y, type, is.pct = FALSE){
    if(type == 1){ 
        format(y, big.mark = ',', nsmall = 0)
    } else if(type == 2){ 
        if(is.pct){ 
            paste0(format(round(100 * y, 2), nsmall = 2), '%')
        } else { 
            format(y, big.mark = ',', nsmall = 0)
        }    
    } else {
        format(y, nsmall = 2)
    }
}

# list of options for labels in maps
lbl.options <- labelOptions(
    textsize = '12px', 
    direction = 'right', 
    sticky = FALSE, 
    opacity = 0.8,
    offset = c(60, -40), 
    style = list(
        'font-weight' = 'normal', 
        'padding' = '2px 6px'
    )
)
# list of options for highlighted polygons in maps
hlt.options <- highlightOptions(
    color = 'white',
    weight = 6,
    bringToFront = TRUE
)

# list of classification methods, to be used with classInt and ColorBrewer packages 
class.methods <- c(
#    'Fixed' = 'fixed',                  # need an additional argument fixedBreaks that lists the n+1 values to be used
    'Equal Intervals' = 'equal',        # the range of the variable is divided into n part of equal space
    'Quantiles' = 'quantile',           # each class contains (more or less) the same amount of values
    'Pretty Integers' = 'pretty',       # sequence of about ‘n+1’ equally spaced ‘round’ values which cover the range of the values in ‘x’. The values are chosen so that they are 1, 2 or 5 times a power of 10.
#    'Natural Breaks' = 'jenks',         # seeks to reduce the variance within classes and maximize the variance between classes
    'Hierarchical Cluster' = 'hclust',  # Cluster with short distance
    'K-means Cluster' = 'kmeans'        # Cluster with low variance and similar size
)

# list of maptiles as background for maps
tiles.lst <- as.list(mpt[, provider])
names(tiles.lst) <- mpt[, name]

# add text at the left of the upper navbar
navbarPageWithText <- function(..., text) {
    navbar <- navbarPage(...)
    textEl <- tags$p(class = "navbar-text", text)
    navbar[[3]][[1]]$children[[1]] <- tagAppendChild( navbar[[3]][[1]]$children[[1]], textEl)
    navbar
}

# return correct spacing for axis labels rotation
lbl.plt.rotation = function(angle, position = 'x'){
    positions = list(x = 0, y = 90, top = 180, right = 270)
    rads  = (angle - positions[[ position ]]) * pi / 180
    hjust = 0.5 * (1 - sin(rads))
    vjust = 0.5 * (1 + cos(rads))
    element_text(angle = angle, vjust = vjust, hjust = hjust)
}

# global style for ggplot charts
my.ggtheme <- function(g, 
                       xaxis.draw = FALSE, yaxis.draw = FALSE, axis.draw = FALSE, ticks.draw = FALSE, axis.colour = 'black', axis.size = 0.1,
                       hgrid.draw = FALSE, vgrid.draw = FALSE, grids.colour = 'black', grids.size = 0.1, grids.type = 'dotted',
                       labels.rotation = c(45, 0), labels.rotate = FALSE, 
                       bkg.colour = 'white', font.size = 6, ttl.font.size.mult = 1.2, ttl.face = 'bold',
                       legend.pos = 'bottom', plot.border = FALSE, font.family = 'Arial'
){
    g <- g + theme(
        text             = element_text(family = font.family),
        plot.title       = element_text(hjust = 0, size = rel(1.2) ),  # hjust: 0-left, 0.5-center, 1-right
        plot.background  = element_blank(),
        plot.margin      = unit(c(1, 0.5, 0, 0.5), 'lines'),  # space around the plot as in: TOP, RIGHT, BOTTOM, RIGHT
        plot.caption     = element_text(size = 8, face = 'italic'),
        axis.line        = element_blank(),
        axis.ticks       = element_blank(),
        axis.text        = element_text(size = font.size, color = axis.colour),
        axis.text.x      = element_text(angle = labels.rotation[1], hjust = 1), # vjust = 0.5),
        axis.text.y      = element_text(angle = labels.rotation[2]), # , hjust = , vjust = ),
        axis.title       = element_text(size = font.size * (1 + ttl.font.size.mult), face = ttl.face),
        axis.title.x     = element_text(vjust = -0.3), 
        axis.title.y     = element_text(vjust = 0.8, margin = margin(0, 10, 0, 0) ),
        legend.text      = element_text(size = 6),
        legend.title     = element_text(size = 8), 
        legend.title.align = 1,
        legend.position  = legend.pos,
        legend.background = element_blank(), 
        legend.spacing   = unit(0, 'cm'),
        #                legend.key = element_blank(), 
        legend.key.size  = unit(0.2, 'cm'),
        legend.key.height = unit(0.4, 'cm'),      
        legend.key.width = unit(1, 'cm'),
        panel.background = element_rect(fill = bkg.colour, colour = bkg.colour), 
        panel.border     = element_blank(),
        panel.grid       = element_blank(),
        panel.spacing.x  = unit(3, 'lines'),
        panel.spacing.y  = unit(2, 'lines'),
        strip.text       = element_text(hjust = 0.5, size = font.size * (1 + ttl.font.size.mult), face = ttl.face),
        strip.background = element_blank()
    )
    if(plot.border) g <- g + theme( panel.border = element_rect(colour = axis.colour, size = axis.size, fill = NA) )
    if(axis.draw){
        g <- g + theme( axis.line = element_line(color = axis.colour, size = axis.size ) )
    } else {
        if(xaxis.draw) g <- g + theme( axis.line.x = element_line(color = axis.colour, size = axis.size ) )
        if(yaxis.draw) g <- g + theme( axis.line.y = element_line(color = axis.colour, size = axis.size ) )
    }
    if(ticks.draw)  g <- g + theme( axis.ticks = element_line(color = axis.colour, size = axis.size ) )
    if(hgrid.draw & vgrid.draw){
        g <- g + theme( panel.grid.major = element_line(colour = grids.colour, size = grids.size, linetype = grids.type ) )
    } else{
        if(vgrid.draw) g <- g + theme( panel.grid.major.x = element_line(colour = grids.colour, size = grids.size, linetype = grids.type ) ) 
        if(hgrid.draw) g <- g + theme( panel.grid.major.y = element_line(colour = grids.colour, size = grids.size, linetype = grids.type ) )
    }
    if(labels.rotate){
        g <- g + theme( axis.text.x = element_text(hjust = 1, angle = 45 ) )
    }
    return(g)
}
