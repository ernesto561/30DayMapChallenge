options(rgl.useNULL = TRUE)
library(rgl)
library(biscale)
library(terra)
library(tidyverse)
library(tmap)
library(showtext)
library(cowplot)

sysfonts::font_add_google("Roboto")
showtext_auto()
showtext_opts(dpi = 300)



elev <- terra::rast("input/continuous/els_alos/dem_els.sdat") |> 
  as.data.frame(xy = TRUE) |>
  rename(z = `dem_els`)

target_crs <- "EPSG:32616"

# Classify the temperature and precipitation data into bivariate 
#nclasses using the 'biscale' package
# 'style = "quantile"' divides data into quantiles, and 'dim = 4' 
#ncreates 4 classes for each variable, resulting in 16 bivariate categories
data <- bi_class(pred_xgb,
                 x = prob, 
                 y = sd,
                 keep_factors = TRUE,
                 style = "fisher", dim = 3)


# Set the color palette for the bivariate map
pallet <- "BlueOr"

# Create map WITHOUT titles (for 3D rendering)
map_no_titles <- ggplot() +
  theme_void(base_size = 14, base_family = "Roboto") +
  geom_raster(data = data, mapping = aes(x = x, y = y, fill = bi_class), 
              alpha = 1, color = NA, linewidth = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = pallet, dim = 3, flip_axes = FALSE, rotate_pal = FALSE) +
  coord_fixed(expand = FALSE) +
  coord_sf(crs = target_crs) +
  theme(plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA))

# Create final plot for 3D WITHOUT titles
finalPlot_no_titles <- ggdraw() +
  draw_plot(map_no_titles, 0, 0, 1, 1) +
  theme(plot.background = element_rect(fill = "white", color = NA))

# DEM map without titles
dem_map_no_titles <- ggplot(
  elev, aes(x = x, y = y, fill = z)
) +
  geom_raster() +
  scale_fill_gradientn(colors = "white") +
  guides(fill = "none") +
  coord_sf(crs = target_crs) +
  theme_void(base_size = 14, base_family = "Roboto") +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA))

# Close all rgl devices and restart
try(rgl::rgl.close(), silent = TRUE)
try(rgl::clear3d(), silent = TRUE)

# RENDER 3D SCENE
rayshader::plot_gg(
  ggobj = finalPlot_no_titles,
  ggobj_height = dem_map_no_titles,
  width = 8,
  height = 5,
  windowsize = c(2000, 1250),
  scale = 50,
  shadow = FALSE,
  shadow_intensity = 1,
  phi = 87,
  theta = 0, 
  zoom = 0.75,
  multicore = TRUE
)


rayshader::render_camera(phi = 87, theta = 0, zoom = 0.75)

# 9. LIGHTS
#----------

url <- "https://dl.polyhaven.org/file/ph-assets/HDRIs/hdr/4k/brown_photostudio_02_4k.hdr"
hdri_file <- basename(url)

download.file(
  url = url,
  destfile = hdri_file,
  mode = "wb"
)

# 10. RENDER 3D OBJECT
#---------------------

# Render with camera parameters explicitly set
rayshader::render_highquality(
  filename = "lluvia_temp_els_3d.png",
  preview = FALSE,
  light = FALSE,
  environment_light = hdri_file,
  intensity = 1,
  rotate_env = 90,
  parallel = TRUE,
  width = 4000, 
  height = 2500,
  samples = 512,
  sample_method = "sobol",
  interactive = FALSE
)


# Now add 2D titles and legend as overlay
library(magick)

# Read the 3D rendered image
img_3d <- image_read("lluvia_temp_els_3d.png")

# Get dimensions
img_info <- image_info(img_3d)
img_width <- img_info$width
img_height <- img_info$height

breaks2 <- bi_class_breaks(temp_ppt_df, x = temp, 
                           y = prec, style = "fisher", 
                           dim = 3, dig_lab = c(x = 2, y = 4), split = TRUE)

# Create SMALLER legend
legend_smaller <- bi_legend(pal = pallet,   
                            flip_axes = FALSE,
                            rotate_pal = FALSE,
                            dim = 3,
                            xlab = "Temperatura (°C)",
                            ylab = "Lluvia (mm)",
                            breaks = breaks2,
                            size = 7) + # Smaller size
  theme(plot.background = element_rect(fill = "white", color = NA))

ggsave("temp_legend.png", legend_smaller, width = 1.5, height = 1.5, dpi = 300, bg = "transparent")
img_legend <- image_read("temp_legend.png")

# Resize legend to be proportional to final image
img_legend <- image_scale(img_legend, paste0(round(img_width * 0.15)))

