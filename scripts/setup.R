
# -----------------------------------------------------------------------------
# 0. LIBRERÍAS REQUERIDAS
# -----------------------------------------------------------------------------

library(paqueteMODELOS)
library(ggplot2)
library(highcharter)
library(purrr)
library(scales)
library(corrplot)
library(dplyr)
library(knitr)
library(paqueteMODELOS)
library(sf)
library(leaflet)
library(kableExtra)


# Set global chunk options for knitr (R Markdown)
# echo = FALSE means that the code won't be shown in the final document
knitr::opts_chunk$set(echo = FALSE)

# Install devtools package (only needed once, so it is commented out)
# install.packages("devtools")

# Install the paqueteMODELOS package directly from GitHub (force reinstall if needed)
# devtools::install_github("centromagis/paqueteMODELOS", force = TRUE)

# Load the paqueteMODELOS library into the session
library(paqueteMODELOS)

# Load the "vivienda" dataset included in the package
data("vivienda")

# Display the structure of the dataset (variables, types, etc.)
str(vivienda)

# Create a directory named "data" if it does not already exist
dir.create("data", showWarnings = FALSE)

# Save a data frame as a CSV file inside the "data" folder
# NOTE: 'df' must exist previously, otherwise this will produce an error
write.csv(df, "data/raw.csv")


