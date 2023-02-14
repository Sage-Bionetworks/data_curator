#!/usr/bin/bash

# Pass environment variable to Shiny
echo "" >> .Renviron
echo R_CONFIG_ACTIVE=$R_CONFIG_ACTIVE >> .Renviron

# Now run the base start-up script
./startup.sh
