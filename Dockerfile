# Describes how to build the image
FROM rstudio/plumber

# Install system dependencies
RUN apt-get update -qq && apt-get install -y libssl-dev libcurl4-gnutls-dev libpng-dev libpng-dev pandoc 

# Install required R packages
RUN R -e "install.packages(c('plumber', 'tidyverse', 'tidymodels', 'ranger'))" 

# Copy local project files into constainer
COPY water_potability.csv water_potability.csv 
COPY best_model.rds   best_model.rds
COPY API.R API.R

# Expose port 8000
EXPOSE 8000 

# Container entrypint to run plumber API
ENTRYPOINT ["R", "-e", \ 
"pr <- plumber::plumb('API.R'); pr$run(host='0.0.0.0', port=8000)"]