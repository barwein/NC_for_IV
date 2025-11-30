FROM rocker/r-ver:4.5.1

# Install Linux system dependencies required for Tidyverse & Devtools
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    cmake \
    git \
    && rm -rf /var/lib/apt/lists/*

# Set working directory
WORKDIR /project

# Copy requirements and setup script first (better caching)
COPY requirements.txt .
COPY setup_env.R .

# Run setup in STRICT mode
# Note: This relies on the modified setup_env.R defaulting to 'strict' in non-interactive mode
RUN Rscript setup_env.R

# Copy the rest of the project
COPY . .

# Default command: Run the main analysis
CMD ["Rscript", "main.R"]

