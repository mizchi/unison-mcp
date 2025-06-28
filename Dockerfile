FROM ubuntu:22.04

# Install dependencies
RUN apt-get update && apt-get install -y \
    curl \
    && rm -rf /var/lib/apt/lists/*

# Install Unison
RUN curl -L https://github.com/unisonweb/unison/releases/latest/download/ucm-linux.tar.gz | tar -xz -C /usr/local/bin

# Copy Unison codebase
COPY .unison /app/.unison
COPY *.u /app/
WORKDIR /app

# Run the application
CMD ["ucm", "run", "main"]