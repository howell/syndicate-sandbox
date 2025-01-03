# Use an official Racket image as the base
FROM racket/racket:8.9

# Set the working directory inside the container
WORKDIR /app

# Copy the Racket package files to the container
COPY . /app

# Install the Racket package
RUN raco pkg install --auto /app

# Expose the port your service listens on
EXPOSE 4001

ENV PLTSTDERR="error info@sandbox-server"

# Command to start the service listening on all IP addrs
CMD ["racket", "main.rkt", "-l"]
