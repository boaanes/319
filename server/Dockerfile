# We're going to use the official haskell stack docker image as base
FROM haskell:9.0.2

# Create a directory for our application
RUN mkdir /app
WORKDIR /app

# We copy only the necessary files to install dependencies first, 
# to take advantage of Docker's layer caching
COPY ./stack.yaml /app/
COPY ./package.yaml /app/

# Install GHC, which is needed for the next step
RUN stack setup

# Install all dependencies in the package.yaml
RUN stack build --dependencies-only

# Copy the rest of the source code
COPY ./app /app/app
COPY ./src /app/src
COPY ./src/examples /app/src/examples

# Compile our application
RUN stack build

# Our command runs the compiled binary
CMD ["stack", "exec", "app-exe"]
