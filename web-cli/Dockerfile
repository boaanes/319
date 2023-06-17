# React Frontend Dockerfile
# Use a Node.js image to build our React application
FROM node:16.20.0-alpine as build

# Set the working directory
WORKDIR /app

# Add `package.json` and `yarn.lock` before other files
# Utilize caching to save build time
COPY package.json yarn.lock ./

# Install all dependencies
RUN yarn install --frozen-lockfile

# Copy the other files
COPY . .

# Build the project
RUN yarn build

# Serve the app using a minimal nginx image
FROM nginx:1.21.0-alpine

# Copy the build files from the build stage
COPY --from=build /app/build /usr/share/nginx/html

# Expose the listening port
EXPOSE 80

# Run nginx
CMD ["nginx", "-g", "daemon off;"]
