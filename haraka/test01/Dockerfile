FROM node:22-alpine

RUN npm install -g haraka

USER node
WORKDIR /app

COPY package*.json /app/
RUN npm ci

COPY config /app/config
COPY plugins /app/plugins
COPY queue /app/queue

EXPOSE 8080 8443

ENTRYPOINT ["npx", "haraka", "-c", "/app"]
