FROM node:22-alpine

RUN apk update && apk add python3 make gcc g++

USER node
WORKDIR /app

COPY --chown=node package*.json /app/
RUN npm ci

COPY --chown=node test01 /app/test01

EXPOSE 8080 8443

ENTRYPOINT ["npx", "haraka", "-c", "/app/test01"]
