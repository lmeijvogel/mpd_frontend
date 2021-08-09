FROM alpine:3.10.3

MAINTAINER Michael Porter <mike@codesimple.net>

RUN wget -O - 'https://github.com/elm/compiler/releases/download/0.19.1/binary-for-linux-64-bit.gz' \
    | gunzip -c >/usr/local/bin/elm
RUN chmod +x /usr/local/bin/elm
RUN apk add --no-cache nodejs=10.24.1-r0 npm bash
RUN npm install elm-live

# ENTRYPOINT []

WORKDIR /app

CMD ["node_modules/.bin/elm-live", "--host", "0.0.0.0", "--proxy-host", "http://api:9292", "--proxy-prefix", "/api", "src/Main.elm"]

######################################
# FROM mitchty/alpine-ghc:8.0.2 as build

# ENV \
  # ELM_VERSION=0.19 \
  # COMPONENT_VERSION=0.19.0

# ENV \
  # PATH=/Elm-Platform/${ELM_VERSION}/.cabal-sandbox/bin:${PATH}

# RUN \
  # mkdir Elm-Platform && \
  # apk update && \
  # apk add --no-cache git musl-dev ncurses-dev zlib-dev

# WORKDIR /Elm-Platform/${ELM_VERSION}

# RUN git clone https://github.com/elm/compiler.git && \
    # git --git-dir=./compiler/.git --work-tree=./compiler checkout --quiet ${COMPONENT_VERSION}

# RUN \
 # cd compiler && \
 # echo "split-objs: True" > cabal.config && \
 # cabal update && \
 # cabal sandbox init && \
 # cd compiler/src && \
 # cabal sandbox init --sandbox ../../.cabal-sandbox && \
 # cd ../../builder/src && \
 # cabal sandbox init --sandbox ../../.cabal-sandbox

# WORKDIR /Elm-Platform/${ELM_VERSION}/compiler

# RUN cabal install -j --ghc-options="-w" --max-backjumps=-1 .



# FROM alpine:3.8

# ENV \
  # ELM_VERSION=0.19 \
  # REACTOR_PORT=8000

# ENV PATH=/elm/${ELM_VERSION}/bin:${PATH}

# COPY --from=build /Elm-Platform/${ELM_VERSION}/compiler/.cabal-sandbox/bin/elm /elm/${ELM_VERSION}/bin/elm

# RUN \
  # apk update && \
  # apk add --no-cache gmp ncurses-libs nodejs

# EXPOSE ${REACTOR_PORT}

# WORKDIR /opt/app

# ENTRYPOINT ["elm-live"]

# CMD ["--host", "0.0.0.0", "--proxy-host", "http://api:9292", "--proxy-prefix", "/api", "src/Main.elm"]

###########################################
# FROM node:15.14.0

# ENV HOME=/app
# WORKDIR /app

# RUN npm install elm elm-make
# COPY elm.json package-lock.json /app/

# RUN node_modules/.bin/elm-make --yes

# COPY . .

# ENV SHELL=/bin/bash

# CMD npm run watch
