FROM haskell:9.2.5

COPY . /app
WORKDIR /app

RUN stack setup
RUN stack build --install-ghc
RUN stack build --only-dependencies
RUN stack build

EXPOSE 8080

CMD stack ghci