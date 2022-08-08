FROM alpine:3.12

# All but curl and zlib-dev are for ghcup
RUN apk add --no-cache zlib-dev curl gcc g++ gmp-dev ncurses-dev libffi-dev make xz tar perl

WORKDIR /pastebin-haskell

RUN curl 'https://downloads.haskell.org/~ghcup/x86_64-linux-ghcup' >/ghcup

RUN chmod +x /ghcup && \
    /ghcup install ghc 8.8.4 && \
	/ghcup set ghc 8.8.4 && \
	/ghcup install cabal 3.2.0.0 && \
	/ghcup set cabal 3.2.0.0

ENV PATH="/root/.ghcup/bin:${PATH}"
RUN echo "PATH=${PATH}" >>/etc/environment

COPY . .

# TODO: Cache this better somehow?
RUN cabal update && cabal build

ENTRYPOINT ["./pastebin-haskell"]
