FROM debian:bullseye-slim

RUN apt-get clean && apt-get update
RUN apt-get install -y --no-install-recommends \
    software-properties-common \
    build-essential \
    python3.9 \
    python3-pip \
    haskell-platform \
    python3-venv \
    curl

RUN add-apt-repository ppa:swi-prolog/stable
RUN apt-get install -y --no-install-recommends \
        swi-prolog

RUN curl -sSL https://get.haskellstack.org/ | sh

RUN stack install ghc

COPY ./prolog /cpsc312-project/prolog
RUN cd /cpsc312-project/prolog/ && make build

COPY ./Makefile /cpsc312-project/Makefile
COPY ./test.sh /cpsc312-project/test.sh
RUN chmod +x /cpsc312-project/test.sh