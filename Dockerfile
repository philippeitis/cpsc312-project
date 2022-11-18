FROM debian:latest

RUN apt-get clean && apt-get update
RUN apt-get install -y --no-install-recommends \
    software-properties-common \
    build-essential \
    python3.9 \
    python3-pip \
    python3-venv \
    git-all \
    cmake \
    ninja-build \
    zlib1g \
    zlib1g-dev \
    pkg-config \
    ncurses-dev libreadline-dev libedit-dev \
    libgoogle-perftools-dev \
    libunwind-dev \
    libgmp-dev \
    libssl-dev \
    unixodbc-dev \
    libarchive-dev \
    libossp-uuid-dev \
    libxext-dev libice-dev libjpeg-dev libxinerama-dev libxft-dev \
    libxpm-dev libxt-dev \
    libdb-dev \
    libpcre3-dev \
    libyaml-dev \
    default-jdk junit4

RUN git clone https://github.com/SWI-Prolog/swipl.git && cd swipl && git submodule update --init

COPY ./scripts/ ./scripts/
RUN chmod +x ./scripts/*.sh
RUN /scripts/git-fetch-prolog.sh
RUN /scripts/build-prolog.sh

ENV PATH "$PATH:/root/bin/"

RUN apt-get install -y --no-install-recommends curl

RUN curl https://sh.rustup.rs -sSf | sh -s -- --default-toolchain nightly -y

ENV PATH "$PATH:/root/.cargo/bin/"
RUN apt-get install -y --no-install-recommends clang

COPY ./Makefile /cpsc312-project/Makefile

COPY ./prolog /cpsc312-project/prolog
RUN cd /cpsc312-project/prolog/ && make build


ENV IN_DOCKER=1
WORKDIR "/cpsc312-project/prolog"
CMD ["swipl", "./main.pl", "launch", "5000"]