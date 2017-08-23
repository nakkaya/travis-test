FROM    ubuntu:16.04
MAINTAINER Nurullah Akkaya <nurullah@nakkaya.com>

RUN apt-get update && \
    apt-get upgrade -y

RUN apt-get install apt-utils software-properties-common -y
RUN add-apt-repository ppa:openjdk-r/ppa
RUN apt-get update

# Setup Tangle Requirements

RUN apt-get install wget make git emacs24-nox openjdk-7-jre-headless -y

RUN wget https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein
RUN chmod +x lein
RUN mv lein /usr/bin/
RUN lein

ENV LEIN_ROOT true

# Setup Test Requirements

RUN apt-get install gcc-5 g++-5 llvm-3.6 clang-3.6 cppcheck -y
RUN update-alternatives --install /usr/bin/g++ g++ /usr/bin/g++-5 50

# Setup Platform Build Requirements
RUN apt-get install zip -y
