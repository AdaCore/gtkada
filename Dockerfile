FROM ubuntu:16.04

RUN apt-get -qq update
RUN apt-get install -y cmake g++ gnat gprbuild pkg-config libgtk-3-dev make

RUN mkdir -p /src/
WORKDIR /src/

COPY . /src/

RUN ./configure --disable-static-pic --prefix="/src" --with-GL=no --disable-static && make all install
