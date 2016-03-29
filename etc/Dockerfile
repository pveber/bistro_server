FROM debian:jessie

RUN apt-get update && apt-get install -y \
    aspcud bzip2 curl git  make m4 unzip gcc \
    pkg-config libgdbm-dev libncurses5-dev  libpcre3-dev libssl-dev \
    sudo wget zlib1g-dev

RUN useradd -m bistro
RUN mkdir -m 0755 /nix && chown bistro /nix

USER bistro
ENV USER bistro
WORKDIR /home/bistro

RUN curl https://nixos.org/nix/install | sh
RUN echo ". $HOME/.nix-profile/etc/profile.d/nix.sh" >> ~/.bashrc

RUN git clone https://github.com/pveber/bionix
RUN cd ~/.nix-defexpr && ln -s $HOME/bionix .

RUN mkdir -p usr/bin
RUN wget https://raw.github.com/ocaml/opam/master/shell/opam_installer.sh -O - | sh -s ~/usr/bin 4.02.3
ENV PATH /home/bistro/usr/bin:$PATH
RUN echo "export LD_LIBRARY_PATH=$HOME/.opam/4.02.3/lib/stublibs" >> ~/.bashrc

RUN opam pin add sexplib 113.00.00
RUN opam pin add bistro --dev-repo -n

RUN opam install eliom bistro ppx_deriving

RUN git clone https://github.com/pveber/bistro_server.git
RUN \
  . $HOME/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true && \
  cd bistro_server && \
  make

