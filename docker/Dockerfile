FROM ocaml/opam2

USER root

RUN apt-get update && apt-get install -y \
        gdb \
        clang-format \
        m4 \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

USER opam

RUN opam install menhir \
    && opam install ppx_deriving

WORKDIR /work
