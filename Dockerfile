FROM ocaml/opam

USER 0

WORKDIR /var/rinha/
COPY rinha.opam .
RUN opam install --deps-only .
RUN apt update && apt install -y clang-15 musl-dev musl-tools

COPY . .
RUN eval $(opam env) && dune build --release
RUN clang-15 -c -o runtime.o runtime.c && ar rc /usr/lib/librinha.a runtime.o
RUN cp ./rinharun ./_build/install/default/bin/rinha /usr/bin

ENV LLC llc-15
ENV LD musl-gcc
ENV LD_FLAGS -static -L/usr/lib/
ENTRYPOINT [ "rinharun" ]