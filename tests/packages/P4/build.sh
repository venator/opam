#!/bin/bash

echo "Building P4 with ${OPAM}"
COMP0="`${OPAM} config -r asmcomp P2 P3`"
LINK0="`${OPAM} config -r asmlink P2 P3`"

COMP=${COMP0// /,}
LINK=${LINK0// /,}

ocamlbuild -cflags ${COMP} -lflags ${LINK} p4.native

echo "TEST=${TEST}"