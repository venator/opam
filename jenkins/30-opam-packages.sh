#!/bin/sh -ex
PREFIX=$1
OPAM=$HOME/opam-bin/$PREFIX/bin/opam
ROOT=`echo /x/${JOB_NAME} | sed -e "s,=,_,g" -e "s/,/-/g"`
rm -rf ${ROOT}

case "${compiler}" in
system4)
  export PATH=/x/ocaml-4.00.1/bin:$PATH
  ;;
esac

export OPAMROOT=$ROOT
export OPAMYES=1

$OPAM init $2
$OPAM config -list-vars

case "${compiler}" in
system)
  ;;
system4)
  ;;
*)
  $OPAM $ROOT switch ${compiler}
  ;;
esac

$OPAM --verbose install ${packages}
