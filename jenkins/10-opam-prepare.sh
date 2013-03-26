#!/bin/sh
BASE="ocaml ocaml-compiler-libs"

add () { BASE="${BASE} $1"; }

# riakc
add protobuf-compiler
# frei0r
add frei0r-plugins-dev
# voaacenc
add camlidl
# svm
add libsvm-dev
# buddy
add libbdd-dev
# libvirt
add libvirt-dev
# ffmpeg
add libswscale-dev
# dssi
add dssi-dev
# spf
add libspf2-dev
# oqamldebug
add libqt4-dev
# milter
add libmilter-dev
# lzo
add liblzo2-dev
# magic
add libmagic-dev
# mpi
add mpi-default-dev
# snappy
add libsnappy-dev
# alt-ergo
add autoconf
# yajl
add cmake
# cairo
add libcairo2-dev
# lapack
add libatlas-dev
add liblapack-dev
# ocaml-sdl
add libsdl-dev
# dbm
add libgdbm-dev
# ocamlfuse
add libfuse-dev
# ffmpeg
add libavutil-dev
# ladspa
add ladspa-sdk
# gstreamer
add libgstreamer0.10-dev
# opus
add libavutil-dev
# frei0r
add frei0r-plugins-dev
# liquidsoap
add libao-dev
add portaudio19-dev 
add libsamplerate-dev 
add libgstreamer0.10-dev 
add libgstreamer-plugins-base0.10-dev 
add libgstreamer0.10-dev 
add libgstreamer-plugins-base0.10-dev 
add libgstreamer1.0-dev
add libmp3lame-dev 
add libogg-dev 
add libvorbis-dev
add libspeex-dev
add libtheora-dev 
add libschroedinger-dev 
add libvo-aacenc-dev 
add libfaad-dev
add libflac-dev
add libsoundtouch-dev 
add libgavl-dev 
add liblo-dev
# lablgtk2
add libgtk2.0-dev
add libgtksourceview2.0-dev
add libglade2-dev
add libgnomecanvas2-dev
# oqamldebug
add qt4-qmake
# lwt-zmq
add libzmq-dev
# postgresql-ocaml
add libpq-dev
# camlbz2
add libbz2-dev
# imagemagick
add libgraphicsmagick1-dev 
add libmagickcore-dev
# sqlite3
add libsqlite3-dev
# ocaml-glpk
add libglpk-dev
# lablgl
add mesa-common-dev 
add libglu1-mesa-dev
add freeglut3-dev
# ocamlgsl
add gawk
# mlgmp
add libmpfr-dev
# ocaml-lua
add liblua5.1-0-dev
# ocurl
add libcurl4-gnutls-dev
# gpr
add libgsl0-dev
# planets
add tcl8.5-dev tk8.5-dev
# ocaml-mysql
add libmysqlclient-dev
# odepack
# lbfgs
add gfortran
# fftw-ocaml
add libfftw3-dev
# ocaml-taglib
add libtag1-dev
# ocaml-mad
add libmad0-dev
sudo apt-get -y install ${BASE}

