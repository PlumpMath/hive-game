#! /bin/bash

cython -3 openglTest.pyx
gcc -shared -pthread -fPIC -fwrapv -O2 -Wall -fno-strict-aliasing -I/usr/include/python3.4m -o openglTest.so openglTest.c -lX11 -lGL
