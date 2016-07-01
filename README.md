# mheap

A modern Fortran implementation of the heap data structure

## Synopsis

The mheap module provides a fast and easy to use heap implementation in Fortran 2003.

From Wikipedia: 

> In computer science, a heap is a specialized tree-based data structure that
> satisfies the heap property: If A is a parent node of B then the key (the
> value) of node A is ordered with respect to the key of node B with the same
> ordering applying across the heap. A heap can be classified further as either a
> "max heap" or a "min heap". In a max heap, the keys of parent nodes are always
> greater than or equal to those of the children and the highest key is in the
> root node. In a min heap, the keys of parent nodes are less than or equal to
> those of the children and the lowest key is in the root node. Heaps are crucial
> in several efficient graph algorithms such as Dijkstra's algorithm, and in the
> sorting algorithm heapsort."""

## Code Example

Insert several points in space and order them by distance to the origin:
```fortran
PROGRAM EXAMPLE_MHEAP

   USE MHEAP

   DOUBLE PRECISION :: POINT(3)
   TYPE(THEAP) :: HEAP
   INTEGER :: K

   ! Init a heap with at max 10 elements containing double precision arrays of lenght 3
   ! and the comparison function EUCLIDEAN_CMP, that compares the points' euclidean norm 
   CALL HEAP%INIT( 10, 3, EUCLIDEAN_CMP )

   ! Insert some points
   CALL HEAP%INSERT( [ 1.0D0, 2.0D0, 1.0D0] )
   CALL HEAP%INSERT( [-1.0D0, 8.0D0,-3.0D0] )
   CALL HEAP%INSERT( [ 5.0D0, 1.0D0, 5.0D0] )
   CALL HEAP%INSERT( [ 2.0D0,-1.0D0,-2.0D0] )

   ! Traversal in order of distance to the origin
   DO K = 1, HEAP%SIZE()
      CALL HEAP%POP( POINT )
      WRITE(*,*) POINT
   ENDDO

CONTAINS 

   LOGICAL FUNCTION EUCLIDEAN_CMP( POINT1, POINT2 )
      DOUBLE  PRECISION, INTENT(IN) :: POINT1(:), POINT2(:)
      EUCLIDEAN_CMP = SUM(POINT1**2.0D0) < SUM(POINT2**2.0D0)
   END FUNCTION EUCLIDEAN_CMP

END PROGRAM EXAMPLE_MHEAP
```

## Motivation

The heap is arguably the most useful data structure, and it is very difficult to find an easy to use implementation in modern Fortran.


## Usage and installation

mheap comes in a single source file (mheap.f90), so the easiest way to use it is to include it directly into your 
projects. If, however, you prefer to maintain only one copy, a static or dynamic library can be
built using cmake.

To build and install mheap in the user's $HOME directory:

```bash
$ mkdir bld && cd bld && cmake -DCMAKE_INSTALL_PREFIX=$HOME ../ && make && make install && cd ..
``` 

This will create two files that can be shared across several projects. 

* $HOME/lib/libmheap.so
* $HOME/include/mheap.mod


## API Reference

To learn how to use the API, please see the included test_mheap.f90 example program
and the comments in the mheap source file. 

## Simplified BSD License

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright notice,
this list of conditions and the following disclaimer in the documentation
and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

Copyright (c) 2014, Daniel Pena 
All rights reserved.

