! Copyright (c) 2014, Daniel Pena 
! All rights reserved.

! Redistribution and use in source and binary forms, with or without
! modification, are permitted provided that the following conditions are met:

! 1. Redistributions of source code must retain the above copyright notice, this
! list of conditions and the following disclaimer.
! 2. Redistributions in binary form must reproduce the above copyright notice,
! this list of conditions and the following disclaimer in the documentation
! and/or other materials provided with the distribution.

! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
! ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
! WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
! DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
! ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
! (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
! LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
! ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
! (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
! SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

PROGRAM TEST_MHEAP

   USE MHEAP

   DOUBLE PRECISION :: NODE(2)
   TYPE(THEAP) :: H
   INTEGER     :: K

   ! Init a heap with comparison function GREATER1, that 
   ! compares the nodes' first component to order the heap
   CALL H%INIT( 10, 2, GREATER1 )

   ! insert some data
   CALL H%INSERT( [ 1.0D0, 2.0D0] )
   CALL H%INSERT( [-1.0D0, 8.0D0] )
   CALL H%INSERT( [ 5.0D0, 1.0D0] )
   CALL H%INSERT( [ 2.0D0,-1.0D0] )
   CALL H%INSERT( [-6.0D0, 5.0D0] )
   CALL H%INSERT( [ 3.0D0, 2.0D0] )

   ! Data is kept unordered (except for the root node)
   WRITE(*,*)
   WRITE(*,*) 'Unordered traversal, the first element is always the root node'
   DO K = 1, H%N
      CALL H%PEEK( K, NODE ) 
      WRITE(*,*) NODE
   ENDDO

   ! When the root node is popped, a new root node is set.
   ! To traverse it in order just pop all the root elements.
   WRITE(*,*)
   WRITE(*,*) 'Ordered traversal'
   DO K = 1, H%N
      CALL H%POP(NODE)
      WRITE(*,*) NODE
   ENDDO
   WRITE(*,*) 'Now heap is empty:', H%N

   ! Data is not lost from the tree when using pop,
   ! we can reheap the whole tree and start over
   WRITE(*,*) 'Tree data is kept after popping, so as long'
   write(*,*) 'as no new insertions are made, the same data'
   write(*,*) 'can be reheaped using the same or another function.'
   WRITE(*,*)
   WRITE(*,*) 'Reheap using same function'
   CALL H%REHEAP()
   DO K = 1, H%N
      CALL H%POP(NODE)
      WRITE(*,*) NODE
   ENDDO

   WRITE(*,*)
   WRITE(*,*) 'Reheap using other function'
   CALL H%REHEAP( GREATER2 )
   DO K = 1, H%N
      CALL H%POP(NODE)
      WRITE(*,*) NODE
   ENDDO

   WRITE(*,*)
   WRITE(*,*) 'Pop half the elements'
   CALL H%REHEAP( GREATER2 )
   DO K = 1, H%N / 2
      CALL H%POP(NODE)
      WRITE(*,*) NODE
   ENDDO
   WRITE(*,*) 'Insert 2 new elements'
   CALL H%INSERT( [ 2.0D0, 10.0D0] )
   CALL H%INSERT( [ 3.0D0,-10.0D0] )
   WRITE(*,*) 'Pop all'
   DO K = 1, H%N 
      CALL H%POP(NODE)
      WRITE(*,*) NODE
   ENDDO

CONTAINS

   LOGICAL FUNCTION GREATER1( NODE1, NODE2 )
      DOUBLE  PRECISION, INTENT(IN) :: NODE1(:), NODE2(:)
      GREATER1 = NODE1(1) < NODE2(1)
   END FUNCTION GREATER1

   LOGICAL FUNCTION GREATER2( NODE1, NODE2 )
      DOUBLE PRECISION, INTENT(IN) :: NODE1(:), NODE2(:)
      GREATER2 = NODE1(2) < NODE2(2)
   END FUNCTION GREATER2

END PROGRAM TEST_MHEAP
