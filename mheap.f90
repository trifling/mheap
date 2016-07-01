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

MODULE MHEAP

IMPLICIT NONE

PRIVATE

PUBLIC :: HEAPFUN, THEAP

ABSTRACT INTERFACE
FUNCTION HEAPFUN( NODE1, NODE2 ) RESULT(RES)
   DOUBLE PRECISION, INTENT(IN) :: NODE1(:)
   DOUBLE PRECISION, INTENT(IN) :: NODE2(:)
   LOGICAL :: RES
END FUNCTION HEAPFUN
END INTERFACE

TYPE :: THEAP
   INTEGER                             :: NMAX      ! MAX SIZE
   INTEGER                             :: N         ! CURRENT HEAP SIZE
   INTEGER                             :: M         ! CURRENT TREE SIZE
   INTEGER                             :: NLEN      ! NODE SIZE IN DOUBLE PRECISION UNITS
   DOUBLE PRECISION, ALLOCATABLE       :: DATA(:,:) ! NODE DATA
   INTEGER, ALLOCATABLE                :: INDX(:)   ! NODES INDEX
   PROCEDURE(HEAPFUN), NOPASS, POINTER :: FUN       ! HEAP FUNCTION TO FIND ROOT NODE
   CONTAINS
   PROCEDURE :: INIT    => HEAP_INIT
   PROCEDURE :: INSERT  => HEAP_INSERT
   PROCEDURE :: PEEK    => HEAP_PEEK
   PROCEDURE :: POP     => HEAP_POP
   PROCEDURE :: REHEAP  => HEAP_REHEAP
   PROCEDURE :: SIZE    => HEAP_SIZE
   FINAL     :: HEAP_RELEASE
END TYPE THEAP

CONTAINS

   INTEGER FUNCTION HEAP_SIZE( HEAP )
      ! Returns the heap current size
      CLASS(THEAP) :: HEAP
      HEAP_SIZE = HEAP%N
   END FUNCTION HEAP_SIZE

   SUBROUTINE HEAP_INIT(HEAP,NMAX,NLEN,HPFUN)                                       
      ! Initializes the heap 
      ! NMAX  -  max size of the heap
      ! NLEN  -  size of each node 
      ! HPFUN -  the heap function (provides comparison between two nodes' data)
      CLASS(THEAP)        :: HEAP
      INTEGER, INTENT(IN) :: NMAX, NLEN
      PROCEDURE(HEAPFUN)  :: HPFUN
      INTEGER :: I 
      HEAP%NMAX = NMAX
      HEAP%N    = 0
      HEAP%M    = 0
      HEAP%NLEN = NLEN
      HEAP%FUN  => HPFUN
      ALLOCATE( HEAP%INDX(NMAX)      )
      ALLOCATE( HEAP%DATA(NLEN,NMAX) )
      DO I = 1, NMAX
         HEAP%INDX(I)=I
      ENDDO
   END SUBROUTINE HEAP_INIT
   
   SUBROUTINE HEAP_RELEASE(HEAP)                                       
      ! Releases all the allocated memory and resets the heap
      TYPE(THEAP) :: HEAP
      DEALLOCATE(HEAP%INDX)
      DEALLOCATE(HEAP%DATA)
      HEAP%N    = 0
      HEAP%M    = 0
      HEAP%NMAX = 0
      HEAP%FUN  => NULL()
   END SUBROUTINE HEAP_RELEASE                          

   SUBROUTINE HEAP_INSERT(HEAP,NODE)         
      ! Insert a node into a heap. The resulting tree is re-heaped.
      !  input
      !        heap - the heap 
      !        node - a double precision array, nlen long, which
      !               contains the node's information to be inserted.
      CLASS(THEAP) :: HEAP
      DOUBLE PRECISION, INTENT(IN) :: NODE(HEAP%NLEN)

      INTEGER :: I, K1, K2, IL, IR

      IF( HEAP%N .EQ. HEAP%NMAX ) RETURN
      
      ! Add one element and copy node data to new element
      HEAP%N = HEAP%N + 1
      HEAP%M = HEAP%M + 1
      HEAP%DATA(:,HEAP%INDX(HEAP%N)) = NODE(:)

      ! Re-index the heap from the bottom up
      K2 = HEAP%N
      DO WHILE( K2 /= 1 )
         K1 = K2 / 2
         IR = HEAP%INDX(K2) 
         IL = HEAP%INDX(K1) 
         IF( HEAP%FUN( HEAP%DATA(:,IL), HEAP%DATA(:,IR) ) ) RETURN
         CALL SWAPINT( HEAP%INDX(K2), HEAP%INDX(K1) )
         K2 = K2 / 2
      ENDDO
   END SUBROUTINE HEAP_INSERT                  

   SUBROUTINE HEAP_POP( HEAP, NODE )                  
      ! Retrieve the root element off the heap. The resulting tree is re-heaped.
      ! No data is deleted, thus the original 
      !   input
      !        heap - the heap 
      !   output 
      !        node - the deleted node 
      CLASS(THEAP) :: HEAP
      DOUBLE PRECISION, OPTIONAL :: NODE( HEAP%NLEN )
      INTEGER :: I

      IF( HEAP%N .EQ. 0 ) RETURN

      IF( PRESENT(NODE) ) THEN
         NODE(:) = HEAP%DATA(:,HEAP%INDX(1))
      ENDIF

      CALL SWAPINT( HEAP%INDX(1), HEAP%INDX(HEAP%N) )
      
      HEAP%N = HEAP%N - 1

      CALL HEAP_GROW( HEAP, 1 )

   END SUBROUTINE HEAP_POP

   SUBROUTINE HEAP_PEEK( HEAP, K, NODE )                      
      ! Access the k-th node of the heap
      CLASS(THEAP) :: HEAP
      INTEGER, INTENT(IN) :: K
      DOUBLE PRECISION, INTENT(OUT) :: NODE(HEAP%NLEN)
      IF (K .LT. 1 .OR. K .GT. HEAP%N .OR. HEAP%N .GT. HEAP%NMAX) RETURN
      NODE(:) = HEAP%DATA(:,HEAP%INDX(K))
   END SUBROUTINE HEAP_PEEK

   SUBROUTINE HEAP_GROW(HEAP,KTEMP)                
      ! Forms a heap out of a tree. Used privately by HEAP_REHEAP.
      ! The root node of the tree is stored in the location INDX(KTEMP).
      ! The first child node is in location INDX(2*KTEMP)...
      ! The next child node is in location INDX(2*KTEMP+1).
      ! This subroutines assumes each branch of the tree is itself a heap.
      INTEGER :: I, K, ITEMP, ITP1, IL, IR, KT
      TYPE(THEAP) :: HEAP
      INTEGER :: KTEMP

      IF( HEAP%N .GT. HEAP%NMAX ) RETURN
 
      K = KTEMP
      DO WHILE( 2*K .LE. HEAP%N )

         I = 2*K
 
         ! If there is more than one child node, find which is the smallest.
         IF( 2*K .NE. HEAP%N ) THEN 
            IL = HEAP%INDX(2*K+1) 
            IR = HEAP%INDX(2*K  )   
            IF( HEAP%FUN(HEAP%DATA(:,IL),HEAP%DATA(:,IR)) ) THEN
               I = I + 1
            ENDIF
         ENDIF

         ! If a child is larger than its parent, interchange them... This destroys 
         ! the heap property, so the remaining elements must be re-heaped. 
         IL    = HEAP%INDX(K) 
         IR    = HEAP%INDX(I) 
         IF( HEAP%FUN(HEAP%DATA(:,IL),HEAP%DATA(:,IR)) ) RETURN
         
         CALL SWAPINT( HEAP%INDX(I), HEAP%INDX(K) )
         
         K = I
      
      ENDDO

   END SUBROUTINE HEAP_GROW

   SUBROUTINE HEAP_REHEAP(HEAP,HPFUN)                   
      ! Builds the heap from the element data using the provided heap function.
      ! At exit, the root node satisfies the heap condition:
      !   HPFUN( ROOT_NODE, NODE ) = .true. for any other NODE 
      ! 
      CLASS(THEAP)                 :: HEAP
      PROCEDURE(HEAPFUN), OPTIONAL :: HPFUN
      INTEGER                      :: K

      HEAP%N   = HEAP%M
      IF( PRESENT( HPFUN ) ) THEN
         HEAP%FUN => HPFUN 
      ENDIF

      IF(HEAP%NMAX .LT. HEAP%N) RETURN

      DO K = HEAP%N / 2, 1, -1
         CALL HEAP_GROW(HEAP,K) 
      ENDDO
   END SUBROUTINE HEAP_REHEAP

   SUBROUTINE SWAPINT( I, K )
      INTEGER :: I, K, T
      T = I
      I = K
      K = T
   END SUBROUTINE SWAPINT

END MODULE MHEAP
