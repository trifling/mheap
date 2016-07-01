! 2014, Daniel Pena
! Simplified BSD License 

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
