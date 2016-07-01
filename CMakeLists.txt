
PROJECT( MHEAP Fortran )

SET(VERSION 1.0.0)

CMAKE_MINIMUM_REQUIRED(VERSION 2.8.5)

# set default installation to ./install
set( CMAKE_INSTALL_PREFIX "${CMAKE_SOURCE_DIR}/install/" CACHE STRING "Choose the installation directory; by default it installs in the ./install directory." FORCE )

# Unique directory for Fortran compiled .mod files -> modules folder
SET(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/modules)

# set default build to RELEASE
if( NOT CMAKE_BUILD_TYPE )
   set (CMAKE_BUILD_TYPE RELEASE CACHE STRING "Choose the type of build, options are: NONE DEBUG RELEASE." FORCE)
endif( NOT CMAKE_BUILD_TYPE )

# do not build static version by default  
if( NOT DEFINED MHEAP_BUILD_STATIC )
   set (MHEAP_BUILD_STATIC OFF CACHE STRING "Build a static version of the MHEAP library" FORCE)
endif( NOT DEFINED MHEAP_BUILD_STATIC )

# MHEAP sources
set( MHEAP_SRCS mheap.f90 )

if( MHEAP_BUILD_STATIC )
   add_library( mheap STATIC ${MHEAP_SRCS} ) 
   install( TARGETS mheap ARCHIVE DESTINATION lib ) 
else( MHEAP_BUILD_STATIC )
   add_library( mheap SHARED ${MHEAP_SRCS} ) 
   install( TARGETS mheap LIBRARY DESTINATION lib ) 
endif( MHEAP_BUILD_STATIC )

# copy .mod file when installing
INSTALL( FILES "${CMAKE_Fortran_MODULE_DIRECTORY}/mheap.mod" DESTINATION include)

# Build the MHEAP example program
ADD_EXECUTABLE( test_mheap test_mheap.f90 )

TARGET_LINK_LIBRARIES( test_mheap mheap )

