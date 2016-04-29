# add the binary tree directory to the search path 
# for include files
#include_directories( ${CMAKE_CURRENT_BINARY_DIR} )
#link_directories( ${CMAKE_CURRENT_BINARY_DIR} ) 

project (BQC-Basis)

set (${PROJECT_NAME}_headers "libbasis.h")
set (${PROJECT_NAME}_sources "basis.cc")

add_library (${PROJECT_NAME} SHARED ${${PROJECT_NAME}_headers}
                                    ${${PROJECT_NAME}_sources})

install (TARGETS ${PROJECT_NAME} 
         RUNTIME DESTINATION bin
         LIBRARY DESTINATION lib
         ARCHIVE DESTINATION lib/static)

