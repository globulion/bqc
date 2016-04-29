project (BQC-Ints)

set (${PROJECT_NAME}_headers "libints.h" )
set (${PROJECT_NAME}_sources "ints.cc")

add_library (${PROJECT_NAME} SHARED ${${PROJECT_NAME}_headers}
                                    ${${PROJECT_NAME}_sources})

install (TARGETS ${PROJECT_NAME} 
         RUNTIME DESTINATION bin
         LIBRARY DESTINATION lib
         ARCHIVE DESTINATION lib/static)

