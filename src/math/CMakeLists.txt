project (BQC-Math)

set (${PROJECT_NAME}_headers "libmath.h" )
set (${PROJECT_NAME}_sources "libmath.cc")

add_library (${PROJECT_NAME} SHARED ${${PROJECT_NAME}_headers}
                                    ${${PROJECT_NAME}_sources})

install (TARGETS ${PROJECT_NAME} 
         RUNTIME DESTINATION bin
         LIBRARY DESTINATION lib
         ARCHIVE DESTINATION lib/static)

