project (BQC-Molecule)

set (${PROJECT_NAME}_headers "libmol.h" )
set (${PROJECT_NAME}_sources "mol.cc" "atom.cc")

add_library (${PROJECT_NAME} SHARED ${${PROJECT_NAME}_headers}
                                    ${${PROJECT_NAME}_sources})

install (TARGETS ${PROJECT_NAME} 
         RUNTIME DESTINATION bin
         LIBRARY DESTINATION lib
         ARCHIVE DESTINATION lib/static)

