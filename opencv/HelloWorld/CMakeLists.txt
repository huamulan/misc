PROJECT( helloworld_proj )
FIND_PACKAGE( OpenCV REQUIRED )
ADD_EXECUTABLE( helloworld helloworld.cpp)
TARGET_LINK_LIBRARIES( helloworld ${OpenCV_LIBS} )
