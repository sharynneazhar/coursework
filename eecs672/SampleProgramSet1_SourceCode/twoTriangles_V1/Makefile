CPP = g++ -std=c++11
INC = -I../glslutil -I../mvcutil -I.
C_FLAGS = -fPIC -g -c -DGL_GLEXT_PROTOTYPES $(INC)

LINK = g++ -fPIC -g
LOCAL_UTIL_LIBRARIES = ../lib/libglsl.so
GL_LIB_LOC = -L/usr/lib/nvidia-375
GL_LIBRARIES = $(GL_LIB_LOC) -lglfw -lGLU -lGL

OBJS = twoTriangles.o ModelView.o Controller.o GLFWController.o

twoTriangles: $(OBJS) $(LOCAL_UTIL_LIBRARIES)
	$(LINK) -o twoTriangles $(OBJS) $(LOCAL_UTIL_LIBRARIES) $(GL_LIBRARIES)

../lib/libglsl.so: ../glslutil/ShaderIF.h ../glslutil/ShaderIF.c++
	(cd ../glslutil; make)

twoTriangles.o: twoTriangles.c++
	$(CPP) $(C_FLAGS) twoTriangles.c++
ModelView.o: ModelView.h ModelView.c++
	$(CPP) $(C_FLAGS) ModelView.c++
Controller.o: ../mvcutil/Controller.h ../mvcutil/Controller.c++
	$(CPP) $(C_FLAGS) ../mvcutil/Controller.c++
GLFWController.o: ../mvcutil/Controller.h ../mvcutil/GLFWController.h ../mvcutil/GLFWController.c++
	$(CPP) $(C_FLAGS) ../mvcutil/GLFWController.c++
