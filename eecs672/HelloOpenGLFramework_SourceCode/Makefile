CPP = g++ -std=c++11
C_FLAGS = -fPIC -g -c -DGL_GLEXT_PROTOTYPES

LINK = g++ -fPIC -g
GL_LIB_LOC = -L/usr/lib/nvidia-375
GL_LIBRARIES = $(GL_LIB_LOC) -lglfw -lGLU -lGL

OBJS = hello.o ModelView.o GLFWController.o ShaderIF.o

hello: $(OBJS)
	$(LINK) -o hello $(OBJS) $(GL_LIBRARIES)

hello.o: hello.c++
	$(CPP) $(C_FLAGS) hello.c++
ModelView.o: ModelView.c++
	$(CPP) $(C_FLAGS) ModelView.c++
GLFWController.o: GLFWController.c++
	$(CPP) $(C_FLAGS) GLFWController.c++
ShaderIF.o: ShaderIF.h ShaderIF.c++
	$(CPP) $(C_FLAGS) ShaderIF.c++
