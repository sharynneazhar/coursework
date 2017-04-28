CPP = g++ -std=c++11
INC = -I../contourlines_GL -I../cryphutil -I../glslutil -I../mvcutil
C_FLAGS = -fPIC -g -c -DGL_GLEXT_PROTOTYPES $(INC)

LINK = g++ -fPIC -g
LOCAL_UTIL_LIBRARIES = ../lib/libcryph.so ../lib/libglsl.so ../lib/libmvc.so ../contourlines_GL/contourlines_GL.so
GL_LIB_LOC = -L/usr/lib64/nvidia
OGL_LIBRARIES = $(GL_LIB_LOC) -lglfw -lGLU -lGL -lOpenCL

OBJS = main.o ContourGenerator.o OpenCLManager.o readSource.o

main: $(OBJS) $(LOCAL_UTIL_LIBRARIES)
	$(LINK) -o main $(OBJS) $(LOCAL_UTIL_LIBRARIES) $(OGL_LIBRARIES)

../contourlines_GL/contourlines_GL.so: ../contourlines_GL/ContourLineController.c++
	(cd ../contourlines_GL; make)

../lib/libcryph.so: ../cryphutil/AffPoint.h ../cryphutil/AffPoint.c++
	(cd ../cryphutil; make)

../lib/libglsl.so: ../glslutil/ShaderIF.h ../glslutil/ShaderIF.c++
	(cd ../glslutil; make)

../lib/libmvc.so: ../mvcutil/Controller.h ../mvcutil/Controller.c++ ../mvcutil/ModelView.h ../mvcutil/ModelView.c++
	(cd ../mvcutil; make)

main.o: ContourGenerator.h ../contourlines_GL/GL_LINES_Renderer.h main.c++
	$(CPP) $(C_FLAGS) main.c++

ContourGenerator.o: ContourGenerator.h ContourGenerator.c++
	$(CPP) $(C_FLAGS) ContourGenerator.c++

OpenCLManager.o: OpenCLManager.h OpenCLManager.c++
	g++ $(C_FLAGS) OpenCLManager.c++

readSource.o: readSource.c++
	$(CPP) $(C_FLAGS) readSource.c++

clean:
	rm -rf *.o main
