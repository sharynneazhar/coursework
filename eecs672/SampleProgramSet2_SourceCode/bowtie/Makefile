CPP = g++ -std=c++11
INC = -I../cryphutil -I../fontutil -I../glslutil -I../mvcutil -I.
C_FLAGS = -fPIC -g -c -DGL_GLEXT_PROTOTYPES $(INC)

LINK = g++ -fPIC -g
LOCAL_UTIL_LIBRARIES = ../lib/libcryph.so ../lib/libfont.so ../lib/libglsl.so
GL_LIB_LOC = -L/usr/lib/nvidia-375
GL_LIBRARIES = $(GL_LIB_LOC) -lglfw -lGLU -lGL

OBJS = bowtie.o ModelView.o Controller.o GLFWController.o

bowtie: $(OBJS) $(LOCAL_UTIL_LIBRARIES)
	$(LINK) -g -o bowtie $(OBJS) $(LOCAL_UTIL_LIBRARIES) $(GL_LIBRARIES)

../lib/libcryph.so: ../cryphutil/AffPoint.h ../cryphutil/AffPoint.c++ ../cryphutil/AffVector.h ../cryphutil/AffVector.c++
	(cd ../cryphutil; make)

../lib/libfont.so: ../fontutil/CFont.h ../fontutil/CFont.c++ ../fontutil/CGLString.h ../fontutil/CGLString.c++
	(cd ../fontutil; make)

../lib/libglsl.so: ../glslutil/ShaderIF.h ../glslutil/ShaderIF.c++
	(cd ../glslutil; make)

bowtie.o: bowtie.c++
	$(CPP) $(C_FLAGS) bowtie.c++
ModelView.o: ModelView.h ModelView.c++
	$(CPP) $(C_FLAGS) ModelView.c++
Controller.o: ../mvcutil/Controller.h ../mvcutil/Controller.c++
	$(CPP) $(C_FLAGS) ../mvcutil/Controller.c++
GLFWController.o: ../mvcutil/GLFWController.h ../mvcutil/GLFWController.c++
	$(CPP) $(C_FLAGS) ../mvcutil/GLFWController.c++
