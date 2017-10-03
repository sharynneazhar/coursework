CPP = g++ -std=c++11
INC = -I../cryphutil -I../fontutil -I../glslutil -I../mvcutil -I../imageutil
C_FLAGS = -fPIC -g -c -DGL_GLEXT_PROTOTYPES $(INC)

LINK = g++ -fPIC -g
LOCAL_UTIL_LIBRARIES = ../lib/libcryph.so ../lib/libfont.so ../lib/libglsl.so ../lib/libimage.so ../lib/libmvc.so
GL_LIB_LOC = -L/usr/lib/nvidia-375
GL_LIBRARIES = $(GL_LIB_LOC) -lglfw -lGLU -lGL
SYSTEM_LIBRARIES = -ljpeg

OBJS = room.o SceneElement.o Barbell.o Floor.o Tetrahedron.o Table.o PhongMaterial.o ModelView_Additions.o

all: room
	ls -l room

room: $(OBJS) ../lib/libcryph.so ../lib/libfont.so ../lib/libglsl.so ../lib/libimage.so ../lib/libmvc.so
	$(LINK) -o room $(OBJS) $(LOCAL_UTIL_LIBRARIES) $(GL_LIBRARIES) $(SYSTEM_LIBRARIES)

../lib/libcryph.so: ../cryphutil/AffPoint.h ../cryphutil/AffPoint.c++ ../cryphutil/AffVector.h ../cryphutil/AffVector.c++ ../cryphutil/Matrix4x4.h ../cryphutil/Matrix4x4.c++
	(cd ../cryphutil; make)

../lib/libfont.so: ../fontutil/CFont.h ../fontutil/CFont.c++ ../fontutil/CGLString.h ../fontutil/CGLString.c++
	(cd ../fontutil; make)

../lib/libglsl.so: ../glslutil/ShaderIF.h ../glslutil/ShaderIF.c++
	(cd ../glslutil; make)

../lib/libimage.so: ../imageutil/ImageReader.h ../imageutil/ImageReader.c++
	(cd ../imageutil; make)

../lib/libmvc.so: ../mvcutil/Controller.h ../mvcutil/Controller.c++ ../mvcutil/ModelView.h ../mvcutil/ModelView.c++ ../mvcutil/BasicShape.h ../mvcutil/BasicShape.c++
	(cd ../mvcutil; make)

room.o: room.c++
	$(CPP) $(C_FLAGS) room.c++

SceneElement.o: SceneElement.h SceneElement.c++
	$(CPP) $(C_FLAGS) SceneElement.c++

Barbell.o: Barbell.h Barbell.c++
	$(CPP) $(C_FLAGS) Barbell.c++

Floor.o: Floor.h Floor.c++
	$(CPP) $(C_FLAGS) Floor.c++

ModelView_Additions.o: ModelView_Additions.c++
	$(CPP) $(C_FLAGS) ModelView_Additions.c++

Tetrahedron.o: Tetrahedron.h Tetrahedron.c++
	$(CPP) $(C_FLAGS) Tetrahedron.c++

Table.o: Table.c++
	$(CPP) $(C_FLAGS) Table.c++

PhongMaterial.o: PhongMaterial.h PhongMaterial.c++
	$(CPP) $(C_FLAGS) PhongMaterial.c++
