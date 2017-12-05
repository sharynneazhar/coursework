CPP = g++ -std=c++11
INC = -I../cryphutil -I../fontutil -I../glslutil -I../imageutil -I../mvcutil
C_FLAGS = -fPIC -g -c -DGL_GLEXT_PROTOTYPES $(INC)

LINK = g++ -fPIC -g
LOCAL_UTIL_LIBRARIES = ../lib/libcryph.so ../lib/libfont.so ../lib/libglsl.so ../lib/libimage.so ../lib/libmvc.so
GL_LIB_LOC = -L/usr/lib/nvidia-375
GL_LIBRARIES = $(GL_LIB_LOC) -lglfw -lGLU -lGL

OBJS = project4.o ModelView_Additions.o PhongMaterial.o SceneElement.o Block.o CrateTop.o Parachute.o Crate.o LightPost.o Building.o Door.o Tree.o TreeTop.o

project4: $(OBJS) $(LOCAL_UTIL_LIBRARIES)
	$(LINK) -o project4 $(OBJS) $(LOCAL_UTIL_LIBRARIES) $(GL_LIBRARIES)

../lib/libcryph.so: ../cryphutil/AffVector.h ../cryphutil/AffVector.c++ ../cryphutil/AffPoint.h ../cryphutil/AffPoint.c++
	(cd ../cryphutil; make)

../lib/libfont.so: ../fontutil/CFont.h ../fontutil/CFont.c++ ../fontutil/CGLString.h ../fontutil/CGLString.c++
	(cd ../fontutil; make)

../lib/libglsl.so: ../glslutil/ShaderIF.h ../glslutil/ShaderIF.c++
	(cd ../glslutil; make)

../lib/libimage.so: ../imageutil/ImageReader.h ../imageutil/ImageReader.c++
	(cd ../imageutil; make)

../lib/libmvc.so: ../mvcutil/Controller.h ../mvcutil/Controller.c++ ../mvcutil/ModelView.h ../mvcutil/ModelView.c++
	(cd ../mvcutil; make)

project4.o: project4.c++
	$(CPP) $(C_FLAGS) project4.c++
ModelView_Additions.o: ModelView_Additions.c++
	$(CPP) $(C_FLAGS) ModelView_Additions.c++
PhongMaterial.o: PhongMaterial.c++
	$(CPP) $(C_FLAGS) PhongMaterial.c++
SceneElement.o: SceneElement.h SceneElement.c++
	$(CPP) $(C_FLAGS) SceneElement.c++
Block.o: Block.h Block.c++
	$(CPP) $(C_FLAGS) Block.c++
CrateTop.o: CrateTop.h CrateTop.c++
	$(CPP) $(C_FLAGS) CrateTop.c++
Parachute.o: Parachute.h Parachute.c++
	$(CPP) $(C_FLAGS) Parachute.c++
Crate.o: Crate.h Crate.c++
	$(CPP) $(C_FLAGS) Crate.c++
LightPost.o: LightPost.h LightPost.c++
	$(CPP) $(C_FLAGS) LightPost.c++
Building.o: Building.h Building.c++
	$(CPP) $(C_FLAGS) Building.c++
Door.o: Door.h Door.c++
	$(CPP) $(C_FLAGS) Door.c++
Tree.o: Tree.h Tree.c++
	$(CPP) $(C_FLAGS) Tree.c++
TreeTop.o: TreeTop.h TreeTop.c++
	$(CPP) $(C_FLAGS) TreeTop.c++

clean:
	rm project4 ../**/*.o ../**/*.so
