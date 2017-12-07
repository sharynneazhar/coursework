// ProjController.h

#ifndef PROJCONTROLLER_H
#define PROJCONTROLLER_H

#include "GLFWController.h"
#include "SceneElement.h"

class ProjController : public GLFWController
{
public:
  ProjController (const std::string& windowTitle, int rcFlags = 0);
  ~ProjController();
  virtual void handleDisplay();
  bool drawingOpaque() const;

private:
  bool sceneHasTranslucentObjects;
	virtual void drawAllOpaqueObjects();
	virtual void drawAllTranslucentObjects();
};

#endif
