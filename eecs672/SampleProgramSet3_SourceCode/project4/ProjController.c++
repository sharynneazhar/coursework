// ProjController.c++

#include "ProjController.h"


ProjController::ProjController(const std::string& windowTitle, int rcFlags) :
  GLFWController(windowTitle, rcFlags)
{

}

ProjController::~ProjController() {

}

void ProjController::handleDisplay() {
  if (theWindow == nullptr)
    return;

  int width, height;
  glfwMakeContextCurrent(theWindow);
  glfwGetFramebufferSize(theWindow, &width, &height);
  glViewport(0, 0, width, height);

  // Clear the frame buffer
  glClear(glClearFlags);

  // draw the collection of models
  glDepthMask(GL_TRUE);
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

  // draw all opaque objects
  glDisable(GL_BLEND);
  drawAllOpaqueObjects();

  // draw transluscent objects
  glDepthMask(GL_FALSE);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  drawAllTranslucentObjects();

  glfwSwapBuffers(theWindow);

  // checkForErrors(std::cout, "ProjController::handleDisplay");
}

bool ProjController::drawingOpaque() const {
	return sceneHasTranslucentObjects;
}

void ProjController::drawAllOpaqueObjects() {
  sceneHasTranslucentObjects = true;
  for (std::vector<ModelView*>::iterator it = models.begin(); it != models.end(); ++it)
    (*it)->render();
}

void ProjController::drawAllTranslucentObjects() {
  sceneHasTranslucentObjects = false;
  for (std::vector<ModelView*>::iterator it = models.begin(); it != models.end(); ++it)
    (*it)->render();
}
