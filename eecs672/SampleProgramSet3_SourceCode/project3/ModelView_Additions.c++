// Extracted from: ModelView.c++ - an Abstract Base Class for a combined Model and View for OpenGL

#include "ModelView.h"

void ModelView::addToGlobalPan(double dxInLDS, double dyInLDS, double dzInLDS)
{
	// 1. Use last_ecXmin, et al. with dxInLDS, et al. to translate the LDS
	//          pan vector to an EC pan vector.
	double dxInEC = (dxInLDS / 2) * (last_ecXmax - last_ecXmin);
	double dyInEC = (dyInLDS / 2) * (last_ecYmax - last_ecYmin);
	cryph::Matrix4x4 tMatrix = cryph::Matrix4x4::translation(
		cryph::AffVector(dxInEC, dyInEC, dzInLDS));

	// 2. UPDATE dynamic_view
	// 3. The updated dynamic_view will be used in ModelView::getMatrices
	dynamic_view = tMatrix * dynamic_view;
}

void ModelView::addToGlobalRotationDegrees(double rx, double ry, double rz)
{
	// 1. UPDATE dynamic_view
	// 2. The updated dynamic_view will be used in ModelView::getMatrices
	cryph::Matrix4x4 xRot = cryph::Matrix4x4::xRotationDegrees(rx);
	cryph::Matrix4x4 yRot = cryph::Matrix4x4::yRotationDegrees(ry);
	cryph::Matrix4x4 zRot = cryph::Matrix4x4::yRotationDegrees(rz);
	dynamic_view = xRot * yRot * zRot * dynamic_view;
}

void ModelView::getMatrices(cryph::Matrix4x4& mc_ec, cryph::Matrix4x4& ec_lds)
{
	// 1. Create the mc_ec matrix:
	//    Create a local variable of type Matrix4x4 called M_ECu from the eye,
	//    center, and up. Recall that those values are initialized in the main
	//    program and may be modified at any time while processing interactive
	//    events. Their current values are stored in protected class variables
	//    that you directly access.
	//
	cryph::Matrix4x4 M_ECu = cryph::Matrix4x4::lookAt(ModelView::eye,
		ModelView::center, ModelView::up);

	// For project 2:
	// mc_ec = M_ECu;

	//    For project 3: Either:
	//        mc_ec = dynamic_view * M_ECu (if rotations are to be about the eye)
	//    or:
	//        mc_ec = postTrans * dynamic_view * preTrans * M_ECu (if
	//                         rotations are to be about the center of attention)
	//    NOTE: If using preTrans/postTrans, be sure you build them CORRECTLY here!!!
	cryph::AffVector distVec(0, 0, ModelView::distEyeCenter);
	cryph::Matrix4x4 preTrans = cryph::Matrix4x4::translation(distVec);
	cryph::Matrix4x4 postTrans = cryph::Matrix4x4::translation(-distVec);
	mc_ec = postTrans * dynamic_view * preTrans * M_ECu;

	// 2. Create the ec_lds matrix:
	//    Use the mcRegionOfInterest. (As with eye, center, up, the mcRegionOfInterest
	//    is initialized in main and is subject to modification at any time while
	//    processing events. Its current values are also stored in protected class
	//    variables that you directly access.
	//    2.a. Determine the maximum of delta_mcX, delta_mcY, and delta_mcZ. (For
	//         example, delta_mcX = mcRegionOfInterest[1] - mcRegionOfInterest[0].)
	//         Suppose you store the maximum of these delta_mc* values in "maxDelta".
	double deltaMCValues[3];
	deltaMCValues[0] = ModelView::mcRegionOfInterest[1] - ModelView::mcRegionOfInterest[0];
	deltaMCValues[1] = ModelView::mcRegionOfInterest[3] - ModelView::mcRegionOfInterest[2];
	deltaMCValues[2] = ModelView::mcRegionOfInterest[5] - ModelView::mcRegionOfInterest[4];

	double maxDelta = deltaMCValues[0];
	if (deltaMCValues[1] > maxDelta)
		maxDelta = deltaMCValues[1];
	if (deltaMCValues[2] > maxDelta)
		maxDelta = deltaMCValues[2];

	//    2.b. In project 3 & 4: Scale "halfWidth" by "dynamic_zoomScale"
	//    2.c. initialize the XY direction of the view volume as:
	double halfWidth = 0.5 * maxDelta;
	last_ecXmin = -halfWidth * dynamic_zoomScale;
	last_ecXmax = halfWidth * dynamic_zoomScale;
	last_ecYmin = -halfWidth * dynamic_zoomScale;
	last_ecYmax = halfWidth * dynamic_zoomScale;

	//    2.d. Use ModelView::matchAspectRatio to alter one of these pairs.
	double viewAspectRatio = Controller::getCurrentController()->getViewportAspectRatio();
	ModelView::matchAspectRatio(last_ecXmin, last_ecXmax, last_ecYmin, last_ecYmax, viewAspectRatio);

	if (ModelView::projType == ORTHOGONAL)
		ec_lds = cryph::Matrix4x4::orthogonal(last_ecXmin, last_ecXmax, last_ecYmin, last_ecYmax, ModelView::ecZmin, ModelView::ecZmax);
	else if (ModelView::projType == PERSPECTIVE)
		ec_lds = cryph::Matrix4x4::perspective(ModelView::ecZpp,
				last_ecXmin, last_ecXmax, last_ecYmin, last_ecYmax, ModelView::ecZmin, ModelView::ecZmax);
	else // Must be OBLIQUE
		ec_lds = cryph::Matrix4x4::oblique(ModelView::ecZpp,
				last_ecXmin, last_ecXmax, last_ecYmin, last_ecYmax, ModelView::ecZmin, ModelView::ecZmax, ModelView::obliqueProjectionDir);

	// THEN IN THE CALLER OF THIS METHOD:
	//
	// float mat[16];
	// glUniformMatrix4fv(shaderIF->ppuLoc("mc_ec"), 1, false, mc_ec.extractColMajor(mat));
	// glUniformMatrix4fv(shaderIF->ppuLoc("ec_lds"), 1, false, ec_lds.extractColMajor(mat));
	//
	// (The extractColMajor method copies the elements of the matrix into the given
	// array which is assumed to be of length 16. It then returns the array pointer
	// so it can be used as indicated in the two calls. Since the array is immediately
	// copied by glUniformMatrix to the GPU, "mat" can be reused as indicated.)
}

void ModelView::scaleGlobalZoom(double multiplier) {
	dynamic_zoomScale *= multiplier;
}
