#include "FloodFill_hsc.h"
#line 26 "FloodFill.hsc"
void cvFloodFill_wrap(CvArr* img, CvPoint* seedPt,                           CvScalar* newVal, CvScalar* loDiff,                           CvScalar* upDiff, void *comp, int flags,                           CvArr* mask) {  cvFloodFill(img, *seedPt, *newVal, *loDiff, *upDiff, comp, flags, mask);}
