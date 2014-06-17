#ifndef SRC_OPENCV_FLOODFILL_HSC_H
#define SRC_OPENCV_FLOODFILL_HSC_H
#include <HsFFI.h>
#if __NHC__
#undef HsChar
#define HsChar int
#endif
#line 19 "FloodFill.hsc"
#include <opencv2/imgproc/imgproc_c.h>
#line 26 "FloodFill.hsc"
extern void cvFloodFill_wrap(CvArr* img, CvPoint* seedPt,                           CvScalar* newVal, CvScalar* loDiff,                           CvScalar* upDiff, void *comp, int flags,                           CvArr* mask) ;
#endif
