#ifndef SRC_OPENCV_DRAWING_HSC_H
#define SRC_OPENCV_DRAWING_HSC_H
#include <HsFFI.h>
#if __NHC__
#undef HsChar
#define HsChar int
#endif
#line 56 "Drawing.hsc"
#include <opencv2/core/core_c.h>
#line 58 "Drawing.hsc"
extern void cvPutText_wrap(CvArr* img, const char* text, CvPoint* org,                         const CvFont* font, CvScalar* color) ;
#endif
