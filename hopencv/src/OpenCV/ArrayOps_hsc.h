#ifndef SRC_OPENCV_ARRAYOPS_HSC_H
#define SRC_OPENCV_ARRAYOPS_HSC_H
#include <HsFFI.h>
#if __NHC__
#undef HsChar
#define HsChar int
#endif
#line 26 "ArrayOps.hsc"
#include <opencv2/core/core_c.h>
#line 28 "ArrayOps.hsc"
extern void c_cvSubRS(CvArr* src1, CvScalar* src2, CvArr* dst, const CvArr* mask) ;
#line 56 "ArrayOps.hsc"
extern void c_cvAbsDiffS(const CvArr* src, CvArr* dst, CvScalar* value) ;
#line 140 "ArrayOps.hsc"
extern void c_cvAndS(const CvArr* src, CvScalar* value, CvArr* dst, const CvArr* mask) ;
#line 157 "ArrayOps.hsc"
extern void c_cvScaleAdd(const CvArr* src1, CvScalar* scale,                       const CvArr* src2, CvArr* dst) ;
#line 213 "ArrayOps.hsc"
extern void c_cvAddS(const CvArr* src, CvScalar* value, CvArr* dst,                   const CvArr* mask) ;
#line 267 "ArrayOps.hsc"
extern void c_cvOrS(const CvArr* src, CvScalar* value, CvArr* dst,                  const CvArr* mask) ;
#line 287 "ArrayOps.hsc"
extern void c_cvSet(CvArr* src, CvScalar* value, const CvArr* mask) ;
#endif
