#include "ArrayOps_hsc.h"
#line 28 "ArrayOps.hsc"
void c_cvSubRS(CvArr* src1, CvScalar* src2, CvArr* dst, const CvArr* mask) { cvSubRS(src1, *src2, dst, mask); }
#line 56 "ArrayOps.hsc"
void c_cvAbsDiffS(const CvArr* src, CvArr* dst, CvScalar* value) {  cvAbsDiffS(src, dst, *value);}
#line 140 "ArrayOps.hsc"
void c_cvAndS(const CvArr* src, CvScalar* value, CvArr* dst, const CvArr* mask) { cvAndS(src, *value, dst, mask); }
#line 157 "ArrayOps.hsc"
void c_cvScaleAdd(const CvArr* src1, CvScalar* scale,                       const CvArr* src2, CvArr* dst) {  cvScaleAdd(src1, *scale, src2, dst);}
#line 213 "ArrayOps.hsc"
void c_cvAddS(const CvArr* src, CvScalar* value, CvArr* dst,                   const CvArr* mask) {  cvAddS(src, *value, dst, mask);}
#line 267 "ArrayOps.hsc"
void c_cvOrS(const CvArr* src, CvScalar* value, CvArr* dst,                  const CvArr* mask) {  cvOrS(src, *value, dst, mask);}
#line 287 "ArrayOps.hsc"
void c_cvSet(CvArr* src, CvScalar* value, const CvArr* mask) {  cvSet(src, *value, mask);}
