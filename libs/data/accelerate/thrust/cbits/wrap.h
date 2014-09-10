
#ifndef __ACCELERATE_THRUST_WRAP_H__
#define __ACCELERATE_THRUST_WRAP_H__

extern "C" {

void thrustSortF(float* devicePtr, int numOfElems);
void thrustSortD(double* devicePtr, int numOfElems);
void thrustSortI(long long* devicePtr, int numOfElems);
void thrustSortW8(unsigned char* devicePtr, int numOfElems);

}

#endif
