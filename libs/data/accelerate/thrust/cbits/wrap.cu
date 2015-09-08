#include "wrap.h"

#include <thrust/device_ptr.h>
#include <thrust/sort.h>

void thrustSortF(float* devicePtr, int numOfElems) {
    thrust::device_ptr<float> devArray(devicePtr);
    thrust::sort(devArray, devArray + numOfElems);
}

void thrustSortD(double* devicePtr, int numOfElems) {
	thrust::device_ptr<double> devArray(devicePtr);
	thrust::sort(devArray, devArray + numOfElems);
}

void thrustSortI(long long* devicePtr, int numOfElems) {
	thrust::device_ptr<long long> devArray(devicePtr);
	thrust::sort(devArray, devArray + numOfElems);
}

void thrustSortW8(unsigned char* devicePtr, int numOfElems) {
	thrust::device_ptr<unsigned char> devArray(devicePtr);
	thrust::sort(devArray, devArray + numOfElems);
}
