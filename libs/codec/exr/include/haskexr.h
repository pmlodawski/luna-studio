#include "ImfInputFile.h"
#include <stdint.h>

extern "C" {

void* openFile(const char* filename);
void closeFile(void* fileHandle);

int parts(void* fileHandle);
int version(void* fileHandle);

// predefined atributes accesors
void displayWindowUnsafe(void* fileHandle, int part, int* minx, int* miny, int* maxx, int* maxy);
void dataWindowUnsafe(void* fileHandle, int part, int* x, int* y, int* maxx, int* maxy);

char* channels(void* fileHandle, int part, int* channelsNamesLength);

float* readScanlineChannelUnsafe(void* fileHandle, int part, const char* chanName, int* height, int* width);
// float* readDeepScanlineChannelUnsafe(void* fileHandle, int part, const char* chanName, int* height, int* width);
float* readTileFromChannelUnsafe(void* fileHandle, int part, const char* chanName, int xTilePosition, int yTilePosition, int* height, int* width);
// float* readTileFromChannelUnsafe(void* fileHandle, int part, const char* chanName, int* height, int* width);
float* readTiledScanlineChannelUnsafe(void* fileHandle, int part, const char* chanName, int* height, int* width);

void saveTest(const char* fileName, const char* chanName, int height, int width, float* buffer);
void saveChannelsToFile(const char* fileName, const char** chanNames, float** buffers, int channels, int height, int width);

char* getPartType(void* fileHandle, int part);
char* getPartName(void* fileHandle, int part);
int   getPartVersion(void* fileHandle, int part, int* version);
char* getPartView(void* fileHandle, int part);
float getPixelAspectRatio(void* fileHandle, int part);
float getScreenWindowWidth(void* fileHandle, int part);
void  getScreenWindowCenter(void* fileHandle, int part, float* x, float* y);

void dumpImageInfo(void* fileHandle);

}