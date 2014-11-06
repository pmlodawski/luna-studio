#include "haskexr.h"

#include "ImfMultiPartInputFile.h"
#include "ImfDeepScanLineInputPart.h"
#include "ImfChannelList.h"
#include "ImfAttribute.h"
#include "ImfFrameBuffer.h"
#include "ImfDeepFrameBuffer.h"
#include "ImfInputPart.h"
#include "ImfTiledInputPart.h"
#include "Iex.h"

#include <algorithm>
#include <memory>
#include <vector>
#include <stdio.h>
#include <iostream>

namespace IMF = OPENEXR_IMF_NAMESPACE;



void* openFile(const char* filename) {
    try {
        auto exrFile = new Imf::MultiPartInputFile {filename};

        return exrFile;
    } catch (IEX_NAMESPACE::BaseExc &e) {
        printf("%s\n", e.what());
        return nullptr;
    } catch (...) {
        printf("Unknown error\n");
        return nullptr;
    }
}

void closeFile(void* fileHandle) {
    auto exrFile = static_cast<Imf::MultiPartInputFile*>(fileHandle);
    delete exrFile;
}

int parts(void* fileHandle) {
    auto exrFile = static_cast<Imf::MultiPartInputFile*>(fileHandle);
    return exrFile->parts();
}

int version(void* fileHandle) {
    auto exrFile = static_cast<Imf::MultiPartInputFile*>(fileHandle);
    return exrFile->version();
}

// will segfault if part is not present in file
void displayWindowUnsafe(void* fileHandle, int part, int* minx, int* miny, int* maxx, int* maxy) {
    auto exrFile = static_cast<Imf::MultiPartInputFile*>(fileHandle);

    *minx = exrFile->header(part).displayWindow().min.x;
    *miny = exrFile->header(part).displayWindow().min.y;

    *maxx = exrFile->header(part).displayWindow().max.x;
    *maxy = exrFile->header(part).displayWindow().max.y;
}

void dataWindowUnsafe(void* fileHandle, int part, int* minx, int* miny, int* maxx, int* maxy) {
    auto exrFile = static_cast<Imf::MultiPartInputFile*>(fileHandle);

    *minx = exrFile->header(part).dataWindow().min.x;
    *miny = exrFile->header(part).dataWindow().min.y;

    *maxx = exrFile->header(part).dataWindow().max.x;
    *maxy = exrFile->header(part).dataWindow().max.y;
}

char* channels(void* fileHandle, int part, int* channelsNamesLength) {
    auto exrFile = static_cast<Imf::MultiPartInputFile*>(fileHandle);

    auto& channels = exrFile->header(part).channels();

    std::vector<char> channelsNamesVector;
    for (auto it = channels.begin(); it != channels.end(); it++) {
        auto chanName = it.name();

        // explicitly add NULL character to overall length
        std::vector<char>::size_type chanNameLen = strlen(chanName) + 1;
        std::vector<char> channelVector (chanName, chanName + chanNameLen);

        std::for_each(begin(channelVector), end(channelVector), [&](char x){
            channelsNamesVector.push_back(x);
        });
    }

    *channelsNamesLength = channelsNamesVector.size();
    auto channelsNames = static_cast<char*>(malloc(*channelsNamesLength * sizeof(char)));
    memcpy(channelsNames, channelsNamesVector.data(), *channelsNamesLength);

    return channelsNames;
}

float* readScanlineChannelUnsafe(void* fileHandle, int part, const char* chanName, int* height, int* width) {
    auto exrFile = static_cast<Imf::MultiPartInputFile*>(fileHandle);

    auto dataWindow = exrFile->header(part).dataWindow();
    *width  = dataWindow.max.x - dataWindow.min.x + 1;
    *height = dataWindow.max.y - dataWindow.min.y + 1;

    Imf::InputPart inputPart {*exrFile, part};

    auto channel = inputPart.header().channels().findChannel(chanName);
    if (channel == 0)
        return nullptr;

    auto buffer = static_cast<float*>(calloc(*width * *height, sizeof(float)));


    Imf::FrameBuffer framebuffer;

    framebuffer.insert(
        chanName,
        Imf::Slice(
            IMF::FLOAT,
            (char*)buffer,
            sizeof(float) * 1,
            sizeof(float) * *width,
            channel->xSampling,
            channel->ySampling,
            0.0));

    inputPart.setFrameBuffer(framebuffer);
    inputPart.readPixels(dataWindow.min.y, dataWindow.max.y);

    return buffer;
}

float* readTileFromChannelUnsafe(void* fileHandle, int part, const char* chanName, int xTilePosition, int yTilePosition, int* height, int* width) {
    auto exrFile = static_cast<Imf::MultiPartInputFile*>(fileHandle);

    Imf::TiledInputPart tiledInputPart {*exrFile, part};

    auto channel = tiledInputPart.header().channels().findChannel(chanName);
    if (channel == 0)
        return nullptr;

    auto tileDescription = tiledInputPart.header().tileDescription();
    *width = tileDescription.xSize;
    *height = tileDescription.ySize;

    auto buffer = static_cast<float*>(calloc(*width * *height, sizeof(float)));
    Imf::FrameBuffer frameBuffer;

    frameBuffer.insert(
        chanName,
        Imf::Slice(
            IMF::FLOAT,
            (char*)buffer,
            sizeof(float) * channel->xSampling,
            sizeof(float) * channel->ySampling * *width,
            channel->xSampling,
            channel->ySampling));

    tiledInputPart.setFrameBuffer(frameBuffer);

    try {
        tiledInputPart.readTile(0, 0, 0, 0);
    }
    catch (const Iex::BaseExc &e) {
        // FIXME[mm]: log exception
    }

    return buffer;
}

float* readTiledScanlineChannelUnsafe(void* fileHandle, int part, const char* chanName, int* height, int* width) {
    auto exrFile = static_cast<Imf::MultiPartInputFile*>(fileHandle);

    Imf::TiledInputPart tiledInputPart {*exrFile, part};

    auto channel = tiledInputPart.header().channels().findChannel(chanName);
    if (channel == 0)
        return nullptr;

    auto dataWindow = tiledInputPart.dataWindowForLevel(0, 0);
    *width = dataWindow.max.x - dataWindow.min.x + 1;
    *height = dataWindow.max.y - dataWindow.min.y + 1;

    auto dx = dataWindow.min.x;
    auto dy = dataWindow.min.y;

    auto buffer = static_cast<float*>(calloc(*width * *height, sizeof(float)));

    Imf::FrameBuffer frameBuffer;

    frameBuffer.insert(
        chanName,
        Imf::Slice(
            IMF::FLOAT,
            (char*)&buffer[-dx - dy * *width],
            sizeof(float) * channel->xSampling,
            sizeof(float) * channel->ySampling * *width,
            channel->xSampling,
            channel->ySampling));

    tiledInputPart.setFrameBuffer(frameBuffer);

    try {
        auto tx = tiledInputPart.numXTiles(0);
        auto ty = tiledInputPart.numYTiles(0);

        if (tiledInputPart.header().lineOrder() == IMF::INCREASING_Y) {
            for (auto y = 0; y < ty; ++y)
                for (auto x = 0; x < tx; ++x)
                    tiledInputPart.readTile(x, y, 0, 0);
        }
        else {
            for (auto y = ty - 1; y >= 0; --y)
                for (auto x = 0; x < tx; ++x)
                    tiledInputPart.readTile(x, y, 0, 0);
        }
    }
    catch (const Iex::BaseExc &e) {
        // FIXME[mm]: log these errors or something, returning damaged image should be
        //            acceptable
    }

    return buffer;
}


// float* readDeepScanlineChannelUnsafe(void* fileHandle, int part, const char* chanName, int* height, int* width) {
//     auto exrFile = static_cast<Imf::MultiPartInputFile*>(fileHandle);

//     auto dataWindow = exrFile->header(part).dataWindow();
//     *width = dataWindow.max.x - dataWindow.min.x + 1;
//     *height = dataWindow.max.y - dataWindow.min.y + 1;

//     Imf::DeepScanLineInputPart inputPart {*exrFile, part};

//     auto channel = inputPart.header().channels().findChannel(chanName);
//     if (channel == 0)
//         return nullptr;

//     auto buffer = static_cast<float*>(malloc(*width * *height * sizeof(float)));
//     auto sampleCount = static_cast<unsigned int>(calloc(*width * *height, sizeof(unsigned int)));

//     printf("Sampling %dx%d\n", channel->xSampling, channel->ySampling);

//     Imf::DeepFrameBuffer frameBuffer;

//     frameBuffer.insertSampleCountSlice(
//         Imf::Slice(
//             IMF::UINT,
//             (char*)(sampleCount - dataWindow.min.x - dataWindow.min.y * *width),
//             sizeof(unsigned int) * 1,
//             sizeof(unsigned int) * *width));

//     // frameBuffer.insert(
//     //     chanName,
//     //     Imf::DeepSlice(
//     //         IMF::FLOAT,
//     //         (char*)buffer,
//     //         sizeof(float) * 1,
//     //         sizeof(float) * *width,
//     //         channel->xSampling,
//     //         channel->ySampling,
//     //         0.0));

//     inputPart.setFrameBuffer(frameBuffer);
//     inputPart.readPixelSampleCounts(dataWindow.min.y, dataWindow.max.y);

//     inputPart.readPixels(dataWindow.min.y, dataWindow.max.y);

//     return buffer;
// }

char* getPartType(void* fileHandle, int part) {
    auto exrFile = static_cast<Imf::MultiPartInputFile*>(fileHandle);

    char* result = nullptr;

    if (exrFile->header(part).hasType()) {
        auto type = exrFile->header(part).type().c_str();
        auto typeLength = strlen(type) + 1;

        result = static_cast<char*>(malloc(sizeof(char) * typeLength));
        memcpy(result, type, typeLength);
    }

    return result;
}

char* getPartName(void* fileHandle, int part) {
    auto exrFile = static_cast<Imf::MultiPartInputFile*>(fileHandle);

    char* result = nullptr;

    if (exrFile->header(part).hasName()) {
        auto name = exrFile->header(part).name().c_str();
        auto nameLength = strlen(name) + 1;

        result = static_cast<char*>(malloc(sizeof(char) * nameLength));
        memcpy(result, name, nameLength);
    }

    return result;
}

char* getPartView(void* fileHandle, int part) {
    auto exrFile = static_cast<Imf::MultiPartInputFile*>(fileHandle);

    char* result = nullptr;

    if (exrFile->header(part).hasView()) {
        auto view = exrFile->header(part).view().c_str();
        auto viewLength = strlen(view) + 1;

        result = static_cast<char*>(malloc(sizeof(char) * viewLength));
        memcpy(result, view, viewLength);
    }

    return result;
}

int getPartVersion(void* fileHandle, int part, int* version) {
    auto exrFile = static_cast<Imf::MultiPartInputFile*>(fileHandle);

    if (exrFile->header(part).hasVersion()) {
        *version = exrFile->header(part).version();

        return 0;
    }

    return -1;
}

float getPixelAspectRatio(void* fileHandle, int part) {
    auto exrFile = static_cast<Imf::MultiPartInputFile*>(fileHandle);

    return exrFile->header(part).pixelAspectRatio();
}

float getScreenWindowWidth(void* fileHandle, int part) {
    auto exrFile = static_cast<Imf::MultiPartInputFile*>(fileHandle);

    return exrFile->header(part).screenWindowWidth();
}

void getScreenWindowCenter(void* fileHandle, int part, float* x, float* y) {
    auto exrFile = static_cast<Imf::MultiPartInputFile*>(fileHandle);

    *x = exrFile->header(part).screenWindowCenter().x;
    *y = exrFile->header(part).screenWindowCenter().y;
}

void dumpImageInfo(void* fileHandle) {
    auto exrFile = static_cast<Imf::MultiPartInputFile*>(fileHandle);

    printf("========= FILE INFO DUMP ==========\n");
    printf("Parts: %d\n", exrFile->parts());
    printf("Version: %d\n", exrFile->version());

    for (auto i = 0; i < exrFile->parts(); ++i) {
        auto header = exrFile->header(i);

        printf("=== Part %d ===\n", i);
        if (exrFile->partComplete(i))
            printf("Part is complete\n");
        else
            printf("Part is not complete\n");

        printf("All attributes:\n");
        for (auto it = header.begin(); it != header.end(); ++it) {
            printf("%s of type %s\n", it.name(), it.attribute().typeName());
        }

        printf("Known attributes:\n");
        printf("displayWindow (%d x %d) x (%d x %d)\n", header.displayWindow().min.x, header.displayWindow().min.y,
            header.displayWindow().max.x, header.displayWindow().max.y);
        printf("dataWindow (%d x %d) x (%d x %d)\n", header.dataWindow().min.x, header.dataWindow().min.y,
            header.dataWindow().max.x, header.dataWindow().max.y);
        printf("pixelAspectRatio %f\n", header.pixelAspectRatio());
        printf("screenWindowCenter %f x %f\n", header.screenWindowCenter().x, header.screenWindowCenter().y);
        printf("screenWindowWidth %f\n", header.screenWindowWidth());

        printf("Channels:\n");
        for (auto it = header.channels().begin(); it != header.channels().end(); ++it) {
            printf("%s ", it.name());
            printf("pixelType: %d ", it.channel().type);
            printf("x/y sampling: %d/%d ", it.channel().xSampling, it.channel().ySampling);
            printf("linear: %d\n", it.channel().pLinear);
        }

        printf("Line order: %d\n", header.lineOrder());
        printf("Compression: %d\n", header.compression());

        printf("Optional attributes:\n");
        try {
            printf("name: %s\n", header.name().c_str());
        } catch (std::exception &e) {}
        try{
            printf("type: %s\n", header.type().c_str());
        } catch (std::exception &e) {}
        try {
            printf("version: %d\n", header.version());
        } catch (std::exception &e) {}
        try {
            printf("chunk count: %d\n", header.chunkCount());
        } catch (std::exception &e) {}
        try {
            printf("view: %s\n", header.view().c_str());
        } catch (std::exception &e) {}
        try {
            auto tileDescription = header.tileDescription();
            printf("Tile description:\n");
            printf("Size: %d x %d (x, y)\n", tileDescription.xSize, tileDescription.ySize);
            printf("Level mode: %d\n", tileDescription.mode);
            printf("Level rounding mode: %d\n", tileDescription.roundingMode);

            Imf::TiledInputPart tiledInputPart {*exrFile, 0};

            printf("X tiles: %d\n", tiledInputPart.numXTiles());
            printf("Y tiles: %d\n", tiledInputPart.numYTiles());
            printf("Level (0,0) is valid: %s\n", std::to_string(tiledInputPart.isValidLevel(0, 0)).c_str());
        } catch (std::exception &e) {}
        if (header.hasPreviewImage())
            printf("has preview image\n");
    }
    printf("======== DONE =========\n");
}
