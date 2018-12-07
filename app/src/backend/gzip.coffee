pako = require('pako');

export compressBytes   = (bytes) -> pako.gzip(bytes).buffer
export decompressBytes = (bytes) -> pako.ungzip(bytes).buffer

