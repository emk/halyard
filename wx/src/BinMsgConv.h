// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#ifndef BinMsgConv_H
#define BinMsgConv_H

#include "wxquake2.h"

class BinMsg {
    binmsg_message mMessage;
    TValue mArgs;

    TValue ConvArray(binmsg_array *inArray);
    TValue ConvNext(binmsg_array *inArray);

public:
    BinMsg(binmsg_byte *buffer, size_t size);
    
    std::string GetName() { return mMessage.name; }
    TValue GetArgs() { return mArgs; }
};

#endif // BinMsgConv_H

