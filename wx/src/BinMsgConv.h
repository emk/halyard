// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
// @BEGIN_LICENSE
//
// Halyard - Multimedia authoring and playback system
// Copyright 1993-2008 Trustees of Dartmouth College
// 
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation; either version 2
// of the License, or (at your option) any later version.
// 
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
//
// @END_LICENSE

#ifndef BinMsgConv_H
#define BinMsgConv_H

#include "wxquake2.h"

/// Wrapper for low-level C binary message type used to receive state
/// messages from Quake 2 AI DLL.
class BinMsg {
    binmsg_message mMessage;
    FIVEL_NS TValue mArgs;

    FIVEL_NS TValue ConvArray(binmsg_array *inArray);
    FIVEL_NS TValue ConvNext(binmsg_array *inArray);

public:
    BinMsg(binmsg_byte *buffer, size_t size);
    
    std::string GetName() { return mMessage.name; }
    FIVEL_NS TValue GetArgs() { return mArgs; }
};

#endif // BinMsgConv_H

