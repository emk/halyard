// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-
// @BEGIN_LICENSE
//
// Halyard - Multimedia authoring and playback system
// Copyright 1993-2009 Trustees of Dartmouth College
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

#include "CommonHeaders.h"
#include <cairo.h>

using namespace GraphicsTools;


//=========================================================================
// GreyMap Methods
//=========================================================================

GreyMap::GreyMap(Distance inWidth, Distance inHeight)
    : width(inWidth), height(inHeight),
      stride(cairo_format_stride_for_width(CAIRO_FORMAT_A8, inWidth)),
      pixels(new Channel[stride * height])
{
    ASSERT(stride >= 0);
    // All done.
}

GreyMap::~GreyMap() {
    delete [] pixels;
}

void GreyMap::Clear(Channel inColor) {
    Channel *end = pixels + stride * height;
    for (Channel *cursor = pixels; cursor < end; cursor++)
        *cursor = inColor;
}
