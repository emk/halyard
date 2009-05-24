// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
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

#include "AppHeaders.h"

#define IN_DRAWING_AREA_OPT 1

#include "DrawingAreaOpt.h"

using namespace Halyard;

using GraphicsTools::Channel;
using GraphicsTools::AlphaBlendChannel;
using GraphicsTools::MultiplyChannels;
using GraphicsTools::Color;
using GraphicsTools::Distance;
using GraphicsTools::Point;

typedef wxNativePixelData::Iterator NativeIterator;
typedef wxAlphaPixelData::Iterator AlphaIterator;

template <class Iterator>
static void BlendPixel(Iterator &i, const Color &c);

template <> __inline static void
BlendPixel<NativeIterator>(NativeIterator &i, const Color &c) {
	i.Red()   = AlphaBlendChannel(i.Red(),   c.red,   c.alpha);
	i.Green() = AlphaBlendChannel(i.Green(), c.green, c.alpha);
	i.Blue()  = AlphaBlendChannel(i.Blue(),  c.blue,  c.alpha);
}

template <> __inline static void
BlendPixel<AlphaIterator>(AlphaIterator &i, const Color &c) {
	// We need to combine two colors, each with an alpha value.  Let:
	//
	//   x = composited background
	//   y = layer pointed to by 'i' (range 0 to 1)
    //   z = layer pointed to by 'c'
	//   m = blend of x, y and z layers
	//   n = blend of y and z layers
    //   a_n = alpha value for layer n (range 0 to 1, 1 is opaque)
    //   \bar{a_n} = 1 - a_n
	//
	// We know that:
	//
    //   m = (x\bar{a_y} + ya_y)\bar{a_z} + za_z
    //   m = x\bar{a_n} + na_n
	//
	// If we solve for n and na_n, we get:
	//
	//   n = \over{ya_y\bar{a_z} + za_z}{a_n}
	//   a_n = \bar{\bar{a_y}\bar{a_z}}
	//
	// However, reality is a bit more rude: 'i' points towards 255-opaque,
	// premuliplied pixel data.  'c' contains 255-opaque, non-premuliplied
	// pixel data.  So we actually need something like this:
	i.Red()   = AlphaBlendChannel(i.Red(),   c.red,   c.alpha);
	i.Green() = AlphaBlendChannel(i.Green(), c.green, c.alpha);
	i.Blue()  = AlphaBlendChannel(i.Blue(),  c.blue,  c.alpha);
	i.Alpha() = Color::CombineAlphaChannels(i.Alpha(), c.alpha);
}
