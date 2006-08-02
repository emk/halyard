// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
// @BEGIN_LICENSE
//
// Tamale - Multimedia authoring and playback system
// Copyright 1993-2006 Trustees of Dartmouth College
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

// This needs to come before STL and boost headers; it surpresses various
// bogus compiler warnings when processing template code.
#include "TCommon.h"

// Commonly-used STL headers.
#include <string>
#include <vector>
#include <map>
#include <list>
#include <deque>
#include <memory>
#include <algorithm>

// Commonly-used I/O headers.  We mostly want these for std::stringstream,
// which is the Right Way<tm> to build formatted output in C++.
#include <iostream>
#include <sstream>

// Commonly-used Boost headers.
#define BOOST_ALL_NO_LIB 1
#include <boost/config.hpp> // Get working std::max under MSVC.
#include <boost/utility.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/shared_array.hpp>
#include <boost/scoped_ptr.hpp>
#include <boost/scoped_array.hpp>
#include <boost/checked_delete.hpp>
using boost::shared_ptr;
using boost::shared_array;
using boost::detail::dynamic_cast_tag;
using boost::scoped_ptr;
using boost::scoped_array;

// Commonly-used "Common" headers.
#include "TException.h"
#include "TPoint.h"
#include "TRect.h"
#include "TPolygon.h"
#include "GraphicsTools.h"
#include "TValue.h"
#include "TVariableManager.h"
#include "TTemplateUtils.h"
#include "TLogger.h"
#include "TestCase.h"
