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
#include <boost/config.hpp> // Get working std::max under MSVC.
#include <boost/utility.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/shared_array.hpp>
#include <boost/scoped_ptr.hpp>
#include <boost/scoped_array.hpp>
#include <boost/checked_delete.hpp>

// Commonly-used "Common" headers.
#include "TException.h"
#include "TObject.h"
#include "TString.h"
#include "TPoint.h"
#include "TRect.h"
#include "TPolygon.h"
#include "GraphicsTools.h"
#include "TValue.h"
#include "TVariableManager.h"
#include "TTemplateUtils.h"
#include "TLogger.h"
#include "TestCase.h"
