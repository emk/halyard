// -*- Mode: C++; tab-width: 4; -*-

// WARNING!  This file lives in Common/, but because it requires all sorts
// of exciting platform headers, it's actually built as part of the
// platform-specific engines.  This is a wart.

#ifndef TQTPrimitives_H
#define TQTPrimitives_H

#include "TCommon.h"
#include "TPrimitives.h"

BEGIN_NAMESPACE_FIVEL

//////////
// Register a bunch of QuickTime-related commands with the gPrimitiveManager.
//
extern void RegisterQuickTimePrimitives();

END_NAMESPACE_FIVEL

#endif // TQTPrimitives_H
