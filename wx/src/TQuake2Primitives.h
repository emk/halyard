// -*- Mode: C++; tab-width: 4; -*-

#ifndef TQuake2Primitives_H
#define TQuake2Primitives_H

#include "TPrimitives.h"
#include "AppConfig.h"

#if CONFIG_HAVE_QUAKE2

BEGIN_NAMESPACE_FIVEL

//////////
// Register a set of primitives for working with Quake 2.
//
extern void RegisterQuake2Primitives();

END_NAMESPACE_FIVEL

#endif // CONFIG_HAVE_QUAKE2
#endif // TQuake2Primitives_H
