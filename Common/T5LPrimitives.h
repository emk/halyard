// -*- Mode: C++; tab-width: 4; -*-

#ifndef T5LPrimitives_H
#define T5LPrimitives_H

#include "TCommon.h"
#include "TPrimitives.h"

BEGIN_NAMESPACE_FIVEL

//////////
// Register a bunch of primitive commands with the gPrimitiveManager.
// These commands aren't necessary for any language other than the
// old 5L interpeter, which has no built in commands other than
// those provided by the engine.
//
extern void Register5LPrimitives();

END_NAMESPACE_FIVEL

#endif // T5LPrimitives_H
