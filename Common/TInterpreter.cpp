// -*- Mode: C++; tab-width: 4; -*-

#include "TInterpreter.h"

USING_NAMESPACE_FIVEL

TInterpreter *TInterpreter::sInstance = NULL;

TInterpreter::TInterpreter()
{
    ASSERT(sInstance == NULL);
    sInstance = this;
}

TInterpreter::~TInterpreter()
{
    sInstance = NULL;
}
