// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#ifndef TamaleProgram_H
#define TamaleProgram_H

#include "Model.h"

BEGIN_NAMESPACE_FIVEL

class TamaleProgram : public model::Object {
public:
	DECLARE_MODEL_CLASS(TamaleProgram);
	virtual void Initialize();
};

END_NAMESPACE_FIVEL

#endif // TamaleProgram_H
