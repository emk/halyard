// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#ifndef Background_H
#define Background_H

#include "Model.h"

BEGIN_NAMESPACE_FIVEL

class Background : public model::Object {
public:
	DECLARE_MODEL_CLASS(Background);
	virtual void Initialize();
};

END_NAMESPACE_FIVEL

#endif // Background_H
