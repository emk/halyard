// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include "TCommon.h"
#include "Model.h"
#include "ModelView.h"

USING_NAMESPACE_FIVEL
using namespace model;


//=========================================================================
//  View Methods
//=========================================================================

View::View()
	: mObject(NULL), mObjectIsLive(false)
{
}

View::~View()
{
	if (mObject)
		mObject->UnregisterView(this);
}

void View::SetObject(Object *inObject)
{
	ASSERT(inObject);
	ASSERT(!mObject);
	mObject = inObject;
	mObject->RegisterView(this);
	CallObjectChanged();
}

Object *View::GetObject()
{
	ASSERT(mObject && ObjectIsLive());
	return mObject;
}

void View::CallObjectChanged()
{
	mObjectIsLive = true;
	ObjectChanged();
}

void View::CallObjectDeleted()
{
	mObjectIsLive = false;
	ObjectDeleted();
}

void View::ClearObject()
{
	// We are called by the destructor of Object, and we should set any
	// out-of-date pointers to NULL.
	ASSERT(mObject);
	mObject = NULL;
	CallObjectDeleted();
}
