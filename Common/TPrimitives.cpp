// -*- Mode: C++; tab-width: 4; -*-

#include "TCommon.h"
#include "TPrimitives.h"
#include "TException.h"
#include "TLogger.h"

USING_NAMESPACE_FIVEL

TPrimitiveManager FIVEL_NS gPrimitiveManager;


//=========================================================================
// TArgumentList Methods
//=========================================================================

TArgumentList &FIVEL_NS operator>>(TArgumentList &args, TString &out)
{
    out = TString(args.GetStringArg().c_str());
    return args;
}

TArgumentList &FIVEL_NS operator>>(TArgumentList &args, std::string &out)
{
    out = args.GetStringArg();
    return args;
}

TArgumentList &FIVEL_NS operator>>(TArgumentList &args, int16 &out)
{
    int32 temp;
    temp = args.GetInt32Arg();
    if (temp < MIN_INT16 || MAX_INT16 < temp)
	throw TException("Can't represent value as 16-bit integer");
    out = temp;
    return args;
}

TArgumentList &FIVEL_NS operator>>(TArgumentList &args, int32 &out)
{
    out = args.GetInt32Arg();
    return args;
}

TArgumentList &FIVEL_NS operator>>(TArgumentList &args, uint32 &out)
{
    out = args.GetUInt32Arg();
    return args;
}

TArgumentList &FIVEL_NS operator>>(TArgumentList &args, double &out)
{
    out = args.GetDoubleArg();
    return args;
}

TArgumentList &FIVEL_NS operator>>(TArgumentList &args, TRect &out)
{
    out = args.GetRectArg();
    return args;
}

TArgumentList &FIVEL_NS operator>>(TArgumentList &args, TPoint &out)
{
    out = args.GetPointArg();
    return args;
}

TArgumentList &FIVEL_NS operator>>(TArgumentList &args,
				   GraphicsTools::Color &out)
{
    out = args.GetColorArg();
    return args;
}

TArgumentList &FIVEL_NS operator>>(TArgumentList &args, TCallback* &out)
{
    out = args.GetCallbackArg();
    return args;
}


//=========================================================================
// TPrimitiveManager Methods
//=========================================================================

void TPrimitiveManager::RegisterPrimitive(const std::string &inName,
										  PrimitiveFunc inFunc)
{
    ASSERT(inName != "");
    ASSERT(inFunc != NULL);

    // Erase any existing primitive with this name.
    std::map<std::string,void*>::iterator existing =
	mPrimitiveMap.find(inName);
    if (existing != mPrimitiveMap.end())
    {
		gDebugLog.Log("Replacing primitive <%s>", inName.c_str());
		mPrimitiveMap.erase(existing);
    }
    
    // Insert the new entry.
    mPrimitiveMap.insert(std::pair<std::string,void*>(inName, inFunc));
}

bool TPrimitiveManager::DoesPrimitiveExist(const std::string &inName)
{
    ASSERT(inName != "");

    std::map<std::string,void*>::iterator found = mPrimitiveMap.find(inName);
    if (found != mPrimitiveMap.end())
		return true;
    else
		return false;
}

void TPrimitiveManager::CallPrimitive(const std::string &inName,
									  TArgumentList &inArgs)
{
    ASSERT(inName != "");
    
    // Find the primitive.
    std::map<std::string,void*>::iterator found = mPrimitiveMap.find(inName);
    if (found == mPrimitiveMap.end())
		throw TException("Tried to call non-existant primitive");
    PrimitiveFunc primitive = static_cast<PrimitiveFunc>(found->second);
    
    // Call it.
    (*primitive)(inArgs);
}
