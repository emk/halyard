#include "TPrimitives.h"
#include "TException.h"

USING_NAMESPACE_FIVEL

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
