// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include "CommonHeaders.h"
#include "TPrimitives.h"

USING_NAMESPACE_FIVEL

TPrimitiveManager FIVEL_NS gPrimitiveManager;


//=========================================================================
// TArgumentList Methods
//=========================================================================

void TArgumentList::BeginLog(const std::string &inFunctionName)
{
	ASSERT(mDebugString.empty());
	mDebugString = inFunctionName + std::string(":");
}

void TArgumentList::LogParameter(const std::string &inParameterValue)
{
	if (!mDebugString.empty())
		mDebugString += std::string(" ") + inParameterValue;
}

std::string TArgumentList::EndLog()
{
	ASSERT(!mDebugString.empty());
	std::string result = mDebugString;
	mDebugString = "";
	return result;
}

std::string TArgumentList::GetStringArg() {
	TValue arg = GetNextArg(); 

	if(arg.GetType() == TValue::TYPE_SYMBOL) {
		gDebugLog.Caution("Symbol '%s passed as string argument.",
						  TSymbol(arg).GetName().c_str());
		
		return TSymbol(arg).GetName();
	}
	
	return std::string(arg);
}

std::string TArgumentList::GetSymbolArg() {
	TValue arg = GetNextArg(); 
	return TSymbol(arg).GetName();
}

int32 TArgumentList::GetInt32Arg() {
	TValue arg = GetNextArg(); 
	return int32(arg);
}

uint32 TArgumentList::GetUInt32Arg() {	
    TValue arg = GetNextArg(); 
	return uint32(arg);
}

bool TArgumentList::GetBoolArg() {
	TValue arg = GetNextArg(); 
	return bool(arg);
}

double TArgumentList::GetDoubleArg() {	
	TValue arg = GetNextArg(); 
	return double(arg);
}

TPoint TArgumentList::GetPointArg() {
	TValue arg = GetNextArg(); 
	return TPoint(arg);
}

TRect TArgumentList::GetRectArg() {
	TValue arg = GetNextArg(); 
	return TRect(arg);
}

TPolygon TArgumentList::GetPolygonArg() {
	TValue arg = GetNextArg(); 
	return TPolygon(arg);
}

GraphicsTools::Color TArgumentList::GetColorArg() {
	TValue arg = GetNextArg(); 
	return GraphicsTools::Color(arg);
}

void TArgumentList::GetValueOrPercentArg(bool &outIsPercent,
										 int32 &outValue)
{
	TValue arg = GetNextArg(); 

	if(arg.GetType() != TValue::TYPE_PERCENT) {
		outIsPercent = false;
		outValue = int32(arg);
	} else {
		outIsPercent = true;
		outValue = TPercent(arg).GetValue();
	}
}

TCallbackPtr TArgumentList::GetCallbackArg() {
	TValue arg = GetNextArg();
	return arg.GetCallbackPtr();
}

TArgumentList *TArgumentList::GetListArg() {
	TValue arg = GetNextArg(); 
	return new TArgumentList(TValueList(arg));
}

TArgumentList::TArgumentList(TValueList inVal) {
	mArgList = TValueList(inVal);
	mArgPtr = mArgList.begin();
}

TArgumentList &FIVEL_NS operator>>(TArgumentList &args, TString &out)
{
    out = TString(args.GetStringArg().c_str());
	args.LogParameter(MakeQuotedString(out.GetString()));
    return args;
}

TArgumentList &FIVEL_NS operator>>(TArgumentList &args, std::string &out)
{
    out = args.GetStringArg();
	args.LogParameter(MakeQuotedString(out));
    return args;
}

TArgumentList &FIVEL_NS operator>>(TArgumentList &args, int16 &out)
{
    int32 temp;
    temp = args.GetInt32Arg();
    if (temp < MIN_INT16 || MAX_INT16 < temp)
		throw TException(__FILE__, __LINE__,
						 "Can't represent value as 16-bit integer");
    out = temp;
	args.LogParameter(TString::IntToString(out).GetString());
    return args;
}

TArgumentList &FIVEL_NS operator>>(TArgumentList &args, int32 &out)
{
    out = args.GetInt32Arg();
	args.LogParameter(TString::IntToString(out).GetString());
    return args;
}

TArgumentList &FIVEL_NS operator>>(TArgumentList &args, bool &out)
{
    out = args.GetBoolArg();
	args.LogParameter(out ? "#t" : "#f");
	return args;
}

TArgumentList &FIVEL_NS operator>>(TArgumentList &args, uint32 &out)
{
    out = args.GetUInt32Arg();
	args.LogParameter(TString::UIntToString(out).GetString());
    return args;
}

TArgumentList &FIVEL_NS operator>>(TArgumentList &args, double &out)
{
    out = args.GetDoubleArg();
	args.LogParameter(TString::DoubleToString(out).GetString());
    return args;
}

TArgumentList &FIVEL_NS operator>>(TArgumentList &args, TRect &out)
{
    out = args.GetRectArg();
	args.LogParameter(std::string("(rect ") +
					  TString::IntToString(out.Left()).GetString() +
					  std::string(" ") +
					  TString::IntToString(out.Top()).GetString() +
					  std::string(" ") +
					  TString::IntToString(out.Right()).GetString() +
					  std::string(" ") +
					  TString::IntToString(out.Bottom()).GetString() +
					  std::string(")"));
    return args;
}

TArgumentList &FIVEL_NS operator>>(TArgumentList &args, TPolygon &out)
{
    out = args.GetPolygonArg();
	std::string logmsg = "(poly";
	for (int i = 0; i < out.GetPointCount(); i++)
	{
		TPoint p = out.GetPoint(i);
		logmsg += (std::string(" ") +
				   TString::IntToString(p.X()).GetString() +
				   std::string(",") +
				   TString::IntToString(p.Y()).GetString());
	}
	logmsg += ")";
	args.LogParameter(logmsg);
    return args;
}

TArgumentList &FIVEL_NS operator>>(TArgumentList &args, TPoint &out)
{
    out = args.GetPointArg();
	args.LogParameter(std::string("(point ") +
					  TString::IntToString(out.X()).GetString() +
					  std::string(" ") +
					  TString::IntToString(out.Y()).GetString() +
					  std::string(")"));
    return args;
}

TArgumentList &FIVEL_NS operator>>(TArgumentList &args,
								   GraphicsTools::Color &out)
{
    out = args.GetColorArg();
	args.LogParameter(std::string("(color ") +
					  TString::IntToString(out.red).GetString() +
					  std::string(" ") +
					  TString::IntToString(out.green).GetString() +
					  std::string(" ") +
					  TString::IntToString(out.blue).GetString() +
					  std::string(" ") +
					  TString::IntToString(out.alpha).GetString() +
					  std::string(")"));
    return args;
}

TArgumentList &FIVEL_NS operator>>(TArgumentList &args, TCallbackPtr &out)
{
    out = args.GetCallbackArg();
	args.LogParameter(out->PrintableRepresentation());
    return args;
}

TArgumentList &FIVEL_NS operator>>(TArgumentList &args, TArgumentList* &out)
{
	out = args.GetListArg();
	args.LogParameter("#<list>");
	return args;
}


//=========================================================================
//  SymbolName Methods
//=========================================================================

TArgumentList &FIVEL_NS operator>>(TArgumentList &inArgs,
								   const SymbolName &inSymbolName)
{
	std::string name = inArgs.GetSymbolArg();
	inArgs.LogParameter(std::string("'") + name);
	inSymbolName.mName = name;
	return inArgs;
}


//=========================================================================
//  ValueOrPercent Methods
//=========================================================================

TArgumentList &FIVEL_NS operator>>(TArgumentList &inArgs,
								   const ValueOrPercent &inVoP)
{
	// Fetch the value.
	bool is_percent;
	int32 value;
	inArgs.GetValueOrPercentArg(is_percent, value);

	// Interpret it.
	if (is_percent)
	{
		double result = (inVoP.mBaseValue * value) / 100.0;
		if (result < 0)
			result -= 0.5;
		else
			result += 0.5;
		*inVoP.mOutputValue = static_cast<int>(result);
		inArgs.LogParameter(std::string("(percent ") +
							TString::IntToString(value).GetString() +
							std::string(")"));
	}
	else
	{
		*inVoP.mOutputValue = value;
		inArgs.LogParameter(TString::IntToString(value).GetString());
	}
	return inArgs;
}


//=========================================================================
//  TPrimitiveManager Methods
//=========================================================================

void TPrimitiveManager::RegisterPrimitive(const std::string &inName,
										  PrimitiveFunc inFunc)
{
    ASSERT(inName != "");
    ASSERT(inFunc != NULL);

    // Erase any existing primitive with this name.
    std::map<std::string,PrimitiveFunc>::iterator existing =
		mPrimitiveMap.find(inName);
    if (existing != mPrimitiveMap.end())
    {
		gDebugLog.Log("Replacing primitive <%s>", inName.c_str());
		mPrimitiveMap.erase(existing);
    }
    
    // Insert the new entry.
    mPrimitiveMap.insert(std::pair<std::string,PrimitiveFunc>(inName, inFunc));
}

bool TPrimitiveManager::DoesPrimitiveExist(const std::string &inName)
{
    ASSERT(inName != "");

    std::map<std::string,PrimitiveFunc>::iterator found =
		mPrimitiveMap.find(inName);
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
    std::map<std::string,PrimitiveFunc>::iterator found =
		mPrimitiveMap.find(inName);
    if (found == mPrimitiveMap.end())
		throw TException(__FILE__, __LINE__,
						 "Tried to call non-existant primitive");
    PrimitiveFunc primitive = found->second;

	// Ask the TArgumentList to log all the parameters it returns.
	inArgs.BeginLog(inName);

	// Clear the result value and logging flag.
	gVariableManager.MakeNull("_result");
	gVariableManager.MakeNull(FIVEL_SKIP_LOGGING_VAR);
    
    // Call it.
    (*primitive)(inArgs);

	// Extract the logged arguments and write them to the debug log.
	std::string call_info = inArgs.EndLog();
	if (gVariableManager.IsNull(FIVEL_SKIP_LOGGING_VAR))
	{
		if (gVariableManager.IsNull("_result")) {
			gDebugLog.Log(">>> %s", call_info.c_str());
		} else {
			std::ostringstream out;
			out << gVariableManager.Get("_result");
			gDebugLog.Log(">>> %s -> %s",
						  call_info.c_str(), out.str().c_str());
		}
	}
}
