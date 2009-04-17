// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
// @BEGIN_LICENSE
//
// Halyard - Multimedia authoring and playback system
// Copyright 1993-2009 Trustees of Dartmouth College
// 
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation; either version 2
// of the License, or (at your option) any later version.
// 
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
//
// @END_LICENSE

#include "CommonHeaders.h"
#include "TPrimitives.h"

using namespace Halyard;

TPrimitiveManager Halyard::gPrimitiveManager;


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

void TArgumentList::LogTValueParameter(const TValue &inParameterValue) {
	std::ostringstream out;
	out << inParameterValue;

	LogParameter(out.str());
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
	return tvalue_cast<std::string>(arg);
}

std::string TArgumentList::GetSymbolArg() {
	TValue arg = GetNextArg(); 
	return tvalue_cast<TSymbol>(arg).GetName();
}

int32 TArgumentList::GetInt32Arg() {
	TValue arg = GetNextArg(); 
	return tvalue_cast<int32>(arg);
}

uint32 TArgumentList::GetUInt32Arg() {	
    TValue arg = GetNextArg(); 
	return tvalue_cast<uint32>(arg);
}

bool TArgumentList::GetBoolArg() {
	TValue arg = GetNextArg(); 
	return tvalue_cast<bool>(arg);
}

double TArgumentList::GetDoubleArg() {	
	TValue arg = GetNextArg(); 
	return tvalue_cast<double>(arg);
}

TPoint TArgumentList::GetPointArg() {
	TValue arg = GetNextArg(); 
	return tvalue_cast<TPoint>(arg);
}

TRect TArgumentList::GetRectArg() {
	TValue arg = GetNextArg(); 
	return tvalue_cast<TRect>(arg);
}

TPolygon TArgumentList::GetPolygonArg() {
	TValue arg = GetNextArg(); 
	return tvalue_cast<TPolygon>(arg);
}

GraphicsTools::Color TArgumentList::GetColorArg() {
	TValue arg = GetNextArg(); 
	return tvalue_cast<GraphicsTools::Color>(arg);
}

void TArgumentList::GetValueOrPercentArg(bool &outIsPercent,
										 int32 &outValue)
{
	TValue arg = GetNextArg(); 

	if(arg.GetType() != TValue::TYPE_PERCENT) {
		outIsPercent = false;
		outValue = tvalue_cast<int32>(arg);
	} else {
		outIsPercent = true;
		outValue = tvalue_cast<TPercent>(arg).GetValue();
	}
}

TCallbackPtr TArgumentList::GetCallbackArg() {
	TValue arg = GetNextArg();
	return arg.GetCallbackPtr();
}

TArgumentList::TArgumentList(TValueList inVal) {
	mArgList = TValueList(inVal);
	mArgPtr = mArgList.begin();
}

TArgumentList &Halyard::operator>>(TArgumentList &args, std::string &out)
{
    out = args.GetStringArg();
	args.LogTValueParameter(out);
    return args;
}

TArgumentList &Halyard::operator>>(TArgumentList &args, int16 &out)
{
    int32 temp;
    temp = args.GetInt32Arg();
    if (temp < MIN_INT16 || MAX_INT16 < temp)
		throw TException(__FILE__, __LINE__,
						 "Can't represent value as 16-bit integer");
    out = static_cast<short>(temp);
	args.LogTValueParameter(out);
    return args;
}

TArgumentList &Halyard::operator>>(TArgumentList &args, int32 &out)
{
    out = args.GetInt32Arg();
	args.LogTValueParameter(out);
	return args;
}

TArgumentList &Halyard::operator>>(TArgumentList &args, bool &out)
{
    out = args.GetBoolArg();
	args.LogTValueParameter(out);
	return args;
}

TArgumentList &Halyard::operator>>(TArgumentList &args, uint32 &out)
{
    out = args.GetUInt32Arg();
	args.LogTValueParameter(out);
    return args;
}

TArgumentList &Halyard::operator>>(TArgumentList &args, double &out)
{
    out = args.GetDoubleArg();
	args.LogTValueParameter(out);
    return args;
}

TArgumentList &Halyard::operator>>(TArgumentList &args, TRect &out)
{
    out = args.GetRectArg();
	args.LogTValueParameter(out);
    return args;
}

TArgumentList &Halyard::operator>>(TArgumentList &args, TPolygon &out)
{
    out = args.GetPolygonArg();
	args.LogTValueParameter(out);
    return args;
}

TArgumentList &Halyard::operator>>(TArgumentList &args, TPoint &out)
{
    out = args.GetPointArg();
	args.LogTValueParameter(out);
    return args;
}

TArgumentList &Halyard::operator>>(TArgumentList &args,
								   GraphicsTools::Color &out)
{
    out = args.GetColorArg();
	args.LogTValueParameter(out);
    return args;
}

TArgumentList &Halyard::operator>>(TArgumentList &args, TCallbackPtr &out)
{
    out = args.GetCallbackArg();
	args.LogTValueParameter(out);
    return args;
}

TArgumentList &Halyard::operator>>(TArgumentList &args, TValue &out) {
	out = args.GetNextArg();
	args.LogTValueParameter(out);
	return args;
}


//=========================================================================
//  SymbolName Methods
//=========================================================================

TArgumentList &Halyard::operator>>(TArgumentList &inArgs,
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

TArgumentList &Halyard::operator>>(TArgumentList &inArgs,
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
		inArgs.LogTValueParameter(TPercent(result));
	}
	else
	{
		*inVoP.mOutputValue = value;
		inArgs.LogTValueParameter(value);
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
		gLog.Debug("halyard", "Replacing primitive <%s>", inName.c_str());
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
    
    // Build the log category name for this primitive.
    std::string log_category("halyard.prim." + inName);

    // Find the primitive.
    std::map<std::string,PrimitiveFunc>::iterator found =
		mPrimitiveMap.find(inName);
    if (found == mPrimitiveMap.end())
		throw TException(__FILE__, __LINE__,
						 ("Unknown primitive: " + inName).c_str());
    PrimitiveFunc primitive = found->second;

	// Ask the TArgumentList to log all the parameters it returns.
	inArgs.BeginLog(inName);

	// Clear the result value and logging flag.
	gVariableManager.MakeNull("_result");
	gVariableManager.MakeNull(HALYARD_SKIP_LOGGING_VAR);
    
    // Call it.
    (*primitive)(inArgs);

	// Extract the logged arguments and write them to the debug log.
	std::string call_info = inArgs.EndLog();
	if (gVariableManager.IsNull(HALYARD_SKIP_LOGGING_VAR))
	{
		if (gVariableManager.IsNull("_result")) {
			gLog.Trace(log_category, ">>> %s", call_info.c_str());
		} else {
			std::ostringstream out;
			out << gVariableManager.Get("_result");
			gLog.Trace(log_category, ">>> %s -> %s",
                       call_info.c_str(), out.str().c_str());
		}
	}

    // Make sure all our arguments were used.
    if (inArgs.HasMoreArguments())
        throw TException(__FILE__, __LINE__, "Too many arguments");
}
