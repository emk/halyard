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

std::string TArgumentList::GetSymbolArg() {
	TValue arg = GetNextArg(); 
	return tvalue_cast<TSymbol>(arg).GetName();
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

/*
template<typename T> 
TArgumentList &operator>>(TArgumentList &args, T &out) {
    TValue arg = args.GetNextArg(); 
	out = tvalue_cast<T>(arg);
    return args;
}
*/

TArgumentList &Halyard::operator>>(TArgumentList &args, TCallbackPtr &out)
{
    out = args.GetCallbackArg();
    return args;
}

TArgumentList &Halyard::operator>>(TArgumentList &args, TValue &out) {
	out = args.GetNextArg();
	return args;
}


//=========================================================================
//  SymbolName Methods
//=========================================================================

TArgumentList &Halyard::operator>>(TArgumentList &inArgs,
								   const SymbolName &inSymbolName)
{
	std::string name = inArgs.GetSymbolArg();
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
	}
	else
	{
		*inVoP.mOutputValue = value;
	}
	return inArgs;
}


//=========================================================================
//  Debugging output
//=========================================================================

std::ostream &Halyard::operator<<(std::ostream &out, TArgumentList &args)
{
    TValueList::iterator arg = args.mArgList.begin();
    if (arg == args.mArgList.end())
        return out;
    out << *(arg++);
    while (arg != args.mArgList.end()) {
        out << " " << *(arg++);
    }
    return out;
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

    // Log the primitive before executing, so we know what was
    // happening if it crashes.
    {
        std::ostringstream out;
        out << ">>> " << inName;
        if (inArgs.HasMoreArguments())
            out << ": " << inArgs;
        gLog.Trace(log_category, "%s", out.str().c_str());
    }

	// Clear the result value and logging flag.
	gVariableManager.MakeNull("_result");
    
    // Call it.
    (*primitive)(inArgs);

    // Log primitive and return value after executing.
    { 
        std::ostringstream out;
        out << "<<< " << inName;
        if (!gVariableManager.IsNull("_result")) {
            out << " -> " << gVariableManager.Get("_result");
        }
        gLog.Trace(log_category, "%s", out.str().c_str());
    }

    // Make sure all our arguments were used.
    if (inArgs.HasMoreArguments())
        throw TException(__FILE__, __LINE__, "Too many arguments");
}
