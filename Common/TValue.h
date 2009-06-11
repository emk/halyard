// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-
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

#ifndef TValue_H
#define TValue_H

#include <typeinfo>

BEGIN_NAMESPACE_HALYARD

// Forward declarations.
class TValue;
class TCallback;


//=========================================================================
//  TNull
//=========================================================================

/// This class represents a "null" value, which may have different meanings
/// in different contexts.  Included mostly for the sake of completeness
/// and backwards compatibility.
///
/// \see TValue
class TNull {
public:
    TNull() {}
};

inline bool operator==(const TNull &inV1, const TNull &inV2) {
    return true;
}

inline std::ostream &operator<<(std::ostream &out, const TNull &inV) {
	out << "#<void>";
	return out;
}


//=========================================================================
//  TSymbol
//=========================================================================

/// This class represents a Scheme-style "symbol".  In Scheme, symbols are
/// essentially pre-hashed ("interned") strings.  They can be compared
/// much quicker than strings, and are typically used to represent names
/// or tokens.
///
/// \see TValue
class TSymbol {
    std::string mName;

public:
    TSymbol() {}
    explicit TSymbol(const std::string &inName) : mName(inName) {}
    std::string GetName() const { return mName; }
};

extern bool operator==(const TSymbol &inS1, const TSymbol &inS2);
extern bool operator!=(const TSymbol &inS1, const TSymbol &inS2);
extern std::ostream &operator<<(std::ostream &out, const TSymbol &inSym);


//=========================================================================
//  TPercent
//=========================================================================

/// This class represents a percentage value.  We need this in a few places
/// in the API.
///
/// \see TValue
class TPercent {
    double mValue;

public:
    TPercent() : mValue(0.0) {}
    explicit TPercent(double inValue) : mValue(inValue) {}
    double GetValue() const { return mValue; }
};

extern bool operator==(const TPercent &inP1, const TPercent &inP2);
extern bool operator!=(const TPercent &inP1, const TPercent &inP2);
extern std::ostream &operator<<(std::ostream &out, const TPercent &inPercent);


//=========================================================================
//  TCallbackPtr
//=========================================================================

/// \see TValue
typedef shared_ptr<TCallback> TCallbackPtr;
extern std::ostream &operator<<(std::ostream &out,
								const TCallbackPtr &inCallback);


//=========================================================================
//  TValue & TValueList
//=========================================================================  

/// A list of TValue objects, which can in turn be stored in a TValue.
typedef std::vector<TValue> TValueList;

/// TValue is a fairly straightforward implementation of dynamic typing
/// in C++.  In Windows terms, TValue is a "variant" type--it can hold a
/// wide range of different data types, including lists.
///
/// TValue is used to interface between C++ and dynamically-typed scripting
/// languages.  It may also be useful for working with COM variants on
/// Windows systems.
///
/// TValue has many constructors and many output conversions; these should
/// be used to access the data types.  For more examples of using TValue,
/// see the test cases in TValue.cpp.
///
/// TValue has "value" semantics--you can copy it, store it in STL
/// containers, and generally treat it as a well-behaved data type.
/// No pointers are necessary.  All TValues are read-only, and use
/// reference counting for efficient copying.
///
/// One warning about TValue: Any class with so many implicit constructors
/// and conversion operators is asking for trouble.  It's convenient to
/// use, but you must be VERY CAREFUL about which constructors and
/// conversion operators you add, or you (and every future user) of
/// TValue will drown in a sea of weird C++ compilation errors.
///
/// If you need to add a new data type, please see the locations marked
/// with "ADDING NEW TYPES", here and in TValue.cpp.
class TValue {
public:
    /// ADDING NEW TYPES - You need to add a new item to this
    /// enumeration.
    enum Type {
        TYPE_NULL,      ///< No value.
        TYPE_STRING,    ///< Regular string.
        TYPE_SYMBOL,    ///< A symbol, as in Scheme.
        TYPE_LONG,      ///< A 32-bit signed integer.
        TYPE_ULONG,     ///< A 32-bit unsigned integer.
        TYPE_DOUBLE,    ///< A floating point number.
        TYPE_BOOLEAN,   ///< A boolean value.
        TYPE_POINT,     ///< A point.
        TYPE_RECT,      ///< A rectangle, right-bottom exclusive.
        TYPE_COLOR,     ///< An RGB color.
        TYPE_LIST,      ///< A list of TValues
        TYPE_POLYGON,   ///< A TPolygon
        TYPE_CALLBACK,  ///< A scripting language callback
        TYPE_PERCENT    ///< A TPercent
    };

    /// Magic helper functions for converting C++ types back into runtime
    /// type IDs.  We rely on template specialization--we never
    /// define a body for the generic version of 'FindType', but we
    /// supply a specialized version for each value of T.
    ///
    /// The actual explicit instantiations appear below, outside the class
    /// body.  Apparently, explicit instantiations must appear at the
    /// top-level of the appropriate namespace.
    ///
    /// XXX - This is really only public for use by TemplateImpl, due to
    /// either (a) bizarre C++ design flaws or (b) bizarre MSVC++ bugs.
    /// I'm too lazy to look up which.
    template <typename T> static Type FindType(const T &);

    /// Normal casting from a TValue to a TCallbackPtr does not work
    /// so need to explicitly extract TCallbackPtr.
	TCallbackPtr GetCallbackPtr();

private:
    //////////
    /// Because we want TValue to have "value" semantics, we need to
    /// use the "pimpl" idiom, where we maintain a pointer to our
    /// implementation.  This class defines the abstract interface
    /// to our internal implementation.
    ///
    struct Impl {
        virtual ~Impl() {}
        virtual Type GetType() = 0;
        virtual bool Equals(const Impl *inRight) = 0;
        virtual void Write(std::ostream &out) = 0;
    };
    
    //////////
    /// This template generates a whole family all classes, all derived from
    /// Impl.  There is one template instantiation for each specialization
    /// of FindType above.
    ///
    /// The type T must have value semantics!  See the note on FindType.
    ///
    template <typename T>
    struct TemplateImpl : public Impl {
        T mValue;

        TemplateImpl(const T &inValue) : mValue(inValue) {}

        Type GetType() {
            return FindType(mValue);
        }

        bool Equals(const Impl *inRight) {
            // We can only be called by operator==, which guarantees
            // that *inRight has the same dynamic type as *this.
            const TemplateImpl<T> *right =
                dynamic_cast<const TemplateImpl<T> *>(inRight);
            ASSERT(right != NULL);
            return mValue == right->mValue;
        }
        
        virtual void Write(std::ostream &out);
    };

    typedef shared_ptr<Impl> ImplPtr;

    ImplPtr mPtr;

    //////////
    /// Extract a value from the underlying TemplateImpl, performing a type
    /// check.  This function has two oddities dictated by MSVC++ 6.0 bugs:
    /// (a) it's in the header, where MSVC++ will be able to instantiate it
    /// and (b) it takes a gratuitous argument of type T so MSVC++ can
    /// determine what type we want to bind to T.
    ///
    template <typename T>
    inline const T &Get(T &outVal, const char *inExpectedTypeName) const {
        const TemplateImpl<T> *impl =
            dynamic_cast<const TemplateImpl<T> *>(mPtr.get());
        if (!impl)
            THROW("Expected " + std::string(inExpectedTypeName) +
                  ", got <" + ToDisplayValue() + ">");
        return impl->mValue;
    }

public:
    /// Create an uninitialized TValue.
    TValue() {}

    /// Create a TValue from common datatypes.
    ///
    /// ADDING NEW TYPES - You should add one or more constructors here
    /// for each new type.  It's relatively safe to define convenience
    /// constructors.
    TValue(const TNull &inValue);
    TValue(int inValue);
    TValue(int32 inValue);
    TValue(uint32 inValue);
    TValue(double inValue);
    TValue(bool inValue);
    TValue(const TPoint &inValue);
    TValue(const TRect &inValue);
    TValue(const GraphicsTools::Color &inValue);
    TValue(const std::string &inValue);
    TValue(const TSymbol &inValue);
    TValue(const TPercent &inValue);
    TValue(const char *inValue);
    TValue(const TPolygon &inValue);
    TValue(const TValueList &inValue);
    TValue(const TCallbackPtr &inValue);

    //////////
    /// Has this TValue been initialized?
    ///
    bool IsInitialized() const { return mPtr.get() != NULL; }

    ////////
    /// Get the type of this TValue.  Only valid if the value is
    /// initialized.
    ///
    Type GetType() const;

    friend bool operator==(const TValue &inV1, const TValue &inV2);
    friend std::ostream &operator<<(std::ostream &out, const TValue &inV);

    /// Convert a TValue to common data types.  We used to use implicit
    /// conversion operators for this, but it was too much hassle to make
    /// them work on multiple C++ compilers.  So we're going with explicit
    /// casts.
    ///
    /// ADDING NEW TYPES - You'll need to manually instantiate this for
    /// each type you can convert a TValue to.  See TValue.cpp for the
    /// manual instantiations.
    template <typename T> friend T tvalue_cast(const TValue &v);

    //////////
    /// Return a string representing this TValue.  This should only be
    /// used for error messages or logging, as the format is not
    /// suitable for parsing.
    ///
    std::string ToDisplayValue() const;
};

// Explicit instantiations of TValue::FindType (above).
// 
// ADDING NEW TYPES - Add exactly one new entry here, mapping from the C++
// type used to store your data to the enumeration value you added above.
//
// WARNING - The type you choose to store data MUST HAVE VALUE SEMANTICS.
// This means, specifically, the it supports copy-by-value, assignment, and
// can be stored in an STL container safely.  Pointers which need to be
// deleted and std::auto_ptr are NOT SAFE.
template <> inline TValue::Type TValue::FindType(const TNull &)
    { return TYPE_NULL; }
template <> inline TValue::Type TValue::FindType(const std::string &)
    { return TYPE_STRING; }
template <> inline TValue::Type TValue::FindType(const TSymbol &)
    { return TYPE_SYMBOL; }
template <> inline TValue::Type TValue::FindType(const int32 &)
    { return TYPE_LONG; }
template <> inline TValue::Type TValue::FindType(const uint32 &)
    { return TYPE_ULONG; }
template <> inline TValue::Type TValue::FindType(const double &)
    { return TYPE_DOUBLE; }
template <> inline TValue::Type TValue::FindType(const bool &)
    { return TYPE_BOOLEAN; }
template <> inline TValue::Type TValue::FindType(const TPoint &)
    { return TYPE_POINT; }
template <> inline TValue::Type TValue::FindType(const TRect &)
    { return TYPE_RECT; }
template <> inline TValue::Type TValue::FindType(const GraphicsTools::Color &)
    { return TYPE_COLOR; }
template <> inline TValue::Type TValue::FindType(const TValueList &)
    { return TYPE_LIST; }
template <> inline TValue::Type TValue::FindType(const TPolygon &)
    { return TYPE_POLYGON; }
template <> inline TValue::Type TValue::FindType(const TCallbackPtr &)
    { return TYPE_CALLBACK; }
template <> inline TValue::Type TValue::FindType(const TPercent &)
    { return TYPE_PERCENT; }

//////////
/// Two TValues are equal if and only if they are of the same type and
/// the underlying values are ==.
///
extern bool operator==(const TValue &inV1, const TValue &inV2);

extern bool operator!=(const TValue &inV1, const TValue &inV2);
extern std::ostream &operator<<(std::ostream &out, const TValue &inV);

extern std::ostream &operator<<(std::ostream &out, const TValueList &l);

END_NAMESPACE_HALYARD

#endif // TValue_H
