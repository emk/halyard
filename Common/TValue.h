// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#ifndef TValue_H
#define TValue_H

#include <typeinfo>

BEGIN_NAMESPACE_FIVEL

// Forward declarations.
class TValue;
class TCallback;


//=========================================================================
//  TNull
//=========================================================================
//  This class represents a "null" value, which may have different meanings
//  in different contexts.  Included mostly for the sake of completeness
//  and backwards compatibility.

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
//  This class represents a Scheme-style "symbol".  In Scheme, symbols are
//  essentially pre-hashed ("interned") strings.  They can be compared
//  much quicker than strings, and are typically used to represent names
//  or tokens.

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
//  This class represents a percentage value.  We need this in a few places
//  in the API.

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

typedef shared_ptr<TCallback> TCallbackPtr;
extern std::ostream &operator<<(std::ostream &out,
								const TCallbackPtr &inCallback);


//=========================================================================
//  TValue & TValueList
//=========================================================================
//  TValue is a fairly straightforward implementation of dynamic typing
//  in C++.  In Windows terms, TValue is a "variant" type--it can hold a
//  wide range of different data types, including lists.
//
//  TValue is used to interface between C++ and dynamically-typed scripting
//  languages.  It may also be useful for working with COM variants on
//  Windows systems.
//
//  TValue has many constructors and many output conversions; these should
//  be used to access the data types.  For more examples of using TValue,
//  see the test cases in TValue.cpp.
//
//  TValue has "value" semantics--you can copy it, store it in STL
//  containers, and generally treat it as a well-behaved data type.
//  No pointers are necessary.  All TValues are read-only, and use
//  reference counting for efficient copying.
//
//  One warning about TValue: Any class with so many implicit constructors
//  and conversion operators is asking for trouble.  It's convenient to
//  use, but you must be VERY CAREFUL about which constructors and
//  conversion operators you add, or you (and every future user) of
//  TValue will drown in a sea of weird C++ compilation errors.
//
//  If you need to add a new data type, please see the locations marked
//  with "ADDING NEW TYPES", here and in TValue.cpp.
//  

typedef std::vector<TValue> TValueList;

class TValue {
public:
    // ADDING NEW TYPES - You need to add a new item to this
    // enumeration.
    enum Type {
        TYPE_NULL,      // No value.
        TYPE_STRING,    // Regular string.
        TYPE_SYMBOL,    // A symbol, as in Scheme.
        TYPE_LONG,      // A 32-bit signed integer.
        TYPE_ULONG,     // A 32-bit unsigned integer.
        TYPE_DOUBLE,    // A floating point number.
        TYPE_BOOLEAN,   // A boolean value.
        TYPE_POINT,     // A point.
        TYPE_RECT,      // A rectangle, right-bottom exclusive.
        TYPE_COLOR,     // An RGB color.
        TYPE_LIST,      // A list of TValues
        TYPE_POLYGON,   // A TPolygon
        TYPE_CALLBACK,  // A scripting language callback
        TYPE_PERCENT    // A TPercent
    };

    // Magic helper functions for converting C++ types back into runtime
    // type IDs.  We rely on template specialization--we never
    // define a body for the generic version of 'FindType', but we
    // supply a specialized version for each value of T.
    //
    // ADDING NEW TYPES - Add exactly one new entry here, mapping from
    // the C++ type used to store your data to the enumeration value
    // you added above.
    //
    // WARNING - The type you choose to store data MUST HAVE VALUE
    // SEMANTICS.  This means, specifically, the it supports copy-by-value,
    // assignment, and can be stored in an STL container safely.  Pointers
    // which need to be deleted and std::auto_ptr are NOT SAFE.
    //
    // XXX - These are really only public for use by TemplateImpl, due to
    // either (a) bizarre C++ design flaws or (b) bizarre MSVC++ bugs.
    // I'm too lazy to look up which.
    template <typename T> static Type FindType(const T &);
    template <> static Type FindType(const TNull &) { return TYPE_NULL; }
    template <> static Type FindType(const std::string &)
    	{ return TYPE_STRING; }
    template <> static Type FindType(const TSymbol &) { return TYPE_SYMBOL; }
    template <> static Type FindType(const int32 &) { return TYPE_LONG; }
    template <> static Type FindType(const uint32 &) { return TYPE_ULONG; }
    template <> static Type FindType(const double &) { return TYPE_DOUBLE; }
    template <> static Type FindType(const bool &) { return TYPE_BOOLEAN; }
    template <> static Type FindType(const TPoint &) { return TYPE_POINT; }
    template <> static Type FindType(const TRect &) { return TYPE_RECT; }
    template <> static Type FindType(const GraphicsTools::Color &)
    	{ return TYPE_COLOR; }
    template <> static Type FindType(const TValueList &) { return TYPE_LIST; }
    template <> static Type FindType(const TPolygon &) { return TYPE_POLYGON; }
    template <> static Type FindType(const TCallbackPtr &)
    	{ return TYPE_CALLBACK; }
    template <> static Type FindType(const TPercent &) { return TYPE_PERCENT; }

    // Normal casting from a TValue to a TCallbackPtr does not work
    // so need to explicitly extract TCallbackPtr.
	TCallbackPtr GetCallbackPtr();

private:
    //////////
    // Because we want TValue to have "value" semantics, we need to
    // use the "pimpl" idiom, where we maintain a pointer to our
    // implementation.  This class defines the abstract interface
    // to our internal implementation.
    //
    struct Impl {
        virtual ~Impl() {}
        virtual Type GetType() = 0;
        virtual bool Equals(const Impl *inRight) = 0;
        virtual void Write(std::ostream &out) = 0;
    };
    
    //////////
    // This template generates a whole family all classes, all derived from
    // Impl.  There is one template instantiation for each specialization
    // of FindType above.
    //
    // The type T must have value semantics!  See the note on FindType.
    //
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

        void Write(std::ostream &out) {
            out << mValue;
        }
    };

    typedef shared_ptr<Impl> ImplPtr;

    ImplPtr mPtr;

    //////////
    // Extract a value from the underlying TemplateImpl, performing a type
    // check.  This function has two oddities dictated by MSVC++ 6.0 bugs:
    // (a) it's in the header, where MSVC++ will be able to instantiate it
    // and (b) it takes a gratuitous argument of type T so MSVC++ can
    // determine what type we want to bind to T.
    //
    template <typename T>
    inline const T &Get(T &outVal) const {
        const TemplateImpl<T> *impl =
            dynamic_cast<const TemplateImpl<T> *>(mPtr.get());
        if (!impl)
            THROW("Type mismatch fetching TValue");
        return impl->mValue;
    }

public:
    // Create an uninitialized TValue.
    TValue() {}

    // Create a TValue from common datatypes.
    //
    // ADDING NEW TYPES - You should add one or more constructors here
    // for each new type.  It's relatively safe to define convenience
    // constructors.
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

    // Convert a TValue to common data types.
    //
    // ADDING NEW TYPES - You should add one conversion operator for
    // each new type you add.  Adding the wrong operators here may
    // trigger lots of C++ errors; be careful and test thoroughly.
    operator TNull() const;
    operator std::string() const;
    operator TSymbol() const;
    operator int32() const;
    operator uint32() const;
    operator double() const;
    operator bool() const;
    operator TPoint() const;
    operator TRect() const;
    operator GraphicsTools::Color() const;
    operator const TValueList &() const;
    operator TPolygon() const;
    operator TPercent() const;

    //////////
    // Has this TValue been initialized?
    //
    bool IsInitialized() const { return mPtr.get() != NULL; }

    ////////
    // Get the type of this TValue.  Only valid if the value is
    // initialized.
    //
    Type GetType() const;

    friend bool operator==(const TValue &inV1, const TValue &inV2);
    friend std::ostream &operator<<(std::ostream &out, const TValue &inV);
};

//////////
// Two TValues are equal if and only if they are of the same type and
// the underlying values are ==.
//
extern bool operator==(const TValue &inV1, const TValue &inV2);

extern bool operator!=(const TValue &inV1, const TValue &inV2);
extern std::ostream &operator<<(std::ostream &out, const TValue &inV);

extern std::ostream &operator<<(std::ostream &out, const TValueList &l);

END_NAMESPACE_FIVEL

#endif // TValue_H
