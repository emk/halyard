// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#ifndef TValue_H
#define TValue_H

#include <typeinfo>
#include "TInterpreter.h"

BEGIN_NAMESPACE_FIVEL


//=========================================================================
//  TNull
//=========================================================================

class TNull {
public:
    TNull() {}
};

inline bool operator==(const TNull &inV1, const TNull &inV2) {
    return true;
}


//=========================================================================
//  TSymbol
//=========================================================================

class TSymbol {
    std::string mName;

public:
    TSymbol() {}
    explicit TSymbol(const std::string &inName) : mName(inName) {}
    std::string GetName() const { return mName; }
};

inline bool operator==(const TSymbol &inS1, const TSymbol &inS2) {
    return inS1.GetName() == inS2.GetName();
}

inline bool operator!=(const TSymbol &inS1, const TSymbol &inS2) {
    return !(inS1 == inS2);
}

inline std::ostream &operator<<(std::ostream &out, const TSymbol &inSym) {
    out << "'" << inSym.GetName();
    return out;
}


//=========================================================================
//  TPercent
//=========================================================================

class TPercent {
    double mValue;

public:
    TPercent() : mValue(0.0) {}
    explicit TPercent(double inValue) : mValue(inValue) {}
    double GetValue() const { return mValue; }
};

inline bool operator==(const TPercent &inP1, const TPercent &inP2) {
    return inP1.GetValue() == inP2.GetValue();
}

inline bool operator!=(const TPercent &inP1, const TPercent &inP2) {
    return !(inP1 == inP2);
}

inline std::ostream &operator<<(std::ostream &out, const TPercent &inPercent) {
    out << "'" << inPercent.GetValue();
    return out;
}


//=========================================================================
//  TValue & TValueList
//=========================================================================

class TValue;
typedef std::vector<TValue> TValueList;

class TValue {
public:
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

private:
    struct Impl {
        virtual ~Impl() {}
        virtual Type GetType() = 0;
        virtual bool Equals(const Impl *inRight) = 0;
        virtual void Write(std::ostream &out) = 0;
    };
    
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

    typedef boost::shared_ptr<Impl> ImplPtr;

    ImplPtr mPtr;

    template <typename T>
    inline T &Get(T &outVal) const {
        const TemplateImpl<T> *impl =
            dynamic_cast<const TemplateImpl<T> *>(mPtr.get());
        if (!impl)
            THROW("Type mismatch fetching TValue");
        outVal = impl->mValue;
        return outVal;
    };

public:
    TValue() {}
    TValue(const TNull &inValue) : mPtr(new TemplateImpl<TNull>(TNull())) {}
    TValue(int inValue) : mPtr(new TemplateImpl<int32>(inValue)) {}
    TValue(int32 inValue) : mPtr(new TemplateImpl<int32>(inValue)) {}
    TValue(uint32 inValue) : mPtr(new TemplateImpl<uint32>(inValue)) {}
    TValue(double inValue) : mPtr(new TemplateImpl<double>(inValue)) {}
    TValue(bool inValue) : mPtr(new TemplateImpl<bool>(inValue)) {}
    TValue(const TPoint &inValue) : mPtr(new TemplateImpl<TPoint>(inValue)) {}
    TValue(const TRect &inValue) : mPtr(new TemplateImpl<TRect>(inValue)) {}
    TValue(const GraphicsTools::Color &inValue)
        : mPtr(new TemplateImpl<GraphicsTools::Color>(inValue)) {}
    TValue(const std::string &inValue)
        : mPtr(new TemplateImpl<std::string>(inValue)) {}
    TValue(const TSymbol &inValue)
        : mPtr(new TemplateImpl<TSymbol>(inValue)) {}
    TValue(const TPercent &inValue)
        : mPtr(new TemplateImpl<TPercent>(inValue)) {}
    TValue(const char *inValue)
        : mPtr(new TemplateImpl<std::string>(inValue)) {}
    TValue(const TPolygon &inValue)
        : mPtr(new TemplateImpl<TPolygon>(inValue)) {}
    TValue(const TValueList &inValue)
        : mPtr(new TemplateImpl<TValueList>(inValue)) {}
    TValue(const TCallbackPtr &inValue)
        : mPtr(new TemplateImpl<TCallbackPtr>(inValue)) {}

    inline operator TNull() const { TNull r; return Get(r); }
    inline operator std::string() const { std::string r; return Get(r); }
    inline operator TSymbol() const { TSymbol r; return Get(r); }
    inline operator int32() const { int32 r; return Get(r); }
    inline operator uint32() const { uint32 r; return Get(r); }
    inline operator double() const { double r; return Get(r); }
    inline operator bool() const { bool r; return Get(r); }
    inline operator TPoint() const { TPoint r; return Get(r); }
    inline operator TRect() const { TRect r; return Get(r); }
    inline operator GraphicsTools::Color() const
		{ GraphicsTools::Color r; return Get(r); }
    inline operator TValueList() const { TValueList r; return Get(r); }
    inline operator TPolygon() const { TPolygon r; return Get(r); }
    inline operator TCallbackPtr() const { TCallbackPtr r; return Get(r); }
    inline operator TPercent() const { TPercent r; return Get(r); }

    bool IsInitialized() const { return mPtr.get() != NULL; }
    
    Type GetType() const {
        if (!IsInitialized())
            THROW("Cannot get type of uninitialized TValue");
        return mPtr->GetType();
    }

    friend bool operator==(const TValue &inV1, const TValue &inV2);
    friend std::ostream &operator<<(std::ostream &out, const TValue &inV);
};

inline bool operator==(const TValue &inV1, const TValue &inV2) {
    // Check for uninitialized values.
    if (!inV1.IsInitialized() || !inV2.IsInitialized())
        THROW("Cannot compare uninitialized TValue");
    
    // If the Impl classes aren't the same, the values can't be equal.
    if (typeid(*inV1.mPtr.get()) != typeid(*inV2.mPtr.get()))
        return false;

    // Delegate the comparison to our implementation class.
    return inV1.mPtr->Equals(inV2.mPtr.get());
}

inline bool operator!=(const TValue &inV1, const TValue &inV2) {
    return !(inV1 == inV2);
}

inline std::ostream &operator<<(std::ostream &out, const TValue &inV) {
    if (!inV.IsInitialized())
        out << "#<TValue: uninitialized>";
    else
        inV.mPtr->Write(out);
    return out;
}

END_NAMESPACE_FIVEL

#endif // TValue_H
