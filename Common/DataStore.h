// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#if !defined (DataStore_H)
#define DataStore_H

#include <memory>
#include <map>
#include <string>

#include "FileSystem.h"


//////////
// A lightweight, persistent object database with XML serialization.
//
namespace DataStore {

	// Forward declarations.
	class Datum;

	// Internal support code.
	namespace Private {
		class DatumMap : public std::map<std::string,Datum *> {
		public:
			// Becase std::map does not have a virtual destructor, it is
			// not safe to cast this class to std::map.
			~DatumMap();
			void DeleteValue(value_type &inValue);
		};

		class DatumList : public std::list<Datum *> {
		public:
			// Becase std::list does not have a virtual destructor, it is
			// not safe to cast this class to std::list.
			~DatumList();
			void DeleteValue(value_type &inValue);
		};
	};

	//////////
	// The type of the data.
	//
	enum Type {
		StringType,
		IntegerType,
		ListType,
		MapType,
		ObjectType
	};
	
	//////////
	// The abstract superclass of all data in the DataStore.
	//
	class Datum {
		DISABLE_COPY_AND_ASSIGN(Datum);
		Type mType;

	protected:
		Datum(Type inType) : mType(inType) {}

	public:
		virtual ~Datum() {}
	};

	template <typename T>
	class ValueDatum : public Datum {
	public:
		// TODO - Use type traits to get an efficient reference type.
		typedef T ValueType;
		typedef const T ConstValueType;
		ConstValueType &Value() const { return mValue; }

	protected:
		ValueDatum(Type inType, ConstValueType &inValue)
			: Datum(inType), mValue(inValue) {}

	private:
		ValueType mValue;
	};

#	define DEF_VALUE_DATUM_CLASS(NAME,TYPECODE,DATATYPE) \
		class NAME : public ValueDatum<DATATYPE> { \
		public: \
			NAME(DATATYPE inValue) \
				: ValueDatum<DATATYPE>(TYPECODE, inValue) {} \
		}
	
	DEF_VALUE_DATUM_CLASS(IntegerDatum,IntegerType,long);
	DEF_VALUE_DATUM_CLASS(StringDatum,StringType,std::string);

#	undef DEF_VALUE_DATUM

	template <typename T>
	class CollectionDatum : public Datum {
	public:
		// TODO - Use type traits to get an efficient reference type.
		typedef T KeyType;
		typedef const T ConstKeyType;

		virtual void Set(ConstKeyType &inKey, Datum *inValue) = 0;
		virtual Datum *Get(ConstKeyType &inKey) = 0;

		template <typename D>
		void Set(ConstKeyType &inKey, D *inValue)
		{ Set(inKey, static_cast<Datum*>(inValue)); }
			
		template <typename D>
		D *Get(ConstKeyType &inKey)
		{
			D *datum = dynamic_cast<D*>(Get(inKey));
			if (!datum)
				throw TException(__FILE__, __LINE__,
								 "Wrong type in CollectionDatum::Get");
			return datum;
		}

		template <typename VD>
		void SetValue(ConstKeyType &inKey, typename VD::ConstValueType &inVal)
		{ Set<VD>(inKey, new VD(inVal)); }
		
		template <typename VD>
		typename VD::ConstValueType GetValue(ConstKeyType &inKey)
		{ return Get<VD>(inKey)->Value(); }	

	protected:
		CollectionDatum(Type inType)
			: Datum(inType) {}
	};

	//////////
	// A basic hash-table-style datum.
	//
	class MapDatum : public CollectionDatum<std::string> {
		Private::DatumMap mMap;

	public:
		MapDatum() : CollectionDatum<std::string>(MapType) {}

		void Set(ConstKeyType &inKey, Datum *inValue);
		Datum *Get(ConstKeyType &inKey);
	};

	//////////
	// The DataStore itself.  This class manages a single persistent
	// object tree.
	//
	class Store {
		std::auto_ptr<MapDatum> mRoot;

	public:
		Store();

		bool CanUndo() { return false; }
		bool CanRedo() { return false; }

		MapDatum *GetRoot() { ASSERT(mRoot.get()); return mRoot.get(); }
		const MapDatum *GetRoot() const
			{ ASSERT(mRoot.get()); return mRoot.get(); }
	};

	
};

#endif // DataStore_H
