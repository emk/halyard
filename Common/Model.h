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
	class Store;
	class Change;

	// Internal support code.
	namespace Private {
		class DatumMap : public std::map<std::string,Datum *> {
		public:
			// Becase std::map does not have a virtual destructor, it is
			// not safe to cast this class to std::map.
			~DatumMap();
			void RemoveKnownDatum(const std::string &inKey, Datum *inDatum);
		};

		class DatumList : public std::list<Datum *> {
		public:
			// Becase std::list does not have a virtual destructor, it is
			// not safe to cast this class to std::list.
			~DatumList();
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

		//////////
		// Immediately after a Datum is created, it should be registered
		// with a Store object.  You can do this by inserting the Datum
		// into a container, which will call RegisterWithStore.
		//
		virtual void RegisterWithStore(Store *inStore) {}
	};

	//////////
	// A ValueDatum is a simple, immutable datum which holds a basic
	// C++ data type.  You can't change the value in a value datum--you
	// can only replace it within its container.  This simplifies the
	// Undo/Redo logic.
	//
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

	//////////
	// A MutableDatum can be changed and must therefore support Undo.
	// 
	class MutableDatum : public Datum {
		Store *mStore;

	protected:
		MutableDatum(Type inType) : Datum(inType), mStore(NULL) { }

		//////////
		// To change this datum, instantiate an appropriate Change
		// object, and pass it to this method.
		//
		void ApplyChange(Change *inChange);

		//////////
		// Register a child object with our store.
		//
		// NOTE - This should really be a method on ContainDatum, but
		// that class is a template class.
		//
		void RegisterChildObjectWithStore(Datum *inDatum);

	public:
		//////////
		// Mutable objects actually need to know about their Store,
		// and communicate with it on a regular basis.  Therefore, we
		// actually pay attention to this method.
		//
		virtual void RegisterWithStore(Store *inStore);
	};		

	//////////
	// The parent class of all Datum objects which contain other Datum
	// objects.  This class is heavily templated to support different
	// key and value types.
	//
	template <typename T>
	class CollectionDatum : public MutableDatum {
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
			: MutableDatum(inType) {}
	};

	//////////
	// A basic hash-table-style datum.
	//
	class MapDatum : public CollectionDatum<std::string> {
		Private::DatumMap mMap;

		class SetChange;

	public:
		MapDatum() : CollectionDatum<std::string>(MapType) {}

		void Set(ConstKeyType &inKey, Datum *inValue);
		Datum *Get(ConstKeyType &inKey);
	};

	//////////
	// A Change represents a mutation of something within the Store.  A
	// Change may be applied, or reverted.  Changes to a given Store occur
	// in a sequence, and must be applied or reverted in that sequence.
	//
	// Because of this careful sequencing, a Change is allowed to hold onto
	// pointers and other resources.  All Change objects are destroyed
	// before the corresponding DataStore object is destroyed.  This means
	// that half their resources will be owned by the DataStore, and half
	// by the Change object, depending on whether the change has been
	// applied or reverted.
	//
	class Change {
		bool mIsApplied;
		bool mIsFreed;
		
	public:
		Change();
		virtual ~Change();
		
		void Apply();
		void Revert();
		void FreeResources();
		
	protected:	
		//////////
		// Apply this change.  This method is called when the change first
		// occurs, and perhaps later on, when the change is redone.  This
		// method must be atomic--if it fails, it must leave the object
		// unchanged and throw an exception.
		//
		virtual void DoApply() = 0;
		
		//////////
		// Revert this change.  This method is called when the change is
		// undone.  This method must be atomic--if it fails, it must leave
		// the object unchanged and throw an exception.
		// 
		virtual void DoRevert() = 0;
		
		//////////
		// Free any resources need to apply this Change, because this
		// Change is being destroyed and will not need to be applied again.
		//
		virtual void DoFreeApplyResources() = 0;

		//////////
		// Free any resources need to revert this Change, because this
		// Change is being destroyed and will not need to be reverted
		// again.
		//
		virtual void DoFreeRevertResources() = 0;
	};

	//////////
	// The DataStore itself.  This class manages a single persistent
	// object tree.
	//
	class Store {
		typedef std::list<Change*> ChangeList;

		std::auto_ptr<MapDatum> mRoot;

		ChangeList mChanges;

	public:
		Store();
		~Store();
		
		bool CanUndo();
		void Undo();

		bool CanRedo() { return false; }
		
		MapDatum *GetRoot() { ASSERT(mRoot.get()); return mRoot.get(); }
		const MapDatum *GetRoot() const
			{ ASSERT(mRoot.get()); return mRoot.get(); }
		
	private:
		friend class MutableDatum;

		void ApplyChange(Change *inChange);
	};

	
};

#endif // DataStore_H
