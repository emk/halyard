// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#if !defined (DataStore_H)
#define DataStore_H

#include <memory>
#include <map>
#include <string>
#include <vector>

#include "FileSystem.h"
#include "XmlUtils.h"


//////////
// A lightweight, persistent object database with XML serialization.
//
namespace DataStore {

	// Forward declarations.
	class Datum;
	class Store;

	// Internal support code.
	namespace Private {
		class DatumMap : public std::map<std::string,Datum *> {
		public:
			// Becase std::map does not have a virtual destructor, it is
			// not safe to cast this class to std::map.
			~DatumMap();
		};

		class DatumVector : public std::vector<Datum *> {
		public:
			// Becase std::list does not have a virtual destructor, it is
			// not safe to cast this class to std::list.
			~DatumVector();
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
		// If an error occurs in the constructor, free any resources
		// allocated by the object, call this function, and throw an
		// exception.
		//
		void ConstructorFailing() { mIsFreed = true; }

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
	// The abstract superclass of all data in the DataStore.
	//
	class Datum {
		DISABLE_COPY_AND_ASSIGN(Datum);
		Type mType;

	protected:
		Datum(Type inType) : mType(inType) {}

		static Datum *CreateFromXML(xml_node inNode);

	public:
		virtual ~Datum() {}

		//////////
		// Immediately after a Datum is created, it should be registered
		// with a Store object.  You can do this by inserting the Datum
		// into a container, which will call RegisterWithStore.
		//
		virtual void RegisterWithStore(Store *inStore) {}

		//////////
		// Fill in any children of the datum using the specified XML node.
		//
		virtual void Fill(xml_node inNode) {}

		//////////
		// Write a Datum to the specified file.
		//
		virtual void Write(xml_node inParent) = 0;
	};

	//////////
	// Verify that inDatum is of type T.  If it isn't, throw an error.
	//
	template <typename T>
	T *TypeCheck(Datum *inDatum)
	{	
		T *datum = dynamic_cast<T*>(inDatum);
		if (!datum)
			throw TException(__FILE__, __LINE__,
							 "Wrong data type in DataStore::TypeCheck");
		return datum;
	}

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

	protected:
		ValueType mValue;
	};

#	define DEFINE_VALUE_DATUM_CLASS(NAME,TYPECODE,DATATYPE) \
		class NAME : public ValueDatum<DATATYPE> { \
		public: \
			NAME(DATATYPE inValue) \
				: ValueDatum<DATATYPE>(TYPECODE, inValue) {} \
			virtual void Write(xml_node inParent); \
		}
	
	DEFINE_VALUE_DATUM_CLASS(IntegerDatum,IntegerType,long);
	DEFINE_VALUE_DATUM_CLASS(StringDatum,StringType,std::string);


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

		Store *GetStore() { ASSERT(mStore); return mStore; }
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

		template <typename D>
		void Set(ConstKeyType &inKey, D *inValue)
		{ PerformSet(inKey, static_cast<Datum*>(inValue)); }

		template <typename VD>
		void SetValue(ConstKeyType &inKey, typename VD::ConstValueType &inVal)
		{ Set<VD>(inKey, new VD(inVal)); }
		
		template <typename D>
		D *Get(ConstKeyType &inKey)
		{ return TypeCheck<D>(DoGet(inKey)); }

		template <typename VD>
		typename VD::ConstValueType GetValue(ConstKeyType &inKey)
		{ return Get<VD>(inKey)->Value(); }	

		void Delete(ConstKeyType &inKey) { PerformDelete(inKey); }

	protected:
		CollectionDatum(Type inType)
			: MutableDatum(inType) {}

		//////////
		// Return the datum associated with the specified key.
		//
		virtual Datum *DoGet(ConstKeyType &inKey) = 0;

		//////////
		// Search the collection for the specified key.  If the key
		// exists, return a pointer to the associated Datum.  If the
		// key does not exist, return NULL.
		//
		// This is part of the low-level editing API called by various
		// subclasses of Change.
		//
		virtual Datum *DoFind(ConstKeyType &inKey) = 0;

		//////////
		// Remove the specified key/datum pair from the collection.  The
		// key/datum *must* exist--if not, trigger an assertion.  Do not
		// delete the Datum; it will become the responsibility of the
		// caller.
		//
		// This is part of the low-level editing API called by various
		// subclasses of Change.
		//
		virtual void DoRemoveKnown(ConstKeyType &inKey, Datum *inDatum) = 0;

		//////////
		// Insert the specified datum into the collection with the
		// specified key.  This operation never destroys any existing
		// values--for map-type classes, any existing key has been
		// removed with DoRemoveKnown; for list-type classes, existing
		// values should be moved forward.
		//
		// This is part of the low-level editing API called by various
		// subclasses of Change.
		//
		virtual void DoInsert(ConstKeyType &inKey, Datum *inDatum) = 0;

	private:
		class SetChange;
		class DeleteChange;
		void PerformSet(ConstKeyType &inKey, Datum *inValue);
		void PerformDelete(ConstKeyType &inKey);
	};

	//////////
	// A basic hash-table-style datum.
	//
	class MapDatum : public CollectionDatum<std::string> {
		Private::DatumMap mMap;

	public:
		MapDatum() : CollectionDatum<std::string>(MapType) {}

		virtual void Write(xml_node inParent);
		void Fill(xml_node inNode);

	protected:
		virtual Datum *DoGet(ConstKeyType &inKey);
		virtual Datum *DoFind(ConstKeyType &inKey);
		virtual void DoRemoveKnown(ConstKeyType &inKey, Datum *inDatum);
		virtual void DoInsert(ConstKeyType &inKey, Datum *inDatum);
	};

	//////////
	// A simle list class, which supports inserting items (anywhere),
	// deleting items (from anywhere), and setting items.
	//
	class ListDatum : public CollectionDatum<size_t> {
		Private::DatumVector mVector;

		class InsertChange;
		void PerformInsert(ConstKeyType &inKey, Datum *inValue);
		
	public:
		ListDatum() : CollectionDatum<size_t>(ListType) {}

		virtual void Write(xml_node inParent);
		void Fill(xml_node inNode);

		template <class D>
		void Insert(ConstKeyType &inKey, D *inValue)
		{ PerformInsert(inKey, static_cast<Datum*>(inValue)); }

		template <typename VD>
		void InsertValue(ConstKeyType &inKey, typename VD::ConstValueType &inV)
		{ Insert<VD>(inKey, new VD(inV)); }

	protected:
		virtual Datum *DoGet(ConstKeyType &inKey);
		virtual Datum *DoFind(ConstKeyType &inKey);
		virtual void DoRemoveKnown(ConstKeyType &inKey, Datum *inDatum);
		virtual void DoInsert(ConstKeyType &inKey, Datum *inDatum);
	};

#if 0
	//////////
	// Move an item within a collection or between collections.
	//
	template <typename C1, typename C2>
	extern void Move(C1 *inDest, typename C1::ConstKeyType &inDestKey,
					 C2 *inSrc, typename C2::ConstKeyType &inSrcKey);
#endif // 0

	//////////
	// The DataStore itself.  This class manages a single persistent
	// object tree.
	//
	class Store {
		typedef std::list<Change*> ChangeList;

		std::auto_ptr<MapDatum> mRoot;

		ChangeList mChanges;
		ChangeList::iterator mChangePosition;

	public:
		Store();
		~Store();
		
		bool CanUndo();
		void Undo();
		void ClearUndoList();

		bool CanRedo();
		void Redo();
		void ClearRedoList();
		
		MapDatum *GetRoot() { ASSERT(mRoot.get()); return mRoot.get(); }
		const MapDatum *GetRoot() const
			{ ASSERT(mRoot.get()); return mRoot.get(); }

		void Write(const std::string &inFile);

		static Store *Read(const std::string &inFile);
		
	private:
		friend class MutableDatum;

		void ApplyChange(Change *inChange);
	};

	
};

#endif // DataStore_H
