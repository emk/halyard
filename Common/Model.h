// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
// @BEGIN_LICENSE
//
// Tamale - Multimedia authoring and playback system
// Copyright 1993-2006 Trustees of Dartmouth College
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

#if !defined (Model_H)
#define Model_H

#include "XmlUtils.h"


//////////
/// A lightweight, persistent object database with XML serialization.
///
namespace model {

	// Forward declarations.
	class Datum;
	class MutableDatum;
	class Object;
	class View;
	class Change;
	class Model;

	/// Internal support code.
	namespace Private {
		/// A std::map wrapper for Datum *.
        /// \obsolete - Used a standard map with shared_ptr<Datum>.
        class DatumMap : public std::map<std::string,Datum *> {
		public:
			/// Becase std::map does not have a virtual destructor, it is
			/// not safe to cast this class to std::map.
			~DatumMap();
		};

		/// A std::vector wrapper for Datum *.
        /// \obsolete - Used a standard map with shared_ptr<Datum>.
		class DatumVector : public std::vector<Datum *> {
		public:
			/// Becase std::list does not have a virtual destructor, it is
			/// not safe to cast this class to std::list.
			~DatumVector();
		};
	};

	//////////
	/// The type of the data represented by a Datum.
	///
	enum Type {
		StringType,
		IntegerType,
		ListType,
		MapType,
		ObjectType
	};
	
	//////////
	/// Class information about subclasses of model::Object. 
	///
	class Class {
	public:
		typedef Object *(*CreatorFunction)();

	private:
		typedef std::map<std::string,Class*> ClassMap;

		static ClassMap *sClasses;

		std::string mName;
		CreatorFunction mCreator;
		
	public:
		Class(const std::string &inName, CreatorFunction inCreator);

		const std::string &GetName() const { return mName; }

		Object *CreateInstance() const;
		static Class *FindByName(const std::string &inClass);
	};

	//////////
	/// The abstract superclass of all data in the Model.
	///
	class Datum {
		DISABLE_COPY_AND_ASSIGN(Datum);
		Type mType;

	protected:
		Datum(Type inType) : mType(inType) {}

		static Datum *CreateFromXML(FIVEL_NS xml_node inNode);

	public:
		virtual ~Datum() {}

		//////////
		/// Immediately after a Datum is created, it should be registered
		/// with a Model object.  You can do this by inserting the Datum
		/// into a container, which will call RegisterWithModel.  The
		/// inParent argument will be NULL for the top-most item in
		/// the container.
		///
		virtual void RegisterWithModel(Model *inModel,
									   MutableDatum *inParent) {}

		//////////
		/// Fill in any children of the datum using the specified XML node.
		///
		virtual void Fill(FIVEL_NS xml_node inNode) {}

		//////////
		/// Write a Datum to the specified XML node.
		///
		virtual void Write(FIVEL_NS xml_node inContainer) = 0;

	protected:
		friend class Change;
		friend class HashDatum;
		friend class List;

		//////////
		/// Called when this datum is deleted, either by regular container
		/// operations or Undo/Redo.
		///
		virtual void NotifyDeleted() {}

		//////////
		/// Called when this datum is undeleted by Undo/Redo.
		///
		virtual void NotifyUndeleted() {}
	};

	//////////
	/// Verify that inDatum is of type T.  If it isn't, throw an error.
	///
	template <typename T>
	T *cast(Datum *inDatum)
	{	
		T *datum = dynamic_cast<T*>(inDatum);
		if (!datum)
			throw FIVEL_NS TException(__FILE__, __LINE__,
									  "Wrong data type in model::TypeCheck");
		return datum;
	}

	//////////
	/// A ValueDatum is a simple, immutable datum which holds a basic
	/// C++ data type.  You can't change the value in a value datum--you
	/// can only replace it within its container.  This simplifies the
	/// Undo/Redo logic.
	///
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

	// Declare a value datum class.  Undeclared at end of file.
#	define DEFINE_VALUE_DATUM_CLASS(NAME,TYPECODE,DATATYPE) \
		class NAME : public ValueDatum<DATATYPE> { \
		public: \
			NAME(DATATYPE inValue) \
				: ValueDatum<DATATYPE>(TYPECODE, inValue) {} \
			virtual void Write(FIVEL_NS xml_node inContainer); \
		}

	// Declare a collection accessor.  Undeclared at end of file.
	// This should have been a template function, but MSVC 6 didn't like it.
#	define DEFINE_VALUE_SET(OP,TYPE) \
		void OP ## TYPE(ConstKeyType &inKey, TYPE::ConstValueType &inVal) \
		{ OP(inKey, new TYPE(inVal)); }

	// Declare a collection accessor.  Undeclared at end of file.
	// This should have been a template function, but MSVC 6 didn't like it.
#	define DEFINE_VALUE_GET(OP,TYPE) \
		TYPE::ConstValueType OP ## TYPE(ConstKeyType &inKey) \
		{ return cast<TYPE>(OP(inKey))->Value(); }
	
	DEFINE_VALUE_DATUM_CLASS(Integer,IntegerType,long);
	DEFINE_VALUE_DATUM_CLASS(String,StringType,std::string);

	//////////
	/// A MutableDatum can be changed and must therefore support Undo.
	/// 
	class MutableDatum : public Datum {
		friend class Change;

		Model *mModel;
		MutableDatum *mParent;

	protected:
		MutableDatum(Type inType);

		//////////
		/// Called by Change to let the datum know a change has occurred.
		///
		virtual void NotifyChanged();

		//////////
		/// To change this datum, instantiate an appropriate Change
		/// object, and pass it to this method.
		///
		void ApplyChange(Change *inChange);

		//////////
		/// Register a child object with our model.
		///
		/// NOTE - This should really be a method on ContainDatum, but
		/// that class is a template class.
		///
		void RegisterChildWithModel(Datum *inDatum);

	public:
		//////////
		/// Mutable objects actually need to know about their Model and
		/// parent, and communicate with them on a regular basis.
		/// Therefore, we actually pay attention to this method.
		///
		virtual void RegisterWithModel(Model *inModel, MutableDatum *inParent);

		Model *GetModel() { ASSERT(mModel); return mModel; }
	};		

	//////////
	/// The parent class of all Datum objects which contain other Datum
	/// objects.  This class is heavily templated to support different
	/// key and value types.
	///
	template <typename T>
	class CollectionDatum : public MutableDatum {
		friend class Change;

	public:
		// TODO - Use type traits to get an efficient reference type.
		typedef T KeyType;
		typedef const T ConstKeyType;

		bool HaveKey(ConstKeyType &inKey) { return DoFind(inKey) != NULL; }

		template <typename D>
		D *Set(ConstKeyType &inKey, D *inValue)
		{ PerformSet(inKey, static_cast<Datum*>(inValue)); return inValue; }

		DEFINE_VALUE_SET(Set, Integer)
		DEFINE_VALUE_SET(Set, String)

		// This used to be a template function with a built-in call to
		// model::cast, but that broke MSVC 6. 
		Datum *Get(ConstKeyType &inKey) { return DoGet(inKey); }

		DEFINE_VALUE_GET(Get, Integer)
		DEFINE_VALUE_GET(Get, String)

		void Delete(ConstKeyType &inKey) { PerformDelete(inKey); }

	protected:
		CollectionDatum(Type inType)
			: MutableDatum(inType) {}

		//////////
		/// Return the datum associated with the specified key.
		/// MSVC 6 - Not pure virtual to work around template bug.
		///
		virtual Datum *DoGet(ConstKeyType &inKey)
			{ ASSERT(false); return NULL; }

		//////////
		/// Search the collection for the specified key.  If the key
		/// exists, return a pointer to the associated Datum.  If the
		/// key does not exist, return NULL.
		///
		/// This is part of the low-level editing API called by various
		/// subclasses of Change.
		/// MSVC 6 - Not pure virtual to work around template bug.
		///
		virtual Datum *DoFind(ConstKeyType &inKey) { return NULL; }

		//////////
		/// Remove the specified key/datum pair from the collection.  The
		/// key/datum *must* exist--if not, trigger an assertion.  Do not
		/// delete the Datum; it will become the responsibility of the
		/// caller.
		///
		/// This is part of the low-level editing API called by various
		/// subclasses of Change.
		/// MSVC 6 - Not pure virtual to work around template bug.
		///
		virtual void DoRemoveKnown(ConstKeyType &inKey, Datum *inDatum) {}

		//////////
		/// Insert the specified datum into the collection with the
		/// specified key.  This operation never destroys any existing
		/// values--for map-type classes, any existing key has been
		/// removed with DoRemoveKnown; for list-type classes, existing
		/// values should be moved forward.
		///
		/// This is part of the low-level editing API called by various
		/// subclasses of Change.
		/// MSVC 6 - Not pure virtual to work around template bug.
		///
		virtual void DoInsert(ConstKeyType &inKey, Datum *inDatum) {}

	private:
		void PerformSet(ConstKeyType &inKey, Datum *inValue);
		void PerformDelete(ConstKeyType &inKey);
	};

	//////////
	/// A basic hash-table-style datum.  This is an abstract class used
	/// to implement Map and Object.
	///
	class HashDatum : public CollectionDatum<std::string> {
		Private::DatumMap mMap;

	protected:
		HashDatum(Type inType) : CollectionDatum<std::string>(inType) {}

		Private::DatumMap::iterator begin() { return mMap.begin(); }
		Private::DatumMap::iterator end() { return mMap.end(); }

		virtual void NotifyDeleted();
		virtual void NotifyUndeleted();

		virtual Datum *DoGet(ConstKeyType &inKey);
		virtual Datum *DoFind(ConstKeyType &inKey);
		virtual void DoRemoveKnown(ConstKeyType &inKey, Datum *inDatum);
		virtual void DoInsert(ConstKeyType &inKey, Datum *inDatum);
	};

	//////////
	/// A hash table.
	///
	class Map : public HashDatum {
	public:
		Map() : HashDatum(MapType) {}

		virtual void Write(FIVEL_NS xml_node inContainer);
		virtual void Fill(FIVEL_NS xml_node inNode);
	};

	//////////
	/// An object.
	///
	class Object : public HashDatum {
		const Class *mClass;
		std::vector<View*> mViews;

	protected:
		Object(const Class *inClass)
			: HashDatum(ObjectType), mClass(inClass) {}

		virtual void NotifyChanged();
		virtual void NotifyDeleted();
		virtual void NotifyUndeleted();

        //////////
        /// Add any new fields to this object, if they don't already exist.
        ///
        virtual void Migrate() {}

	public:
		~Object();

		const Class *GetClass() { return mClass; }

		//////////
		/// After creating a new object and adding it to its
		/// parent container, you should immediately call
		/// Initialize.
		///
		/// XXX - This is a wart designed to work around the fact
		/// that calling Set(...) in a constructor is broken.
		///
		virtual void Initialize();

		virtual void Write(FIVEL_NS xml_node inContainer);
		virtual void Fill(FIVEL_NS xml_node inNode);

		void RegisterView(View *inView);
		void UnregisterView(View *inView);
	};

	//////////
	/// A simle list class, which supports inserting items (anywhere),
	/// deleting items (from anywhere), and setting items.
	///
	class List : public CollectionDatum<size_t> {
		friend class Change;

		Private::DatumVector mVector;

		void PerformInsert(ConstKeyType &inKey, Datum *inValue);
		
	public:
		List() : CollectionDatum<size_t>(ListType) {}

		virtual void Write(FIVEL_NS xml_node inContainer);
		virtual void Fill(FIVEL_NS xml_node inNode);

		size_t GetSize() { return mVector.size(); }

		template <class D>
		D* Insert(ConstKeyType &inKey, D *inValue)
		{ PerformInsert(inKey, static_cast<Datum*>(inValue)); return inValue; }

		DEFINE_VALUE_SET(Insert, Integer)
		DEFINE_VALUE_SET(Insert, String)

		template <class D>
		D* Append(D *inValue)
		{
			PerformInsert(GetSize(), static_cast<Datum*>(inValue));
			return inValue;
		}

	protected:
		virtual void NotifyDeleted();
		virtual void NotifyUndeleted();

		virtual Datum *DoGet(ConstKeyType &inKey);
		virtual Datum *DoFind(ConstKeyType &inKey);
		virtual void DoRemoveKnown(ConstKeyType &inKey, Datum *inDatum);
		virtual void DoInsert(ConstKeyType &inKey, Datum *inDatum);
	};

#if 0
	//////////
	/// Move an item within a collection or between collections.
	///
	template <typename C1, typename C2>
	extern void Move(C1 *inDest, typename C1::ConstKeyType &inDestKey,
					 C2 *inSrc, typename C2::ConstKeyType &inSrcKey);
#endif // 0

	//////////
	/// We support different data model formats.  Each format has three
	/// properties:
	///
	///   name: The name of this format.
	///   version: The version of this format.
	///   compatible back to: The earliest version of this format with
	///     which the current version maintains backwards compatibility.
	///
	class ModelFormat {
	public:
		typedef unsigned long Version;

	private:
		std::string mName;
		Version mVersion;
		Version mCompatibleBackTo;

	public:
		ModelFormat(const std::string &inName, Version inVersion,
					Version inCompatibleBackTo)
			: mName(inName), mVersion(inVersion),
			  mCompatibleBackTo(inCompatibleBackTo) {}

		std::string GetName() const { return mName; }
		Version GetVersion() const { return mVersion; }
		Version GetCompatibleBackTo() const { return mCompatibleBackTo; }
	};

	//////////
	/// The Model itself.  This class manages a single persistent
	/// object tree.
	///
	class Model {
	public:
		typedef unsigned long Version;

	private:
		typedef std::list<Change*> ChangeList;

		ModelFormat mFormat;
		std::auto_ptr<Object> mRoot;

		ChangeList mChanges;
		ChangeList::iterator mChangePosition;
		
		bool mIsDirty;
		std::string mSavePath;

		void Initialize();

	public:
		Model(const ModelFormat &inFormat);
		Model(const ModelFormat &inCurrentFormat,
			  ModelFormat::Version inEarliestFormat,
			  const std::string &inPath);
		virtual ~Model();
		
		const ModelFormat &GetFormat() { return mFormat; }

		bool CanUndo();
		void Undo();
		void ClearUndoList();

		bool CanRedo();
		void Redo();
		void ClearRedoList();
		
		Object *GetRoot() { ASSERT(mRoot.get()); return mRoot.get(); }
		const Object *GetRoot() const
			{ ASSERT(mRoot.get()); return mRoot.get(); }

		void SaveAs(const std::string &inFile);
		void Save();
		std::string GetSavePath() { return mSavePath; }

		bool IsDirty() { return mIsDirty; }
		void ClearDirty() { mIsDirty = false; }

	private:
		friend class MutableDatum;

		void ApplyChange(Change *inChange);
	};

#	undef DEFINE_VALUE_GET
#	undef DEFINE_VALUE_SET
#	undef DEFINE_VALUE_DATUM_CLASS

};

#define DECLARE_MODEL_CLASS(NAME) \
	static model::Class class_info; \
	NAME() : Object(&class_info) {} \
	static model::Object *create_object()

#define IMPLEMENT_MODEL_CLASS(NAME) \
	model::Object *NAME::create_object() { return new NAME(); } \
	model::Class NAME::class_info(#NAME, &NAME::create_object)

// We need the BEGIN_MODEL_CLASSES, REGISTER_MODEL_CLASS and
// END_MODEL_CLASSES to work around a bug which causes the MSVC
// linker to discard static variables with non-trivial destructors
// if nothing refers to them.  Yes, this is massively stupid.
#define BEGIN_MODEL_CLASSES() \
    model::Class *gModelClasses[] = {

#define REGISTER_MODEL_CLASS(NAME) \
    &NAME::class_info,

#define END_MODEL_CLASSES() \
	NULL };

#endif // Model_H
