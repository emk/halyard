// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#ifndef TStateDB_H
#define TStateDB_H

#include <typeinfo>
#include <algorithm>

BEGIN_NAMESPACE_FIVEL

class TStateDB;

//////////
// Subclasses of TStateListener listener for changes to keys in a TStateDB. 
//
class TStateListener {
	friend class TStateDB;
 
	typedef std::vector<std::string> KeyList; 
	KeyList mKeys; 	
	TStateDB *mDB;
	
	void RecordKey(TStateDB *db, const std::string &inKey); 

public:
	TStateListener() : mDB(NULL) {}
	virtual ~TStateListener(); 
	virtual void NotifyStateChanged() = 0;								
};

//////////
// A TStateDB stores key/value pairs (much like the Windows registry) and
// notifies TStateListeners when those keys change.
//
class TStateDB {     
	friend class TStateListener;
	
	//////////
	// Represents the value of an event and all the Listerners
	// to that event.
	// 
	struct Datum {
		TValue mValue;
		typedef std::vector<TStateListener *>  ListenerList;
		ListenerList mListeners;
		
		Datum(TValue inValue) :mValue(inValue) {}
		
		void EnsureListenerRegistered(TStateListener *inListener);
		void UnregisterListener(TStateListener *inListener);
		void NotifyListeners();
		void MaybeSetVal(TValue inValue);
	};
	
	typedef std::map<std::string, Datum> DatumMap;	
	DatumMap mDB;
	
	//////////
	// Accept only keys of the form "/pathname". Should not
	// end with a forward slash and should not contain more
	// than one sequence of forward slashes.
	// 
	void CheckForLegalKey(const std::string &inKey);
	
	void UnregisterListener(TStateListener *inListener,
							std::vector<std::string> inKeyList);
	
public:
	void Set(const std::string &inKey, TValue inValue);     
	TValue Get(TStateListener *inListener, const std::string &inKey);
};

END_NAMESPACE_FIVEL

#endif // TStateDB_H

