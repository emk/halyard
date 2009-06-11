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

#ifndef TStateDB_H
#define TStateDB_H

BEGIN_NAMESPACE_HALYARD

class TStateDB;

//////////
/// Subclasses of TStateListener listener for changes to keys in a TStateDB. 
///
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

typedef shared_ptr<TStateListener> TStateListenerPtr;

//////////
/// A TStateDB stores key/value pairs (much like the Windows registry) and
/// notifies TStateListeners when those keys change.
///
class TStateDB {     
    friend class TStateListener;
        
    /// Represents the value of an event and all the Listerners
    /// to that event.
    struct Datum {
        TValue mValue;
        typedef std::vector<TStateListener *>  ListenerList;
        ListenerList mListeners;
        
        Datum(TValue inValue) : mValue(inValue) {}
        
        void EnsureListenerRegistered(TStateListener *inListener);
        void UnregisterListener(TStateListener *inListener);
        bool IsRegistered(TStateListener *listener);
        void NotifyListeners();
        void MaybeSetVal(TStateDB *inDB, TValue inValue);
        bool HasListeners();
    };
    friend struct Datum;
    
    enum { MAX_RECURSION = 64 };

    typedef std::map<std::string, Datum> DatumMap;  
    DatumMap mDB;
    int mNotifyCount;
    
    /// Accept only keys of the form "/pathname". Should not
    /// end with a forward slash and should not contain more
    /// than one sequence of forward slashes.
    void CheckForLegalKey(const std::string &inKey);
    
    void UnregisterListener(TStateListener *inListener,
                            std::vector<std::string> inKeyList);
    
public:
    TStateDB() : mNotifyCount(0) {}
    void Set(const std::string &inKey, TValue inValue);     
    TValue Get(TStateListener *inListener, const std::string &inKey);

    /// Attempt to remove all entries from the database.
    void Clear();
};

extern TStateDB gStateDB;

END_NAMESPACE_HALYARD

#endif // TStateDB_H

