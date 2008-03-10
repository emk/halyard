// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
// @BEGIN_LICENSE
//
// Halyard - Multimedia authoring and playback system
// Copyright 1993-2008 Trustees of Dartmouth College
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

// Miscellaneous libxml2-related functions and classes.

#if !defined (XmlUtils_H)
#define XmlUtils_H

struct _xmlNode;
struct _xmlDoc;

BEGIN_NAMESPACE_HALYARD

//////////
/// A lightweight pointer/iterator for referring to libxml2 XML nodes.
///
class xml_node {
public:
	// We declare local versions of libxml2 types here, so users of this
	// library can avoid including any libxml headers.
	typedef struct _xmlNode *node_ptr;     // xmlNodePtr
	typedef const node_ptr const_node_ptr; // const xmlNodePtr
	typedef unsigned char char_type;       // xmlChar

private:
	const_node_ptr mNode;
	xml_node();

public:
	typedef std::string string;

	static const char_type *to_utf8(const char *inStr);
	static const char *to_ascii(const char_type *inStr);

	xml_node(const_node_ptr inNode) : mNode(inNode) { ASSERT(mNode); }

    /// Iterator class for xml_node objects.
	class iterator {
		node_ptr mNode;
		bool mIsInMixed;

		void skip_whitespace();

	public:
		iterator() : mNode(NULL), mIsInMixed(false) {}
		iterator(const_node_ptr inNode, bool inIsInMixed = false);

		xml_node operator*() { return xml_node(mNode); }
		//xml_node operator->() { return xml_node(mNode); }
		iterator &operator++();

		friend bool operator==(const iterator &inLeft,
							   const iterator &inRight);
		friend bool operator!=(const iterator &inLeft,
							   const iterator &inRight);
	};

	// Basic collection support.  The 'mixed' versions assume that the node
	// may contain valid text content; the regular versions hide whitespace
	// nodes and throw an exception if they encounter non-whitespace text
	// content.
	size_t size();
	iterator begin();
	iterator end() { return iterator(); }
	size_t size_mixed();
	iterator begin_mixed();
	iterator end_mixed() { return iterator(); }

	// Useful accessors.
	string name();
	string attribute(const char *inName);

    // Returns true if this node is an element.
    bool is_element_node();

    // Returns true if this node is a text node, an entity ref, or
    // something else which represents plain text content.
    bool is_content_node();

    // Return the content of this node as a Unicode string.  Works on
    // any node which returns true for is_content_node.
    //
    // TODO - Yes, this is a silly duplicate of text(), but with slightly
    // different semantics.  We'll need to clean up the clients of this
    // class someday, once we have better Unicode support.
    utf16_string content();

	// Get the contents of an element node which contains only text.
    // @see content
	string text();

    // Extract the child elements and text of this node as XML.
    // @see text
    string contentAsXml();

	// Get an "only child".  Raise an error if more than one child is
	// present.
	xml_node only_child();

	// Modification functions.
	void append_text(const std::string &inText);
	xml_node new_child(const char *inName);
	xml_node new_child(const char *inName, const std::string &inData);
	void set_attribute(const char *inName, const std::string &inValue);
	
	// Conversion operator.
	operator node_ptr() { return mNode; }
};

inline bool operator==(const xml_node::iterator &inLeft,
					   const xml_node::iterator &inRight)
{
	return inLeft.mNode == inRight.mNode;
}

inline bool operator!=(const xml_node::iterator &inLeft,
					   const xml_node::iterator &inRight)
{
	return inLeft.mNode != inRight.mNode;
}

/// An XML document.
class xml_doc : boost::noncopyable {
    typedef struct _xmlDoc *doc_ptr;
    typedef const doc_ptr const_doc_ptr;

    doc_ptr mDoc;
    
public:
    xml_doc(const std::string &file);
    ~xml_doc();

    xml_node root();
};

END_NAMESPACE_HALYARD

#define XML_CHECK_NAME(NODE,NAME) \
	CHECK((NODE).name() == (NAME), "expected <" NAME "> element")

#endif // XmlUtils_H

