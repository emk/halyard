// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
// Miscellaneous libxml2-related functions and classes.

#if !defined (XmlUtils_H)
#define XmlUtils_H

#include <string>
#include <libxml/tree.h>

#include "TException.h"

BEGIN_NAMESPACE_FIVEL

//////////
// A lightweight pointer/iterator for referring to libxml2 XML nodes.
//
class xml_node {
	const xmlNodePtr mNode;
	xml_node();

public:
	typedef std::string string;

	static const xmlChar *to_utf8(const char *inStr);
	static const char *to_ascii(const xmlChar *inStr);

	xml_node(const xmlNodePtr inNode) : mNode(inNode) { ASSERT(mNode); }

	class iterator {
		xmlNodePtr mNode;
		bool mIsInMixed;

		void skip_whitespace();

	public:
		iterator() : mNode(NULL), mIsInMixed(false) {}
		iterator(xmlNodePtr inNode, bool inIsInMixed = false);

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
	iterator begin() { return iterator(mNode->children); }
	iterator end() { return iterator(); }
	size_t size_mixed();
	iterator begin_mixed() { return iterator(mNode->children, true); }
	iterator end_mixed() { return iterator(); }

	// Useful accessors.
	string name();
	string attribute(const char *inName);

	// Get an "only child".  Raise an error if more than one child is
	// present.
	xml_node only_child();

	// Modification functions.
	xml_node new_child(const char *inName);
	xml_node new_child(const char *inName, const std::string &inData);
	void set_attribute(const char *inName, const std::string &inValue);
	
	// Conversion operator.
	operator xmlNodePtr() { return mNode; }
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

END_NAMESPACE_FIVEL

#define XML_CHECK_NAME(NODE,NAME) \
	CHECK((NODE).name() == (NAME), "expected <" NAME "> element")

#endif // XmlUtils_H

