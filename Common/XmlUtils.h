// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
// Miscellaneous libxml2-related functions and classes.

#if !defined (XmlUtils_H)
#define XmlUtils_H

struct _xmlNode;

BEGIN_NAMESPACE_FIVEL

//////////
// A lightweight pointer/iterator for referring to libxml2 XML nodes.
//
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

	// Get the contents of a text-only node.
	string text();

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

END_NAMESPACE_FIVEL

#define XML_CHECK_NAME(NODE,NAME) \
	CHECK((NODE).name() == (NAME), "expected <" NAME "> element")

#endif // XmlUtils_H

