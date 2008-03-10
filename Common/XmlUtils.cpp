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

#include "CommonHeaders.h"

#include <libxml/globals.h>
#include <libxml/tree.h>

#include "XmlUtils.h"

USING_NAMESPACE_FIVEL


//=========================================================================
//  Support Code
//=========================================================================

const xml_node::char_type *xml_node::to_utf8(const char *inStr)
{
	ASSERT(xmlCheckUTF8(reinterpret_cast<const unsigned char*>(inStr)));
	return reinterpret_cast<const xmlChar*>(inStr);
}

const char *xml_node::to_ascii(const char_type *inStr)
{
	// This function should check carefully for UTF-8 characters.  Failing
	// to do so may result in overlong-encoding attacks, which might or might
	// not matter to the security of Tamale when run with untrusted data.
	// We check for UTF-8 data by checking for the high bit.
	unsigned const char *result =
		reinterpret_cast<unsigned const char*>(inStr);
	for (unsigned const char *c = result; *c != '\0'; c++)
		CHECK(*c < 128, "Unexpected UTF-8 data in XML file");
	return reinterpret_cast<const char*>(result);
	
}


//=========================================================================
//  xml_node::iterator Methods
//=========================================================================

xml_node::iterator::iterator(node_ptr inNode, bool inIsInMixed)
	: mNode(inNode), mIsInMixed(inIsInMixed)
{
	if (!mIsInMixed)
		skip_whitespace();
}


void xml_node::iterator::skip_whitespace()
{
	while (mNode != NULL && xmlNodeIsText(mNode))
	{
		// XXX - Disabled to work around bug in libxml2 2.5.0, which thinks
		// that the "\n  " after the open tag of the root element is not
		// a blank node.  Feel free to enable this check if the bug is
		// fixed.
		//CHECK(xmlIsBlankNode(mNode), "Unexpected text among XML elements");
		mNode = mNode->next;
	}
}

xml_node::iterator &xml_node::iterator::operator++()
{
    CHECK(mNode != NULL, "Unexpected end of XML elements");
	mNode = mNode->next;
	if (!mIsInMixed)
		skip_whitespace();
	return *this;
}


//=========================================================================
//  xml_node Methods
//=========================================================================

size_t xml_node::size()
{
	size_t count = 0;
	for (iterator node = begin(); node != end(); ++node)
		++count;
	return count;
}

xml_node::iterator xml_node::begin()
{
	return iterator(mNode->children);
}

size_t xml_node::size_mixed()
{
	size_t count = 0;
	for (iterator node = begin_mixed(); node != end_mixed(); ++node)
		++count;
	return count;
}

xml_node::iterator xml_node::begin_mixed()
{
	return iterator(mNode->children, true);
}

xml_node xml_node::only_child()
{
	iterator node = begin();
	CHECK(node != end(), "Expected XML node to have one child");
	iterator tester = node;
	CHECK(++tester == end(), "Expected XML node to have only one child");
	return *node;
}

xml_node::string xml_node::name()
{
	return string(to_ascii(mNode->name));
}

xml_node::string xml_node::attribute(const char *inName)
{
	xmlChar *attr = xmlGetProp(mNode, to_utf8(inName));
	CHECK(attr, "Missing expected XML attribute");
	string result(to_ascii(attr)); // XXX - Will leak attr if fails.
	xmlFree(attr);
	return result;
}

bool xml_node::is_element_node()
{
    return (mNode->type == XML_ELEMENT_NODE);
}

bool xml_node::is_content_node()
{
    return (mNode->type == XML_TEXT_NODE ||
            mNode->type == XML_ENTITY_REF_NODE);
}

utf16_string xml_node::content()
{
    xmlChar *str = xmlNodeGetContent(mNode);
    CHECK(str, "Expected XML node to have content");
    // XXX - Will leak str if fails.
    utf16_string result(utf16_from_utf8(reinterpret_cast<char*>(str)));
    xmlFree(str);
    return result;
}

xml_node::string xml_node::text()
{
	// Extract the text, expanding entities.
	// TODO - Check for element content (which is ignored by
	// xmlNodeListGetString at the moment).
	xmlChar *text = xmlNodeListGetString(mNode->doc, mNode->children, 1);
	if (text == NULL)
		return "";
	else
	{
        // XXX - Will leak text if fails.
		string result(to_ascii(text));
		xmlFree(text);
		return result;
	}
}

xml_node::string xml_node::contentAsXml() {
    xmlBufferPtr buf = xmlBufferCreate();
    CHECK(buf, "Could not allocate XML buffer");

    for (iterator i = begin_mixed(); i != end_mixed(); ++i)
        xmlNodeDump(buf, mNode->doc, (*i).mNode, 0, 0);
    
    string result(to_ascii(buf->content));
    xmlBufferFree(buf);
    return result;
}

void xml_node::append_text(const std::string &inText)
{
	CHECK(xmlAddChild(mNode, xmlNewText(to_utf8(inText.c_str()))),
		  "Could not add text to XML tree");
}

xml_node xml_node::new_child(const char *inName)
{
	return xmlNewChild(mNode, NULL, to_utf8(inName), NULL);
}

xml_node xml_node::new_child(const char *inName, const std::string &inData)
{
	return xmlNewTextChild(mNode, NULL, to_utf8(inName),
						   to_utf8(inData.c_str()));
}

void xml_node::set_attribute(const char *inName, const std::string &inValue)
{
	xmlSetProp(mNode, to_utf8(inName), to_utf8(inValue.c_str()));
}


//=========================================================================
//  xml_doc Methods
//=========================================================================

xml_doc::xml_doc(const std::string &file) {
    mDoc = xmlParseFile(file.c_str());
    CHECK(mDoc, "Failed to load XML file");
}
 
xml_doc::~xml_doc() {
    ASSERT(mDoc);
    xmlFreeDoc(mDoc);
}

xml_node xml_doc::root() {
	xmlNodePtr root_node = xmlDocGetRootElement(mDoc);
	CHECK(root_node, "No document root in XML file");
	return xml_node(root_node);
}
