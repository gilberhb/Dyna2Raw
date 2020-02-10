// LSDynaToRaw.cpp : This program extracts mesh objects from a set of LS-Dyna Keyfiles and saves the data for each part/object found
//					 to a pair of tab separated text files representing the nodes and element connectivity.
// Copyright Hunter Gilbert 2020 <hunter DOT gilbert AT outlook.com>
//

#include <iostream>
#include <vector>
#include <algorithm>
#include <tuple>
#include <map>
#include <iostream>
#include <string>
#include <boost/program_options.hpp>
#include <boost/filesystem.hpp>
#include <boost/lexical_cast.hpp>
#include <locale>
#include <stdexcept>
#include <fstream>
#include <memory>

namespace po = boost::program_options;
namespace fs = boost::filesystem;
using std::cout;
using std::endl;
using std::wifstream;
using std::string;
using std::vector;
using std::get;
using std::make_tuple;
using std::begin;
using std::end;
using std::map;

std::locale loc("");

typedef std::tuple<double, double, double> Node;
typedef std::tuple<int, int, int, int, int, int, int, int> Element;

struct Nodes
{
	void AddNode(int id, double x_, double y_, double z_)
	{
		nids.push_back(id);
		x.push_back(x_);
		y.push_back(y_);
		z.push_back(z_);
	}

	void AddNode(int id, Node const &x_)
	{
		nids.push_back(id);
		x.push_back(get<0>(x_));
		y.push_back(get<1>(x_));
		z.push_back(get<2>(x_));
	}

	Node GetNode(int k) const
	{
		return make_tuple(
			x.at(k)
			, y.at(k)
			, z.at(k)
		);
	}

	int GetNodeID(int k) const
	{
		return nids.at(k);
	}

	vector<double> nids;
	vector<double> x;
	vector<double> y;
	vector<double> z;
};

struct Elements
{
	void AddElement(int eid_, int pid_, Element const &e_)
	{
		//ensure the element id is unique
		if (std::find(begin(eids), end(eids), eid_) != end(eids)) {
			throw std::runtime_error("Found two elements with the same element id");
		}

		eids.push_back(eid_);
		pids.push_back(pid_);
		n1.push_back(get<0>(e_));
		n2.push_back(get<1>(e_));
		n3.push_back(get<2>(e_));
		n4.push_back(get<3>(e_));
		n5.push_back(get<4>(e_));
		n6.push_back(get<5>(e_));
		n7.push_back(get<6>(e_));
		n8.push_back(get<7>(e_));
	}

	Element FindElement(int eid_)
	{
		auto it = std::find(begin(eids), end(eids), eid_);
		if (it == end(eids)) throw std::runtime_error("Could not find a requested element id");
		int index = it - begin(eids);

		return std::make_tuple(
			n1.at(index)
			, n2.at(index)
			, n3.at(index)
			, n4.at(index)
			, n5.at(index)
			, n6.at(index)
			, n7.at(index)
			, n8.at(index)
		);
	}

	Element GetElement(int k) const
	{
		return std::make_tuple(
			n1.at(k)
			, n2.at(k)
			, n3.at(k)
			, n4.at(k)
			, n5.at(k)
			, n6.at(k)
			, n7.at(k)
			, n8.at(k)
		);
	}

	int GetElementID(int k) const
	{
		return eids.at(k);
	}

	int GetPartID(int k) const
	{
		return pids.at(k);
	}

	vector<int> eids;
	vector<int> pids;
	vector<int> n1;
	vector<int> n2;
	vector<int> n3;
	vector<int> n4;
	vector<int> n5;
	vector<int> n6;
	vector<int> n7;
	vector<int> n8;
};

struct FiniteElementObject
{
	Nodes	nodes;
	Elements elements;
	map<int, int>	element_index;
	map<int, int>	node_index; //maps node ids to vector positions in the nodes list

	Element GetElement(int eid) const
	{
		return elements.GetElement(element_index.at(eid));
	}

	Node GetNode(int nid) const
	{
		return nodes.GetNode(node_index.at(nid));
	}
};

class LexerSymbol
{
public:
	typedef enum SymbolType_t
	{
		WHITESPACE = 0,
		NEWLINE,
		COMMENT,
		COMMA,
		ASTERISK,
		WORD,
		NUMBER,
		END_OF_FILE
	}  SymbolType;

	SymbolType	type;
	string symbol;
};

template <typename T, int N>
class Buffer
{
	typedef T value_type;
	typedef typename std::add_pointer<T>::type	pointer_type;
private:
	Buffer() {
		using std::begin;
		using std::end;
		std::fill(begin(buf), end(buf), static_cast<value_type>(0));
	}

	value_type buf[N];
};

class KeyFileLexer
{
	typedef string string;

public:
	KeyFileLexer(std::istream &stream_) 
		: stream(stream_)
		, state(0)
		, current_line(1)
	{
		stream.unsetf(std::ios_base::skipws);
	}

	LexerSymbol NextSymbol()
	{
		LexerSymbol S;
		//begin reading the symbol
		char c = stream.peek();
		switch (state)
		{
		case 0: // at the beginning of a line, $ and * are options in addition to everything else
			while (c == '$') { c = IgnoreComment(); current_line += 1; } //skip comments at the beginning of lines
			switch (c) {
			case '*':
				S = AcceptAsterisk();
				break;
			case '\n':
				S = AcceptNewline();
				current_line += 1;
				break;
			case ' ':
			case '\t':
			case 0x0c:
			case 0x0b:
				S = AcceptWhitespace();
				break;
			case ',':
				S.type = LexerSymbol::COMMA;
				S.symbol = ",";
				break;
			case '0':
			case '1':
			case '2':
			case '3':
			case '4':
			case '5':
			case '6':
			case '7':
			case '8':
			case '9':
			case '-':
				S = AcceptNumber();
				break;
			case EOF:
				S.type = LexerSymbol::END_OF_FILE; state = -1;
				break;
			default:
				S = AcceptWord();
			}

			break;
		case 1: //not at the beginning of a line
			switch (c) {
			case '\n':
				S = AcceptNewline();
				current_line += 1;
				break;
			case ' ':
			case '\t':
			case 0x0c:
			case 0x0b:
				S = AcceptWhitespace();
				break;
			case '0':
			case '1':
			case '2':
			case '3':
			case '4':
			case '5':
			case '6':
			case '7':
			case '8':
			case '9':
			case '-':
				S = AcceptNumber();
				break;
			case EOF:
				S.type = LexerSymbol::END_OF_FILE; state = -1;
				break;
			default:
				S = AcceptWord();
			}

			break; 
		}

		currentSymbol = S;
		return S;
	}

	LexerSymbol GetCurrentSymbol() const
	{
		return currentSymbol;
	}

	int GetCurrentLine() const
	{
		return current_line;
	}

protected:
	char IgnoreComment()
	{
		char c;
		while ((c = stream.peek()) != '\n' && c != EOF)
		{
			stream.ignore();
		}
		if (c = '\n') stream.ignore();
		return stream.peek();
	}

	LexerSymbol AcceptAsterisk()
	{
		LexerSymbol S;
		S.type = LexerSymbol::ASTERISK;
		S.symbol = "*";
		stream.ignore();

		state = 1;
		return S;
	}

	LexerSymbol AcceptNewline()
	{
		LexerSymbol S;
		S.type = LexerSymbol::NEWLINE;
		S.symbol = "\n";
		stream.ignore();

		state = 0;
		return S;
	}

	LexerSymbol AcceptWhitespace()
	{
		LexerSymbol S;
		S.type = LexerSymbol::WHITESPACE;
		S.symbol += stream.get();

		char c;
		while ((c = stream.peek()) != '\n' && std::isspace(c, loc) && c != EOF)
		{
			S.symbol += stream.get();
		}

		state = 1;
		return S;
	}

	LexerSymbol AcceptWord()
	{
		LexerSymbol S;
		S.type = LexerSymbol::WORD;
		S.symbol += stream.get();

		char c;
		while ((c = stream.peek()) != '\n' 
			&& (!std::isspace(c, loc))
			&& c != EOF)
		{
			S.symbol += stream.get();
		}

		state = 1;
		return S;
	}

	LexerSymbol AcceptNumber()
	{
		LexerSymbol S;
		S.type = LexerSymbol::NUMBER;
		S.symbol += stream.get();

		//First digits
		char c;
		while ((c = stream.peek()) != '\n' && std::isdigit(c, loc) && c != EOF)
		{
			S.symbol += stream.get();
		}

		//We may encounter a decimal place
		if (stream.peek() == '.') {
			S.symbol += stream.get();
		}
			
		//Digits after decimal place
		while ((c = stream.peek()) != '\n' && std::isdigit(c, loc) && c != EOF)
		{
			S.symbol += stream.get();
		}

		//Possible exponent
		if ((c = stream.peek()) == 'e' || c == 'E')
		{
			S.symbol += stream.get();
		}

		//possible negative sign or positive sign
		if ((c = stream.peek()) == '-' || c == '+')
		{
			S.symbol += stream.get();
		}

		//Exponent
		while ((c = stream.peek()) != '\n' && std::isdigit(c, loc) && c != EOF)
		{
			S.symbol += stream.get();
		}
		return S;
	}

private:
	std::istream &stream;
	int state;
	LexerSymbol		currentSymbol;
	int current_line;
};

class KeyFile
{
	typedef string string;
public:
	KeyFile(string name)
	{
		infile = fs::canonical(fs::path(name));
		if (!fs::is_regular_file(infile))
		{
			throw std::invalid_argument("Input file must be a regular file and not a directory or symlink.");
		}

		Parse();
	}

	KeyFile() {}

	void Append(string name)
	{
		infile = fs::canonical(fs::path(name));
		if (!fs::is_regular_file(infile))
		{
			throw std::invalid_argument("Input file must be a regular file and not a directory or symlink.");
		}

		Parse();
	}

	std::map<int, Elements> const & GetParts() const
	{
		return parts;
	}

	std::map<int, string> const & GetPartNames() const
	{
		return part_names;
	}

	FiniteElementObject const & GetObjects() const
	{
		return obj;
	}


protected:
	void Parse()
	{
		cout << "Reading from " << infile.string() << endl;
		std::ifstream f;
		f.open(infile.make_preferred().string(), std::ios_base::in);
		if (f.fail()) throw new std::runtime_error("The file exists, but it could not be opened.");
		lexer = std::make_unique<KeyFileLexer>(f);
		int nSymbols = 0;
		LexerSymbol S = lexer->NextSymbol();
		nSymbols++;
		int state = 0;
		while (S.type != LexerSymbol::END_OF_FILE)
		{
			switch (state) {
			case 0:
				if (S.type == LexerSymbol::ASTERISK) state = 1;
				break;
			case 1:

				if (S.type == LexerSymbol::WORD && !strcmpi(S.symbol.c_str(), "NODE")) { state = 2; }
				else if (S.type == LexerSymbol::WORD && !strcmpi(S.symbol.c_str(), "ELEMENT_SOLID")) { state = 3; }
				else if (S.type == LexerSymbol::WORD && !strcmpi(S.symbol.c_str(), "ELEMENT_SHELL")) { state = 3; }
				else if (S.type == LexerSymbol::WORD && !strcmpi(S.symbol.c_str(), "ELEMENT_BEAM")) { state = 3; }
				else if (S.type == LexerSymbol::WORD && !strcmpi(S.symbol.c_str(), "PART")) { state = 4; }
				else if (S.type == LexerSymbol::WORD && !strcmpi(S.symbol.c_str(), "PART_INERTIA")) { state = 4; }
				else { state = 0; }
				break;
			case 2: //NODE
				if (S.type == LexerSymbol::WHITESPACE) { /*Do nothing*/ }
				else if (S.type == LexerSymbol::NUMBER) {
					AcceptNode();
				} else if (S.type == LexerSymbol::NEWLINE) { /*Do nothing*/ }
				else if (S.type == LexerSymbol::ASTERISK) { state = 1; }
				else { state = 0; }
				
				break;
			case 3: //ELEMENT_SOLID or ELEMENT_BEAM or ELEMENT_SHELL
				if (S.type == LexerSymbol::WHITESPACE) { /*Do nothing*/ }
				else if (S.type == LexerSymbol::NUMBER) {
					AcceptElement();
				}
				else if (S.type == LexerSymbol::NEWLINE) { /*Do nothing*/ }
				else if (S.type == LexerSymbol::ASTERISK) { state = 1; }
				else { state = 0; }

				break;
			case 4: //PART
				if (S.type == LexerSymbol::WHITESPACE) { /*Do nothing*/ }
				else if (S.type == LexerSymbol::NEWLINE) { /*Do nothing*/ }
				else if (S.type == LexerSymbol::ASTERISK) { state = 1; }
				else if (S.type == LexerSymbol::WORD) {
					AcceptPart();
				}
				else { state = 0; }
			}
			
			S = lexer->NextSymbol();
		}

		cout << "Total number of nodes: " << obj.node_index.size() << endl;
		cout << "Total number of elements: " << obj.element_index.size() << endl;

		cout << "Parts with elements found: " << endl;
		for (auto it = begin(parts); it != end(parts); ++it)
		{
			int pid = it->first;
			cout << "Part: " << part_names[pid] << endl;
		}
		lexer.reset();
	}

	void AcceptPart()
	{
		string part_name = lexer->GetCurrentSymbol().symbol;
		LexerSymbol S;
		while ((S = lexer->NextSymbol()).type != LexerSymbol::NEWLINE)
		{
			part_name += S.symbol;
		}

		//the next number that we see is the part ID
		while ((S = lexer->NextSymbol()).type != LexerSymbol::NUMBER)
		{}

		int pid = boost::lexical_cast<int>(S.symbol);
		part_names[pid] = part_name;

	}

	void AcceptNode()
	{
		int nid = boost::lexical_cast<int>(lexer->GetCurrentSymbol().symbol);

		LexerSymbol S = lexer->NextSymbol();
		if (!(S.type == LexerSymbol::WHITESPACE || S.type == LexerSymbol::COMMA)) {
			throw std::runtime_error(
				string("Line ")
				+ boost::lexical_cast<string>(lexer->GetCurrentLine())
				+ string(": Could not parse file: element list appears to be malformed.")
			);
		}
		S = lexer->NextSymbol();
		double x = boost::lexical_cast<double>(S.symbol);

		S = lexer->NextSymbol();
		if (!(S.type == LexerSymbol::WHITESPACE || S.type == LexerSymbol::COMMA)) {
			throw std::runtime_error(
				string("Line ")
				+ boost::lexical_cast<string>(lexer->GetCurrentLine())
				+ string(": Could not parse file: element list appears to be malformed.")
			);
		}
		S = lexer->NextSymbol();
		double y = boost::lexical_cast<double>(S.symbol);

		S = lexer->NextSymbol();
		if (!(S.type == LexerSymbol::WHITESPACE || S.type == LexerSymbol::COMMA)) {
			throw std::runtime_error(
				string("Line ")
				+ boost::lexical_cast<string>(lexer->GetCurrentLine())
				+ string(": Could not parse file: element list appears to be malformed.")
			);
		}
		S = lexer->NextSymbol();
		double z = boost::lexical_cast<double>(S.symbol);

		while ((S = lexer->NextSymbol()).type != LexerSymbol::NEWLINE) {}

		//Now add the node
		obj.nodes.AddNode(nid,
			make_tuple(x, y, z));
		obj.node_index[nid] = (int)obj.nodes.nids.size() - 1;

	}

	void AcceptElement()
	{
		int eid = boost::lexical_cast<int>(lexer->GetCurrentSymbol().symbol);

		LexerSymbol S = lexer->NextSymbol();
		if (!(S.type == LexerSymbol::WHITESPACE || S.type == LexerSymbol::COMMA)) {
			throw std::runtime_error(
				string("Line ")
				+ boost::lexical_cast<string>(lexer->GetCurrentLine())
				+ string(": Could not parse file: element list appears to be malformed.")
			);
		}

		S = lexer->NextSymbol();
		if (S.type != LexerSymbol::NUMBER) {
			throw std::runtime_error(
				string("Line ")
				+ boost::lexical_cast<string>(lexer->GetCurrentLine())
				+ string(": Could not parse file: element list appears to be malformed.")
			);
		}
		int pid = boost::lexical_cast<int>(S.symbol);

		int nids[8]; std::fill(begin(nids), end(nids), 0);
		//Read in 8 nodes
		for (int i = 0; i < 8; ++i)
		{
			S = lexer->NextSymbol();
			if (!(S.type == LexerSymbol::WHITESPACE || S.type == LexerSymbol::COMMA)) {
				throw std::runtime_error(
					string("Line ")
					+ boost::lexical_cast<string>(lexer->GetCurrentLine())
					+ string(": Could not parse file: element list appears to be malformed.")
				);
			}
			S = lexer->NextSymbol();
			if (S.type != LexerSymbol::NUMBER) {
				throw std::runtime_error(
					string("Line ")
					+boost::lexical_cast<string>(lexer->GetCurrentLine())
					+ string(": Could not parse file: element list appears to be malformed.")
				);
			}
			nids[i] = boost::lexical_cast<double>(S.symbol);
		}

		//Now add the element
		obj.elements.AddElement(eid, pid,
			make_tuple(nids[0], nids[1], nids[2], nids[3], nids[4], nids[5], nids[6], nids[7]));
		obj.element_index[eid] = (int)obj.elements.eids.size() - 1;
		parts[pid].AddElement(eid, pid,
			make_tuple(nids[0], nids[1], nids[2], nids[3], nids[4], nids[5], nids[6], nids[7]));
	}


private:
	fs::path						infile;
	std::unique_ptr<KeyFileLexer>	lexer;
	FiniteElementObject				obj;
	std::map<int, string>			part_names;
	std::map<int, Elements >		parts;
};

class Converter
{
public:
	Converter(KeyFile const & kf_) : kf(kf_)
	{}
	
private:
	KeyFile const & kf;
};

FiniteElementObject IsolatePart(std::map<int, Elements> &parts,
	FiniteElementObject &objects,
	int pid)
{
	FiniteElementObject part;
	for (int i = 0; i < objects.elements.eids.size(); ++i)
	{
		//if the part id matches, copy to the output
		if (objects.elements.pids.at(i) == pid)
		{
			part.elements.eids.push_back(objects.elements.eids.at(i));
			part.elements.pids.push_back(objects.elements.pids.at(i));
			part.elements.n1.push_back(objects.elements.n1.at(i));
			part.elements.n2.push_back(objects.elements.n2.at(i));
			part.elements.n3.push_back(objects.elements.n3.at(i));
			part.elements.n4.push_back(objects.elements.n4.at(i));
			part.elements.n5.push_back(objects.elements.n5.at(i));
			part.elements.n6.push_back(objects.elements.n6.at(i));
			part.elements.n7.push_back(objects.elements.n7.at(i));
			part.elements.n8.push_back(objects.elements.n8.at(i));

			part.element_index[objects.elements.eids.at(i)] = part.elements.eids.size() - 1;
			
			//copy only the nodes that aren't already in the set, and don't copy the node with index zero
			if (part.node_index.count(part.elements.n1.back()) == 0 && part.elements.n1.back() != 0)
			{
				int nid = part.elements.n1.back();
				part.nodes.nids.push_back(nid);
				int old_index = objects.node_index[nid];
				part.nodes.x.push_back(objects.nodes.x.at(old_index));
				part.nodes.y.push_back(objects.nodes.y.at(old_index));
				part.nodes.z.push_back(objects.nodes.z.at(old_index));
				part.node_index[nid] = part.nodes.x.size() - 1;
			}

			//copy only the nodes that aren't already in the set
			if (part.node_index.count(part.elements.n2.back()) == 0 && part.elements.n2.back() != 0)
			{
				int nid = part.elements.n2.back();
				part.nodes.nids.push_back(nid);
				int old_index = objects.node_index[nid];
				part.nodes.x.push_back(objects.nodes.x.at(old_index));
				part.nodes.y.push_back(objects.nodes.y.at(old_index));
				part.nodes.z.push_back(objects.nodes.z.at(old_index));
				part.node_index[nid] = part.nodes.x.size() - 1;
			}

			//copy only the nodes that aren't already in the set
			if (part.node_index.count(part.elements.n3.back()) == 0 && part.elements.n3.back() != 0)
			{
				int nid = part.elements.n3.back();
				part.nodes.nids.push_back(nid);
				int old_index = objects.node_index[nid];
				part.nodes.x.push_back(objects.nodes.x.at(old_index));
				part.nodes.y.push_back(objects.nodes.y.at(old_index));
				part.nodes.z.push_back(objects.nodes.z.at(old_index));
				part.node_index[nid] = part.nodes.x.size() - 1;
			}

			//copy only the nodes that aren't already in the set
			if (part.node_index.count(part.elements.n4.back()) == 0 && part.elements.n4.back() != 0)
			{
				int nid = part.elements.n4.back();
				part.nodes.nids.push_back(nid);
				int old_index = objects.node_index[nid];
				part.nodes.x.push_back(objects.nodes.x.at(old_index));
				part.nodes.y.push_back(objects.nodes.y.at(old_index));
				part.nodes.z.push_back(objects.nodes.z.at(old_index));
				part.node_index[nid] = part.nodes.x.size() - 1;
			}

			//copy only the nodes that aren't already in the set
			if (part.node_index.count(part.elements.n5.back()) == 0 && part.elements.n5.back() != 0)
			{
				int nid = part.elements.n5.back();
				part.nodes.nids.push_back(nid);
				int old_index = objects.node_index[nid];
				part.nodes.x.push_back(objects.nodes.x.at(old_index));
				part.nodes.y.push_back(objects.nodes.y.at(old_index));
				part.nodes.z.push_back(objects.nodes.z.at(old_index));
				part.node_index[nid] = part.nodes.x.size() - 1;
			}

			//copy only the nodes that aren't already in the set
			if (part.node_index.count(part.elements.n6.back()) == 0 && part.elements.n6.back() != 0)
			{
				int nid = part.elements.n6.back();
				part.nodes.nids.push_back(nid);
				int old_index = objects.node_index[nid];
				part.nodes.x.push_back(objects.nodes.x.at(old_index));
				part.nodes.y.push_back(objects.nodes.y.at(old_index));
				part.nodes.z.push_back(objects.nodes.z.at(old_index));
				part.node_index[nid] = part.nodes.x.size() - 1;
			}

			//copy only the nodes that aren't already in the set
			if (part.node_index.count(part.elements.n7.back()) == 0 && part.elements.n7.back() != 0)
			{
				int nid = part.elements.n7.back();
				part.nodes.nids.push_back(nid);
				int old_index = objects.node_index[nid];
				part.nodes.x.push_back(objects.nodes.x.at(old_index));
				part.nodes.y.push_back(objects.nodes.y.at(old_index));
				part.nodes.z.push_back(objects.nodes.z.at(old_index));
				part.node_index[nid] = part.nodes.x.size() - 1;
			}

			//copy only the nodes that aren't already in the set
			if (part.node_index.count(part.elements.n8.back()) == 0 && part.elements.n8.back() != 0)
			{
				int nid = part.elements.n8.back();
				part.nodes.nids.push_back(nid);
				int old_index = objects.node_index[nid];
				part.nodes.x.push_back(objects.nodes.x.at(old_index));
				part.nodes.y.push_back(objects.nodes.y.at(old_index));
				part.nodes.z.push_back(objects.nodes.z.at(old_index));
				part.node_index[nid] = part.nodes.x.size() - 1;
			}
		}
	}

	return part;
}

void Print_summary(string const &name, FiniteElementObject &part)
{
	cout << "Part: " << name << endl;
	cout << "  Number of nodes: " << part.nodes.nids.size() << endl;
	cout << "  Number of elements: " << part.elements.eids.size() << endl;
}

FiniteElementObject Renumber_Nodes(FiniteElementObject const &part)
{
	FiniteElementObject R;

	map<int, int> node_remap;
	node_remap[0] = 0; //preserve 0 for "not a node"

	R.nodes.nids.resize(part.nodes.nids.size());
	R.nodes.x.resize(part.nodes.x.size());
	R.nodes.y.resize(part.nodes.y.size());
	R.nodes.z.resize(part.nodes.z.size());

	for (int j = 0; j < part.nodes.nids.size(); ++j)
	{
		node_remap[part.nodes.nids.at(j)] = j+1;
		R.nodes.nids[j] = j+1;
		R.nodes.x[j] = part.nodes.x.at(j);
		R.nodes.y[j] = part.nodes.y.at(j);
		R.nodes.z[j] = part.nodes.z.at(j);
		R.node_index[j+1] = j;
	}

	//loop over elements, renumber nodes according to the remapping scheme
	for (int j = 0; j < part.elements.eids.size(); ++j)
	{
		Element ej = part.elements.GetElement(j);
		Element ej_new;
		get<0>(ej_new) = node_remap[get<0>(ej)];
		get<1>(ej_new) = node_remap[get<1>(ej)];
		get<2>(ej_new) = node_remap[get<2>(ej)];
		get<3>(ej_new) = node_remap[get<3>(ej)];
		get<4>(ej_new) = node_remap[get<4>(ej)];
		get<5>(ej_new) = node_remap[get<5>(ej)];
		get<6>(ej_new) = node_remap[get<6>(ej)];
		get<7>(ej_new) = node_remap[get<7>(ej)];
		int eid = part.elements.GetElementID(j);
		int pid = part.elements.GetPartID(j);
		R.elements.AddElement(
			j+1, //renumber elements in order of appearance
			pid,
			ej_new
		);
	}
	
	return R;
}

void OutputNodes(string const &file_name, Nodes const &n)
{
	fs::path outfile = fs::path(file_name);
	bool output = true;
	if (fs::is_regular_file(outfile)) {
		cout << "File " << outfile.string() << " already exists. Would you like to overwrite? [y/n]";
		char c;
		std::cin >> c;
		if (!(c == 'y' || c == 'Y')) {
			output = false;
		}
	}

	if (output)
	{
		std::ofstream f(outfile.string());
		f.precision(16);
		for (int i = 0; i < n.nids.size(); ++i) {
			f << n.nids[i] << "\t" << n.x[i] << "\t" << n.y[i] << "\t" << n.z[i] << endl;
		}
	}
}

void OutputElements(string const &file_name, Elements const &e)
{
	fs::path outfile = fs::path(file_name);
	bool output = true;
	if (fs::is_regular_file(outfile)) {
		cout << "File " << outfile.string() << " already exists. Would you like to overwrite? [y/n]";
		char c;
		std::cin >> c;
		if (!(c == 'y' || c == 'Y')) {
			output = false;
		}
	}

	if (output)
	{
		std::ofstream f(outfile.string());
		f.precision(16);
		for (int i = 0; i < e.eids.size(); ++i) {
			f << e.eids[i]
				<< "\t" << e.n1[i]
				<< "\t" << e.n2[i]
				<< "\t" << e.n3[i]
				<< "\t" << e.n4[i]
				<< "\t" << e.n5[i]
				<< "\t" << e.n6[i]
				<< "\t" << e.n7[i]
				<< "\t" << e.n8[i]
				<< endl;
		}
	}
}

void OutputToFiles(string const& base_name, FiniteElementObject const &obj)
{
	//check that only one part is present in obj
	vector<int> pids = obj.elements.pids;
	std::sort(pids.begin(), pids.end());
	pids.erase(std::unique(pids.begin(), pids.end()), pids.end());
	if (pids.size() != 1) throw std::runtime_error("Internal error: cannot output a file for an object containing more than one part ID number.");

	OutputNodes(base_name + "-nodes.txt", obj.nodes);
	OutputElements(base_name + "-elements.txt", obj.elements);
}

int main(int argc, char *argv[])
{
	try {
		po::options_description generic("Generic options");
		generic.add_options()
			("help", "Print this help message")			
			("input-file", po::value< vector<string> >(), "Intput filename")
			("output-name", po::value<string>(), "Base name of output files");

		po::positional_options_description p;
		p.add("input-file", 1).add("output-name", 1);

		po::options_description cmdline_options;
		cmdline_options.add(generic);

		po::variables_map vm;
		po::store(po::command_line_parser(argc, argv).
			options(cmdline_options).positional(p).run(), vm);
		po::notify(vm);

		if (vm.count("help"))
		{
			cout << "Usage: LSDynaToRaw.exe input output" << endl << endl;
			cout << "Required Inputs:" << endl
				<< "  " << "input " << "\t\t" << "The input LSDyna Keyfile" << endl
				<< "  " << "output" << "\t\t" << "The name provided here will become the base name of the\n" << "        \t\t" << "output files, <name>-<part name>.nodes.txt and <name>-<part name>.elements.txt" << endl;
			cout << generic << endl;
		}

		else if (vm.count("input-file") && vm.count("output-name")) {
			vector<string> input_files = vm["input-file"].as< vector<string> >();
			KeyFile kf;
			for (int i = 0; i < input_files.size(); ++i)
				kf.Append(input_files.at(i));

			string output_base = vm["output-name"].as<string>();
			auto part_names = kf.GetPartNames();
			auto parts = kf.GetParts();
			auto objects = kf.GetObjects();
			for (auto it = parts.begin(); it != parts.end(); ++it)
			{
				FiniteElementObject part = IsolatePart(parts, objects, it->first);
				FiniteElementObject part_v2 = Renumber_Nodes(part);
				Print_summary(part_names[it->first], part_v2);

				OutputToFiles(output_base + "-" + part_names[it->first], part_v2);
			}
		}
		else {
			cout << "Usage: LSDynaToRaw.exe input output" << endl << endl;
			cout << "Required Inputs:" << endl
				<< "  " << "input " << "\t\t" << "The input LSDyna Keyfile" << endl
				<< "  " << "output" << "\t\t" << "The name provided here will become the base name of the\n" << "        \t\t" << "output files, <name>.nodes.txt and <name>.elements.txt" << endl;
			cout << generic << endl;
		}
	}
	catch (std::exception &e)
	{
		cout << e.what() << endl;
	}
	return 0;
}

// Run program: Ctrl + F5 or Debug > Start Without Debugging menu
// Debug program: F5 or Debug > Start Debugging menu

// Tips for Getting Started: 
//   1. Use the Solution Explorer window to add/manage files
//   2. Use the Team Explorer window to connect to source control
//   3. Use the Output window to see build output and other messages
//   4. Use the Error List window to view errors
//   5. Go to Project > Add New Item to create new code files, or Project > Add Existing Item to add existing code files to the project
//   6. In the future, to open this project again, go to File > Open > Project and select the .sln file
