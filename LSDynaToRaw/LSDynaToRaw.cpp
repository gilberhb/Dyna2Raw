// LSDynaToRaw.cpp : This program extracts a specified mesh object from an LS Dyna Keyfile and saves the data
//					 to a pair of tab separated text files that represent the list of nodes and elements, respectively.
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
#include <locale>
#include <stdexcept>
#include <fstream>
#include <memory>

typedef std::tuple<double, double, double>	Node;

typedef std::vector<Node> NodeList;
typedef std::vector<int> Element;

typedef std::map<int, Node> NodeMap;
typedef std::map<int, Element>	ElementMap;

namespace po = boost::program_options;
namespace fs = boost::filesystem;
using std::cout;
using std::endl;
using std::wifstream;
using std::string;

std::locale loc("");

struct FiniteElementObject
{
	NodeMap			nodes;
	ElementMap		elements;
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
		case 0: // at the beginning of a line, $ and * are options
			if (c == '$') { S = AcceptComment(); }
			else if (c == '*') { S = AcceptAsterisk(); }
			//we don't break, because the following symbols could also occur
		case 1: //not at the beginning of a line
			if (c == '\n') {
				S = AcceptNewline(); 
			}
			else if (std::isspace(c, loc)) {
				S = AcceptWhitespace(); 
			}
			else if (std::isdigit(c, loc) || stream.peek() == '-') {
				S = AcceptNumber(); 
			}
			else if (c == EOF)
			{
				S.type = LexerSymbol::END_OF_FILE; state = -1;
			}
			else {
				S = AcceptWord(); 
			}
			break; 
		}

		return S;
	}

protected:
	LexerSymbol AcceptComment()
	{
		LexerSymbol S;
		S.type = LexerSymbol::COMMENT;
		stream.ignore();

		char c;
		while ((c = stream.peek()) != '\n' && c != EOF)
		{
			S.symbol += stream.get();
		}

		state = 1;
		return S;
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

protected:
	void Parse()
	{
		cout << "Reading from " << infile.string() << endl;
		f.open(infile.make_preferred().string(), std::ios_base::in);
		if (f.fail()) throw new std::runtime_error("The file exists, but it could not be opened.");
		lexer = std::make_unique<KeyFileLexer>(f);
		int nSymbols = 0;
		LexerSymbol S = lexer->NextSymbol();
		nSymbols++;
		while (S.type != LexerSymbol::END_OF_FILE)
		{
			S = lexer->NextSymbol();
			nSymbols++;
			cout.width(10);
			if (nSymbols % 100000 == 0)
				cout << '\r' << nSymbols;
		}
		cout << '\r' << "Total Number of Symbols Found: " << nSymbols << endl;
	}

	void Parse_AcceptComment()
	{

	}

private:
	fs::path		infile;
	std::ifstream	f;
	std::unique_ptr<KeyFileLexer>	lexer;
};

int main(int argc, char *argv[])
{
	try {
		po::options_description generic("Generic options");
		generic.add_options()
			("help", "Print this help message");

		po::options_description hidden("Hidden options");
		hidden.add_options()
			("input-file", po::value<string>(), "Intput filename")
			("output-name", po::value<string>(), "Base name of output files");

		po::positional_options_description p;
		p.add("input-file", 1).add("output-name", 1);

		po::options_description cmdline_options;
		cmdline_options.add(generic).add(hidden);

		po::variables_map vm;
		po::store(po::command_line_parser(argc, argv).
			options(cmdline_options).positional(p).run(), vm);
		po::notify(vm);

		if (vm.count("help"))
		{
			cout << "Usage: LSDynaToRaw.exe input output" << endl << endl;
			cout << "Required Inputs:" << endl
				<< "  " << "input " << "\t\t" << "The input LSDyna Keyfile" << endl
				<< "  " << "output" << "\t\t" << "The name provided here will become the base name of the\n" << "        \t\t" << "output files, <name>.nodes.txt and <name>.elements.txt" << endl;
			cout << generic << endl;
		}
		else if (vm.count("input-file") && vm.count("output-name")) {
			KeyFile kf(vm["input-file"].as<string>());
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
