//
// Created by papo on 03/03/18.
//
#include "catch.hpp"
#include "lc.h"

using namespace std;
using namespace lc;

TEST_CASE("Testing Error", "[base]")
{

	{
		const exception* err = new Error{"gen_error", "something very bad happend"};
		REQUIRE(!strcmp(err->what(), "gen_error : something very bad happend"));
		delete err;
	}

	{
		struct GenericError : virtual public Error {
			GenericError(const string& message) : Error{"generic_error", message} {}
		};

		const exception* err = new GenericError{"oops! something broke!!"};
		REQUIRE(!strcmp(err->what(), "generic_error : oops! something broke!!"));
		delete err;
	}

	{
		const exception* err = new ScanError{"<stdin>", "oops!", {1,2}};
		REQUIRE(!strcmp(err->what(), "scanner error <stdin>:1:2 : oops!"));
		delete err;
	}
}


TEST_CASE( "Testing Position", "[base]" )
{
	Position pos;

	REQUIRE(pos.Column == lc::Position::defaultColumn);
	REQUIRE(pos.Line == lc::Position::defaultLine);
	REQUIRE(pos.Offset == lc::Position::defaultOffset);

	REQUIRE(pos.str() == "1:0");

	pos.incr();
	REQUIRE(pos.Column == Position::defaultColumn + 1);
	REQUIRE(pos.Line == Position::defaultLine);
	REQUIRE(pos.Offset == Position::defaultOffset + 1);

	REQUIRE(pos.str() == "1:1");

	pos.nl();
	REQUIRE(pos.Column == Position::defaultColumn);
	REQUIRE(pos.Line == Position::defaultLine + 1);
	REQUIRE(pos.Offset == Position::defaultOffset + 2);

	REQUIRE(pos.str() == "2:0");
}



TEST_CASE("Testing Token", "[base]")
{
	{
		Token tok{Token::id, "name", {1, 1}};
		REQUIRE(tok.str() == "(id \"name\" 1:1)");
		REQUIRE(tok == Token::id);
	}
}


TEST_CASE("Test Scanner", "[ast]")
{
	using t = Token::Type;

	vector<string> programs{};
	vector<vector<Token> > expected{};

	programs.push_back(R"()");
	expected.push_back({
	   Token(t::eof, Position{1, 0, 0})
	});

	programs.push_back(R"(\x.x)");
	expected.push_back({
			Token(t::lambda, Position{1, 0, 0}),
			Token(t::id, "x", Position{1, 1, 1}),
			Token(t::dot, Position{1,2,2}),
			Token(t::id, "x", Position{1, 3, 3}),
			Token(t::eof, Position{1,4,4})
	});

	programs.push_back(R"(\ abc . abc bcc    )");
	expected.push_back({
		   Token(t::lambda, Position{1,0,0}),
		   Token(t::id, "abc", Position{1,2,2}),
		   Token(t::dot, Position{1,6,6}),
		   Token(t::id, "abc", Position{1,8,8}),
		   Token(t::id, "bcc", Position{1,12,12}),
		   Token(t::eof, Position{1,19,19}),
	});


	programs.push_back(R"((\x.x)(y)
\xxx.yy)");
	expected.push_back({
		   Token(t::lparen, Position{1,0,0}),
		   Token(t::lambda, Position{1,1,1}),
		   Token(t::id, "x", Position{1,2,2}),
		   Token(t::dot, Position{1,3,3}),
		   Token(t::id, "x", Position{1,4,4}),
		   Token(t::rparen, Position{1,5,5}),
		   Token(t::lparen, Position{1,6,6}),
		   Token(t::id, "y", Position{1,7,7}),
		   Token(t::rparen, Position{1,8, 8}),
		   Token(t::nl, Position{1,9, 9}),
		   Token(t::lambda, Position{2, 0, 10}),
		   Token(t::id, "xxx", Position{2, 1, 11}),
		   Token(t::dot, Position{2, 4, 14}),
		   Token(t::id, "yy", Position{2, 5, 15}),
		   Token(t::eof, Position{2, 7, 17}),
   });

	for(uint64_t i = 0,
			l = programs.size(),
			le = expected.size(); i < l && i < le; ++i)
	{

		auto program = programs[i];
		auto test = expected[i];

		stringstream ss;
		ss << program;

		Scanner scanner{ss};

		uint64_t j{};
		int q{false};

		while(!q){
			auto tok = scanner.next();

			if (j < test.size()){
				REQUIRE(tok == test[j]);
			} else {
				REQUIRE(false);
			}

			j++;
			if (tok == Token::eof) q = true;
		}
	}

}



TEST_CASE("Test AST", "[ast]")
{
	{
		const NameAST name{"name", 10, true};
		REQUIRE(name.name() == "name");
		REQUIRE(name.index() == 10);
		REQUIRE(name.bound() == true);
		REQUIRE(name.type() == AST::Type::name);
		REQUIRE(name.str() == "name");

	}
}

