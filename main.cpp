#include <iostream>
#include <string>
#include <fstream>
#include <unistd.h>

#include <readline/readline.h>
#include <readline/history.h>

#include <lc.h>

using namespace std;
using namespace lc;

static const bool isTTY = isatty(STDIN_FILENO);

int main() {

	const string prompt{isTTY ? "λ>>> " : ""};

	while(!cin.eof()){
		try{
			stringstream ss;
			char *cinput  = readline(prompt.c_str());

			if (cinput == nullptr) break;
			string input{cinput};

			add_history(input.c_str());

			if (input == "clear"){
				system("clear");
				continue;
			} else if (input == "quit"){
				break;
			}

			ss << input;

			Scanner scanner{ss};
			Parser parser(scanner);
			Interpreter interpreter;

			auto asts = parser.parse();

			for(auto it = asts.begin(); it != asts.end(); ++it){
				unique_ptr<AST> ast = move(*it);

				if(ast){
					if (isTTY)
						cout << ast->describe("") << endl;
					auto result = interpreter.eval(move(ast));
					if (result)
						cout << *result;
					else
						cout << "<nil>";
				} else {
					cout << "<nil>";
				}
				cout << endl;
			}
		} catch (Error& error){
			cerr << error << endl;
		} catch (runtime_error& error){
			cerr << error.what() << endl;
		}
	}

	if(isTTY)
		cout << endl << "bye" << endl;
}


