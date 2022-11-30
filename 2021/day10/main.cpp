#include <iostream>
#include <fstream>
#include <chrono>
#include <vector>
#include <algorithm>

using namespace std;
using namespace std::chrono;

int scoreChar(char in){
	switch(in){
		case ')':
			return 3;
		case ']':
			return 57;
		case '}':
			return 1197;
		case '>':
			return 25137;
	}
}

int scoreCharP2(char in){
	switch(in){
		case '(':
			return 1;
		case '[':
			return 2;
		case '{':
			return 3;
		case '<':
			return 4;
	}
}

void part(vector<string> lines){
	int total = 0;
	vector<int64_t> p2;

	vector<char> stack;
	for (string line : lines){
		
		for (char c : line){
			if(c == '(' || c == '[' || c == '{' || c == '<') {
				stack.push_back(c);
			} else {
				 char stackTop = stack.back();
				 if( c == ')' && stackTop != '('){
				 	 total += scoreChar(c);
				 	 stack.clear();
				 	 break;
				 } else if ( c == ']' && stackTop != '['){
				 	 total += scoreChar(c);
				 	 stack.clear();
				 	 break;
				 } else if ( c == '}' && stackTop != '{'){
				 	 total += scoreChar(c);
				 	 stack.clear();
				 	 break;
				 } else if ( c == '>' && stackTop != '<'){
				 	 total += scoreChar(c);
				 	 stack.clear();
				 	 break;
				 } else {
				 	 stack.pop_back();
				 }
			}
		}

		if (stack.size() > 0){
			int64_t lineScore = 0;
			reverse(stack.begin(), stack.end());
			for (char c : stack)
				lineScore = lineScore * 5 + scoreCharP2(c);
			p2.push_back(lineScore);
			stack.clear();
		}
	}
	sort(p2.begin(), p2.end());
	cout << total << endl;
	cout << p2[p2.size()/2] << endl;
}

int main(){
	auto start = high_resolution_clock::now();
	ifstream inp("input.txt");
	string item;
	vector<string> lines;
	while(getline(inp,item))
		lines.push_back(item);
	part(lines);
	auto stop = high_resolution_clock::now();
	auto duration = duration_cast<microseconds>(stop - start);
	cout << duration.count() << " µs" << endl;
}
