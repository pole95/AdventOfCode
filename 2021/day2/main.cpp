#include <iostream>
#include <fstream>
#include <string>
#include <cstring>
#include <vector>
#include <sstream>
using namespace std;

//https://stackoverflow.blog/2019/10/11/c-creator-bjarne-stroustrup-answers-our-top-five-c-questions/ #5
vector<string> split(const string& s) {
    stringstream ss(s);
    vector<string> words;
    for (string w; ss>>w; ) words.push_back(w);
    return words;
}

int main(){
	ifstream f("input.txt");
	string l;
	int forward = 0;
	int depth = 0;
	int depth2 = 0;

	if(f.is_open()){
		while(getline(f,l)){
			auto splitted = split(l);
			if(!strcmp(splitted[0].c_str(),"forward")){
				int forwardUnits = stoi(splitted[1]);
				forward += forwardUnits;
				depth2 += depth * forwardUnits;
			}
			else if (!strcmp(splitted[0].c_str(),"down")){
				depth += stoi(splitted[1]);
			} else if (!strcmp(splitted[0].c_str(),"up")) {
				depth -= stoi(splitted[1]);
			}
		}
		f.close();
		cout << "Part1: " << forward * depth << endl;
		cout << "Part2: " << forward * depth2 << endl;
	}
}
