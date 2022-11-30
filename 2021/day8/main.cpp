#include <iostream>
#include <fstream>
#include <sstream>
#include <algorithm>
#include <numeric>
#include <chrono>
#include <map>

using namespace std;
using namespace std::chrono;

map<int,int> numbers ={
	// gfedcba
	{0b1110111,0},
	{0b0100100,1},
	{0b1011101,2},
	{0b1101101,3},
	{0b0101110,4},
	{0b1101011,5},
	{0b1111011,6},
	{0b0100101,7},
	{0b1111111,8},
	{0b1101111,9}
};


map<char, char> getMap(vector<string> in){
	map<char, char> out;
	string _1, _4, _7;
	vector<int> freq(7);
	for(auto el : in){
		switch(el.size()){
			case 2: 
				_1 = el;
				break;
			case 3:
				_7 = el;
				break;
			case 4:
				_4 = el;
				break;
		}
		for(auto c: el)
			freq[c-'a']++;
	}

	for(auto c : _7)
		if(_1.find(c) == -1)
			out['a'] = c;
	for(int i = 0; i < 7; i++){
		switch(freq[i]){
			case 4:
				out['e'] = i + 'a';
				break;
			case 6:
				out['b'] = i + 'a';
				break;
			case 8:
				if(out['a'] != i + 'a')
					out['c'] = i + 'a';
				break;
			case 9:
				out['f'] = i + 'a';
				break;
			}
	}
	for(auto c : _4)
		if(string{out['b'], out['c'], out['f']}.find(c) == -1)
			out['d'] = c;

	for(int i = 0; i < 7; i++){
		if(freq[i] == 7 && out['d'] != i + 'a'){
			out['g'] = i + 'a';
			break;
		}
	}

	map<char,char> reversedOut;
	for( auto [k,v] : out)
		reversedOut[v] = k;
	return reversedOut;
}

void part(map<vector<string>, vector<string>> in){
	int p1 = 0, p2 = 0;
	for (auto el : in){
		auto m = getMap(el.first);
		auto out = el.second;
		int n = 0;
		for (auto p : out){
			int bits = 0; 
			for(char c: p){
				bits |= 1<<(m[c]-'a');
			}
			n = n * 10 + numbers[bits];
			switch(p.size()){
				case 2: case 3: case 4: case 7:
					p1++;
			}
		}
		p2 += n;
	}
	cout << p1 << endl;
	cout << p2 << endl;
}

int main(){
	auto start = high_resolution_clock::now();
	ifstream inp("input.txt");
	string item;
	vector<string> lines;
	map<vector<string>,vector<string>> m;
	while(getline(inp,item))
		lines.push_back(item);
	for(auto line : lines){
		stringstream ss(line);
		string item;
		vector<string> patterns, output;
		while(getline(ss, item, ' ')){
			if (item == "|")
				break;
			patterns.push_back(item);
		}
		while(getline(ss, item, ' '))
			output.push_back(item);
		m.insert(pair{patterns,output});

	}
	part(m); 
	auto stop = high_resolution_clock::now();
	auto duration = duration_cast<microseconds>(stop - start);
	cout << duration.count() << " µs" << endl;
}
