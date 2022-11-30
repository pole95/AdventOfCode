#include <iostream>
#include <fstream>
#include <chrono>
#include <unordered_map>
#include <limits>

using namespace std;
using namespace std::chrono;

int64_t polymerize(string start, int rounds, unordered_map<string, string> rules){
	unordered_map<string, int64_t> freqs;
	for(int i = 0; i < start.size()-1; ++i){
		string sub = start.substr(i,2);
		freqs[sub]++;
	}
	for(int i = 0; i < rounds; i++){
		unordered_map<string, int64_t> tmpFreqs;
		for(auto [k,v] : freqs){
			string firstEl = k[0] + rules[k];
			string secondEl = rules[k] + k[1];
			tmpFreqs[firstEl] += v;
			tmpFreqs[secondEl] += v;
		}
		swap(tmpFreqs,freqs);
	}
	unordered_map<char, int64_t> counts;
	counts[start[start.size()-1]] = 1;
	for(auto [k,v] : freqs)
		counts[k[0]] += v;

	int64_t mx = 0;
	int64_t mn = numeric_limits<int64_t>::max();

	for(auto [k,v] : counts){
		mn = min(mn, v);
		mx = max(mx, v);
	}
	return mx - mn;
}

int main(){
	auto start = high_resolution_clock::now();
	ifstream inp("input.txt");
	string line;
	getline(inp,line);
	
	string init(line);
	unordered_map<string, string> rules;
	while(getline(inp, line)){
		if(line.length())
			rules[line.substr(0,2)] = line[6];
	}
	
	cout << polymerize(init,10,rules) << endl;
	cout << polymerize(init,40,rules) << endl;
	auto stop = high_resolution_clock::now();
	auto duration = duration_cast<microseconds>(stop - start);
	cout << duration.count() << " µs" << endl;
}
