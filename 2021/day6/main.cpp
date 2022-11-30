#include <iostream>
#include <fstream>
#include <sstream>
#include <algorithm>
#include <numeric>
#include <chrono>
#include <array>

using namespace std;
using namespace std::chrono;

int64_t runSimulation(string inp, int daysToSimulate){
	array<int64_t,9> counts{};
	stringstream ss(inp);
	string item;
	while(getline(ss,item,','))
		counts[stoi(item)]++;
	for(int i = 0; i < daysToSimulate; i++){
		rotate(counts.begin(), counts.begin()+1, counts.end());
		counts[6] += counts[8];
	}
	return  accumulate(counts.begin(),counts.end(),0ll);
}

int main(){
	ifstream inp("input.txt");
	string counts;
	getline(inp,counts);
	auto start = high_resolution_clock::now();
	cout << runSimulation(counts, 80) << endl;
	cout << runSimulation(counts, 256) << endl;
	auto stop = high_resolution_clock::now();
	auto duration = duration_cast<microseconds>(stop - start);
	cout << duration.count() << " µs" << endl;
}

