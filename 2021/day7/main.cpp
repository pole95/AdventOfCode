#include <iostream>
#include <fstream>
#include <sstream>
#include <algorithm>
#include <numeric>
#include <chrono>

using namespace std;
using namespace std::chrono;

//https://stackoverflow.com/a/1719155
int median(vector<int> v){
	size_t n = v.size()/2;
	nth_element(v.begin(),v.begin()+n,v.end());
	return v[n];
}

int mean(vector<int> v){
	return accumulate(v.begin(),v.end(),0)/v.size();
}

int part1(vector<int> crabs){
	int m = median(crabs);
	return accumulate(crabs.begin(), crabs.end(), 0, [&](int a, int b){ return a += abs(b-m);});
}

int part2(vector<int> crabs){
	int m = mean(crabs);
	return accumulate(crabs.begin(),crabs.end(),0,[&](int a, int b){ 
			int c = abs(b-m); 
			return a += (c * (c+1))/2;
			});
}

int main(){
	auto start = high_resolution_clock::now();
	ifstream inp("input.txt");
	vector<int> crabs;
	string item;
	while(getline(inp,item,','))
		crabs.push_back(stoi(item));

	cout << part1(crabs) << endl;
	cout << part2(crabs) << endl;
	auto stop = high_resolution_clock::now();
	auto duration = duration_cast<microseconds>(stop - start);
	cout << duration.count() << " µs" << endl;
}
