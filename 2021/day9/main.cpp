#include <iostream>
#include <fstream>
#include <chrono>
#include <cassert>
#include <array>
#include <vector>
#include <algorithm>

using namespace std;
using namespace std::chrono;

constexpr size_t SIZE = 100;

int findBasinSize(array<array<int, SIZE+2>, SIZE+2> &in, int i, int j){
	
	if(in[i][j] < 9){
		in[i][j] = 10; // da waren wir schon

		int size = 1;
		size += findBasinSize(in, i+1, j);
		size += findBasinSize(in, i-1, j);
		size += findBasinSize(in, i, j+1);
		size += findBasinSize(in, i, j-1);
		return size;
	}
	return 0;
}

int  part1(array<array<int, SIZE+2>, SIZE+2> in){
	int risk = 0;
	for(int i = 1; i < SIZE+1; i++){
		for(int j = 1; j < SIZE+1; j++){
			bool low = true;
			int height = in[i][j];
			low &= (height < in[i+1][j]);
			low &= (height < in[i-1][j]);
			low &= (height < in[i][j+1]);
			low &= (height < in[i][j-1]);

			if (low)
				risk += height + 1;
		}
	}
	return risk;

}

int part2(array<array<int, SIZE+2>, SIZE+2> in){
	vector<int> sizes;
	for(int i = 1; i < SIZE+1; i++){
		for(int j = 1; j < SIZE+1; j++){
			bool low = true;
			int height = in[i][j];
			low &= (height < in[i+1][j]);
			low &= (height < in[i-1][j]);
			low &= (height < in[i][j+1]);
			low &= (height < in[i][j-1]);

			if (low){
				sizes.push_back(findBasinSize(in, i, j));
			}
		}
	}
	sort(sizes.begin(),sizes.end(),greater<int>());
	return sizes[0] * sizes[1] * sizes[2];

}

int main(){
	auto start = high_resolution_clock::now();
	ifstream inp("input.txt");
	string item;
	vector<string> lines;
	while(getline(inp,item))
		lines.push_back(item);
	array<array<int, SIZE+2>, SIZE+2> input;
	for(int i = 0; i < SIZE+2; i++){
		for(int j = 0; j < SIZE+2; j++){
				input[i][j] = 9;
		}
	}

	for(int i = 1; i <= lines.size(); i++){
		for(int j = 1; j <= lines[0].size(); j++){
				input[i][j] = lines[i-1][j-1] - '0';
		}
	}
	cout << part1(input) << endl; 
	cout << part2(input) << endl; 
	auto stop = high_resolution_clock::now();
	auto duration = duration_cast<microseconds>(stop - start);
	cout << duration.count() << " µs" << endl;
}
