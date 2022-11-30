#include <iostream>
#include <fstream>
#include <chrono>
#include <cassert>
#include <array>
#include <vector>
#include <algorithm>
#include <set>

using namespace std;
using namespace std::chrono;

void flash(int row, int col, set<pair<int, int>> &spentFlashes, vector<vector<int>> &grid){
	auto coordPair = make_pair(row,col);

	if(spentFlashes.find(coordPair) != spentFlashes.end())
		return;

	spentFlashes.insert(coordPair);

	for(int i = -1; i <= 1; i++){
		for(int j = -1; j <= 1; j++){
			if(i == 0 && j == 0)
				continue;
			int newRow = row + i, newCol = col + j;
			
			if(newRow < 0 || newCol < 0 || newRow > grid.size() - 1 || newCol > grid[newRow].size() - 1)
				continue;
			if(++grid[newRow][newCol] > 9)
				flash(newRow, newCol, spentFlashes, grid);
		}
	}
}

int step(vector<vector<int>> &grid){
	set<pair<int, int>> flashed;
	for(int i = 0; i < grid.size(); i++){
		for(int j = 0; j < grid[i].size(); j++)
			grid[i][j]++;
	}

	for(int i = 0; i < grid.size(); i++){
		for(int j = 0; j < grid[i].size(); j++)
			if (grid[i][j] > 9)
				flash(i, j, flashed, grid);
	}

	for(auto [r,c]: flashed)
		grid[r][c] = 0;

	return flashed.size();
}

int main(){
	auto start = high_resolution_clock::now();
	ifstream inp("input.txt");
	string item;
	vector<vector<int>> input;
	while(getline(inp,item)){
		input.push_back(vector<int>());
		for(char c : item)
			input.back().push_back(c-'0');
	}
	int flashes = 0;
	int totalOctopi = input.size() * input[0].size();
	int i = 0;
	while(true){
		int stepFlashes = step(input);
		if(i < 100)
			flashes += stepFlashes;
		if(stepFlashes == totalOctopi)
			break;
		i++;
	}
	cout << flashes << endl;
	cout << i+1 << endl;
	auto stop = high_resolution_clock::now();
	auto duration = duration_cast<microseconds>(stop - start);
	cout << duration.count() << " µs" << endl;
}
