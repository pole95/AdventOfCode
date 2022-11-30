#include <iostream>
#include <fstream>
#include <sstream>
#include <chrono>
#include <vector>
#include <set>
#include <algorithm>

using namespace std;
using namespace std::chrono;

pair<int, int> foldX(pair<int, int> dot, int x){
	if (dot.first < x)
		return dot;
	else
		return make_pair(x*2 - dot.first, dot.second);
}

pair<int, int> foldY(pair<int, int> dot, int y){
	if (dot.second < y)
		return dot;
	else
		return make_pair(dot.first, y*2 - dot.second);
}

set<pair<int,int>> doFold(set<pair<int, int>> dots, pair<char, int> fold){
	set<pair<int, int>>	nextDots;
	for(auto dot: dots){
		switch(fold.first){
			case 'x':
				nextDots.insert(foldX(dot, fold.second));
				break;
			case 'y':
				nextDots.insert(foldY(dot, fold.second));
				break;
		}
	}
	return nextDots;
}

void run(set<pair<int, int>>& dots, vector<pair<char, int>>& folds){
	int p1 = -1;
	for(auto fold: folds){
		dots = doFold(dots, fold);
		if(p1 == -1)
			p1 = dots.size();
	}

	cout << p1 << endl;

	int maxX = max_element(dots.begin(), dots.end(), 
            [](const auto& lhs, const auto& rhs) { return lhs.first < rhs.first; })->first;
	int maxY = max_element(dots.begin(), dots.end(), 
            [](const auto& lhs, const auto& rhs) { return lhs.second < rhs.second; })->second;

	for(int y = 0; y <= maxY; y++){
		for(int x = 0; x <= maxX; x++)
			cout << (dots.find(make_pair(x,y)) == dots.end() ? '.' : '#');
		cout << endl;
	}
	cout << endl;
}

int main(){
	auto start = high_resolution_clock::now();
	ifstream inp("input.txt");
	string line, fold, along;
	char comma = ',', equals = '=', axis = 'x';
	int x=0, y=0, foldCoord;
	set<pair<int, int>> dots;
	vector<pair<char, int>> folds;
	while(getline(inp,line) && line.length()){
		stringstream ss(line);
		ss >> x >> comma >> y;
		dots.insert(make_pair(x, y));
	}
	while(getline(inp, line)){
		stringstream ss(line);
		ss >> fold >> along >> axis >> equals >> foldCoord;
		folds.push_back(make_pair(axis, foldCoord));
	}
	run(dots, folds);
	auto stop = high_resolution_clock::now();
	auto duration = duration_cast<microseconds>(stop - start);
	cout << duration.count() << " µs" << endl;
}
