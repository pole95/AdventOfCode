#include <iostream>
#include <fstream>
#include <chrono>
#include <set>
#include <limits>

using namespace std;

set<pair<int,int>> runGeneration(string& lut, set<pair<int,int>>& pixels, int& infPixel){
	set<pair<int,int>> next;
	int mx = numeric_limits<int>::max(), my = numeric_limits<int>::max(), 
		lx = numeric_limits<int>::min(), ly = numeric_limits<int>::min();
	for(auto [x,y] : pixels){
		mx = min(mx,x);
		my = min(my,y);
		lx = max(lx,x);
		ly = max(ly,y);
	}

	for (int y = my - 1; y <= ly + 1; y++){
		for (int x = mx - 1; x <= lx + 1; x++){
			int coord = 0;
			for (int j = y - 1; j <= y + 1; j++){
				for (int i = x - 1; i <= x + 1; i++){
					if(mx <= i && lx >= i && my <= j && ly >= j) coord = coord << 1 |pixels.count({i,j});
					else coord = coord << 1 | infPixel;
				}
			}
			if(lut[coord] == '#')
				next.insert({x,y});
		}
	}
	infPixel = (lut[infPixel * (lut.size() - 1)] == '#');
	return next;
}

void simulate(set<pair<int,int>>&pixels, string& lut, int& infPixel,int generations){
	for(int i = 0; i < generations; i++)
		pixels = runGeneration(lut,pixels,infPixel);
}

int main(){
	auto start = chrono::high_resolution_clock::now();
	fstream inp("input.txt");
	string lut;
	getline(inp,lut);
	set<pair<int,int>> pixels;
	string line;
	for(auto[y, line] = pair{0, string{}}; getline(inp,line); y++){
		for(int x = 0 ; x<line.length(); x++)
			if(line[x] == '#') pixels.insert({x,y});
	}
	int infPixel = 0;
	simulate(pixels,lut,infPixel,2);
	cout << pixels.size() << endl;
	simulate(pixels,lut,infPixel,48);
	cout << pixels.size() << endl;
	auto stop = chrono::high_resolution_clock::now();
	auto duration = chrono::duration_cast<chrono::microseconds>(stop-start);
	cout << duration.count() << " µs" << endl;
}
