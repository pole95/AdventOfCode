#include <iostream>
#include <fstream>
#include <chrono>
#include <unordered_set>

using namespace std;


struct pair_hash {
    inline std::size_t operator()(const std::pair<int,int> & v) const {
        return v.first*31+v.second;
    }
};

using intSet = unordered_set<pair<int,int>,pair_hash>;

bool move(intSet& east, intSet& south, int w, int h){
	bool moved = false;
	intSet newEast;
	intSet newSouth;
	for(auto[x,y] : east){
		pair<int,int> newCoord = make_pair((x+1)%w, y);
		if(east.find(newCoord) != east.end() ||
				south.find(newCoord) != south.end()){
			newEast.emplace(x,y);
		} else {
			moved = true;
			newEast.insert(newCoord);
		}
	}
	east = move(newEast);
	for(auto [x,y] : south){
		pair<int,int> newCoord = make_pair(x, (y+1)%h);
		if(east.find(newCoord) != east.end() ||
				south.find(newCoord) != south.end()){
			newSouth.emplace(x,y);
		} else {
			moved = true;
			newSouth.insert(newCoord);
		}
	}
	south = move(newSouth);
	return moved;
}

int main(){
	auto start = chrono::high_resolution_clock::now();
	fstream inp("input.txt");
	string line;
	intSet east;
	intSet south;
	int w =0, h=0;
	while(getline(inp,line)){
		w = line.length();
		for (int x = 0; x<line.length(); x++){
			if(line[x] == 'v')
				south.emplace(x,h);
			else if(line[x] == '>')
				east.emplace(x,h);
		}
		h++;
	}

	int step = 1;
	while (move(east,south,w,h))
		step ++;

	cout << step << endl;
	auto stop = chrono::high_resolution_clock::now();
	auto duration = chrono::duration_cast<chrono::microseconds>(stop-start);
	cout << duration.count() << " µs" << endl;
}
