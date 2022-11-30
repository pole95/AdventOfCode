#include <iostream>
#include <fstream>
#include <chrono>
#include <vector>
#include <queue>
#include <cmath>
#include <unordered_map>
#include <functional>

using namespace std;
using namespace std::chrono;

using Point = pair<int, int>;
using CostPair = pair<double,Point>;

struct pointHash
{
    size_t operator() (const Point &p) const {
        return hash<int>()(p.first ^ (p.second << 4));
    }
};

int manhattanDistance(Point p1, Point p2){
	return abs(p1.first - p2.first) + abs(p1.second - p2.second);
}

int AStar(vector<vector<int>>& grid, Point start, Point end){
	vector<Point> directions = {{0,1},{0,-1},{1,0},{-1,0}};

	int width = grid[0].size();
	int height = grid.size();

	unordered_map<Point,int, pointHash> cost;

	cost[start] = 0;

	priority_queue<CostPair, vector<CostPair>, greater<CostPair>> candidates;
	candidates.emplace(0,start);

	while(!candidates.empty()){
		Point current = candidates.top().second;
		candidates.pop();

		if(current == end) break;
		
		for(auto p : directions){
			Point next = make_pair(current.first + p.first, current.second + p.second);
			if (next.first < 0 || next.second < 0 || 
					next.first >= width || next.second >= height) continue;

			int newCost = cost[current] + grid[next.first][next.second];
			if (cost.find(next) == cost.end() || newCost < cost[next]){
				cost[next] = newCost;

				int heuristic = newCost + manhattanDistance(next,end);
				candidates.emplace(heuristic,next);
			}
		}
	}

	return cost[end];
}

int main(){
	auto t1 = high_resolution_clock::now();
	ifstream inp("input.txt");
	string line;
	vector<vector<int>> grid;
	while(getline(inp,line)){
		vector<int> intLine;
		for(auto c : line)
			intLine.push_back(c-'0');
		grid.push_back(intLine);
	}

	int width = grid[0].size();
	int height = grid.size();

	Point start {0, 0};
	Point end {width-1, height-1};
	Point end2 {width * 5 -1, height * 5 -1};

	vector<vector<int>> grid2(height *5, vector<int>(width * 5));
	for (int i = 0; i< width * 5; i++){
		for(int j = 0; j<height * 5; j++){
			int next = grid[j%height][i%width] + i/width + j/height;
            grid2[j][i] = next > 9 ? (next % 10)+1 : next;
		}
	}


	cout << AStar(grid,start,end) << endl; 
	cout << AStar(grid2,start,end2) << endl;

	
	auto t2 = high_resolution_clock::now();
	auto duration = duration_cast<microseconds>(t2 - t1);
	cout << duration.count() << " µs" << endl;
}
