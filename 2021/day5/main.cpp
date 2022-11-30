#include <iostream>
#include <fstream>
#include <sstream>
#include <map>

using namespace std;

int runPart(ifstream &input, bool allowDiag){
	map<pair<int,int>,int> grid;

	string line;
	while(getline(input,line)){;
		stringstream ss(line);
		int x = 0, y = 0, xx = 0, yy = 0;
		char comma = ',';
		string arrow;
		ss >> x >> comma >> y >> arrow >> xx >> comma >> yy;
		if (x == xx || y == yy || allowDiag){
			int dx = ((x == xx) ? 0 : ((x < xx) ? 1 : -1 ));
			int dy = ((y == yy) ? 0 : ((y < yy) ? 1 : -1 ));
			int distX = abs(x - xx);
			int distY = abs(y - yy);
			int dist = distX ? distX : distY;
			for (int i = 0; i <= dist; i++){
				grid[make_pair(x,y)]++;
				x += dx;
				y += dy;
			}
		}
	}

	int c = 0;
	for(auto p : grid){
		if (p.second > 1)
			c++;
	}
	return c;
}

int main(){
	ifstream inp("input.txt");
	cout << runPart(inp, false) << endl;
	inp.clear();
	inp.seekg(0);
	cout << runPart(inp, true) << endl;

}
