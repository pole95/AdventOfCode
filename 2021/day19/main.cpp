#include <iostream>
#include <fstream>
#include <chrono>
#include <vector>
#include <unordered_set>
#include <sstream>
#include <queue>


using namespace std;

struct Point{
	int x,y,z;
	Point(const int x, const int y, const int z) : x(x), y(y), z(z) {}

	bool operator == (const Point& p) const {
		return x == p.x && y == p.y && z == p.z;
	}

	Point operator + (const Point& p) const {
		return Point(x + p.x, y + p.y, z + p.z);
	}

	Point operator - (const Point& p) const {
		return Point(x - p.x, y - p.y, z - p.z);	
	}
};

struct hash_point{
  std::size_t operator () (const Point& p) const {
    return p.z * 1000 * 1000 + p.y * 1000 + p.x;
  }
};
struct Scanner {
	vector<Point> beaconsScannerFrame;
	vector<Point> vectorsScannerFrame;
	vector<unordered_set<Point,hash_point>> vectorsRotatedFrame;
	vector<Point> beaconsGlobalFrame;
	Point scannerCoord = Point(0,0,0);
	int scannerId = -1;
	int orientation = -1;
};

using FT = Point(const Point&);

const auto transforms = vector<FT*>{
	[](const Point& p) { return Point(p.x, p.y, p.z); },
	[](const Point& p) { return Point(p.x, -p.z, p.y); },
	[](const Point& p) { return Point(p.x, -p.y, -p.z); },
	[](const Point& p) { return Point(p.x, p.z, -p.y); },
	[](const Point& p) { return Point(-p.y, p.x, p.z); },
	[](const Point& p) { return Point(p.z, p.x, p.y); },
	[](const Point& p) { return Point(p.y, p.x, -p.z); },
	[](const Point& p) { return Point(-p.z, p.x, -p.y); },
	[](const Point& p) { return Point(-p.x, -p.y, p.z); },
	[](const Point& p) { return Point(-p.x, -p.z, -p.y); },
	[](const Point& p) { return Point(-p.x, p.y, -p.z); },
	[](const Point& p) { return Point(-p.x, p.z, p.y); },
	[](const Point& p) { return Point(p.y, -p.x, p.z); },
	[](const Point& p) { return Point(p.z, -p.x, -p.y); },
	[](const Point& p) { return Point(-p.y, -p.x, -p.z); },
	[](const Point& p) { return Point(-p.z, -p.x, p.y); },
	[](const Point& p) { return Point(-p.z, p.y, p.x); },
	[](const Point& p) { return Point(p.y, p.z, p.x); },
	[](const Point& p) { return Point(p.z, -p.y, p.x); },
	[](const Point& p) { return Point(-p.y,-p.z, p.x); },
	[](const Point& p) { return Point(-p.z, -p.y, -p.x); },
	[](const Point& p) { return Point(-p.y, p.z, -p.x); },
	[](const Point& p) { return Point(p.z, p.y, -p.x); },
	[](const Point& p) { return Point(p.y, -p.z, -p.x); }

};

const auto inverseTransforms = vector<FT*> {
	[](const Point& p) { return Point(p.x, p.y, p.z); },
	[](const Point& p) { return Point(p.x, p.z, -p.y); },
	[](const Point& p) { return Point(p.x, -p.y, -p.z); },
	[](const Point& p) { return Point(p.x, -p.z, p.y); },
	[](const Point& p) { return Point(p.y, -p.x, p.z); },
	[](const Point& p) { return Point(p.y, p.z, p.x); },
	[](const Point& p) { return Point(p.y, p.x, -p.z); },
	[](const Point& p) { return Point(p.y, -p.z, -p.x); },
	[](const Point& p) { return Point(-p.x, -p.y, p.z); },
	[](const Point& p) { return Point(-p.x, -p.z, -p.y); },
	[](const Point& p) { return Point(-p.x, p.y, -p.z); },
	[](const Point& p) { return Point(-p.x, p.z, p.y); },
	[](const Point& p) { return Point(-p.y, p.x, p.z); },
	[](const Point& p) { return Point(-p.y,-p.z, p.x); },
	[](const Point& p) { return Point(-p.y, -p.x, -p.z); },
	[](const Point& p) { return Point(-p.y, p.z, -p.x); },
	[](const Point& p) { return Point(p.z, p.y, -p.x); },
	[](const Point& p) { return Point(p.z, p.x, p.y); },
	[](const Point& p) { return Point(p.z, -p.y, p.x); },
	[](const Point& p) { return Point(p.z, -p.x, -p.y); },
	[](const Point& p) { return Point(-p.z, -p.y, -p.x); },
	[](const Point& p) { return Point(-p.z, -p.x, p.y); },
	[](const Point& p) { return Point(-p.z, p.y, p.x); },
	[](const Point& p) { return Point(-p.z, p.x, -p.y); }
};

void printQueue(queue<int> q){
	while (!q.empty()){
		cout<<" "<<q.front();
		q.pop();
	}
	cout<<endl;
}

Point getVectorOrigin(Scanner& s, Point& vect){
	Point vectGlobalFrame = inverseTransforms[s.orientation](vect);
	int idx;
	bool found = false;
	for (int i = 0; i < s.beaconsScannerFrame.size(); i++){
		for(int j = i+1; j < s.beaconsScannerFrame.size(); j++){
			if (vectGlobalFrame == (s.beaconsScannerFrame[i] - s.beaconsScannerFrame[j])){
				idx = i;
				break;
			}
			if (found) break;
		}
	}
	return transforms[s.orientation](s.beaconsScannerFrame[idx]);
	
}

void orientScanner(Scanner& scannerToOrient, Scanner& scannerRelative, Point& commonVector){
	Point p1 = getVectorOrigin(scannerRelative, commonVector);
	Point p2 = getVectorOrigin(scannerToOrient, commonVector);
	scannerToOrient.scannerCoord = scannerRelative.scannerCoord + (p1-p2);
}

int main(){
	auto start = chrono::high_resolution_clock::now();
	fstream inp("input.txt");
	string line;
	vector<Scanner> scanners;
	while(getline(inp,line)){
		Scanner s;
		int endIdx = line.find(' ', 12);
		s.scannerId = stoi(line.substr(12,endIdx));
		while(getline(inp,line)){
			if(line.empty()) break;
			stringstream ss(line);
			char comma = ',';
			int x,y,z;
			ss >> x >> comma >> y >> comma >> z;
			s.beaconsScannerFrame.emplace_back(x,y,z);
		}
		for (int i = 0; i < s.beaconsScannerFrame.size(); i++){
			for(int j = i+1; j < s.beaconsScannerFrame.size(); j++){
				s.vectorsScannerFrame.emplace_back(s.beaconsScannerFrame[i] - s.beaconsScannerFrame[j]);
			}
		}
		for(auto t : transforms){
			unordered_set<Point,hash_point> rotatedFrame;
			for(auto p : s.vectorsScannerFrame)
				rotatedFrame.insert(t(p));
			s.vectorsRotatedFrame.push_back(rotatedFrame);
		}
		scanners.push_back(s);
	}

	scanners[0].orientation = 0;
	queue<int> workset;
	unordered_set<int> matched;
	workset.push(0);
	matched.insert(0);

	while(!workset.empty()){
		Scanner& s1 = scanners[workset.front()];
		workset.pop();
		for(int i = 0; i < scanners.size(); i++){
			if (i == s1.scannerId) continue;
			if (matched.find(i) != matched.end()) continue;
			Scanner& s2 = scanners[i];
			bool found = false;
			for(int j = 0; j < 24; j++){
				int count = 0;
				for (auto v : s2.vectorsRotatedFrame[j]) {
					if(s1.vectorsRotatedFrame[s1.orientation].find(v)
							!= s1.vectorsRotatedFrame[s1.orientation].end())
						count++;
					if (count >= 12){
						found = true;
						s2.orientation = j;
						orientScanner(s2, s1, v);
						workset.push(i);
						matched.insert(i);
						break;
					}
					if (found) break;
				}
			}
		}
	}

	unordered_set<Point,hash_point> beacons;
	for(auto& s:scanners){
		auto transform = transforms[s.orientation];
		for(auto p : s.beaconsScannerFrame){
			Point tp = transform(p) + s.scannerCoord;
			s.beaconsGlobalFrame.push_back(tp);
			beacons.insert(tp);
		}
	}

	auto manhattan = [](Point p1, Point p2){ return abs(p1.x-p2.x) + abs(p1.y-p2.y) + abs(p1.z-p2.z);};
	
	int maxDist = 0;
	for(int i = 0; i < scanners.size(); i++){
		for(int j = i+1; j < scanners.size(); j++){
			Point d = scanners[i].scannerCoord - scanners[j].scannerCoord;
			maxDist = max(maxDist,abs(d.x) + abs(d.y) + abs(d.z));
		}
	}
	cout << beacons.size() << endl;
	cout << maxDist << endl;

	auto stop = chrono::high_resolution_clock::now();
	auto duration = chrono::duration_cast<chrono::microseconds>(stop-start);
	cout << duration.count() << " µs" << endl;
}
