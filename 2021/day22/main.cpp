#include <iostream>
#include <fstream>
#include <chrono>
#include <vector>
#include <map>
#include <regex>

using namespace std;
struct Cube{
	int x,xx,y,yy,z,zz;
	Cube(vector<int>coords): x(coords[0]), xx(coords[1]),
							 y(coords[2]), yy(coords[3]),
							 z(coords[4]), zz(coords[5]) {}
	Cube(int x, int xx,
		int y, int yy,
		int z, int zz):
		x(x), xx(xx), y(y),
		yy(yy), z(z), zz(zz){}
	uint64_t volBounded(){
		if(x<-50 || xx > 50 || y < -50 || y > 50 || z < -50 || z > 50)
			return 0;
		return vol();
	}

	uint64_t vol(){
		uint64_t out = xx-x+1;
		out *= yy-y+1;
		out *= zz-z+1;
		return out;
	}
};

bool intersects(Cube& a, Cube& b){
	if(a.xx < b.x || b.xx < a.x) return false;
	if(a.yy < b.y || b.yy < a.y) return false;
	if(a.zz < b.z || b.zz < a.z) return false;
	return true;
}

void subtract(Cube& a, Cube& b, vector<Cube>& build){
	Cube c(max(a.x, b.x), min(a.xx,b.xx), max(a.y,b.y),
			min(a.yy,b.yy), max(a.z,b.z), min(a.zz,b.zz));
	if(a.z < c.z) 		build.emplace_back(a.x, a.xx, a.y, a.yy, a.z, c.z-1);
	if(c.zz < a.zz) 	build.emplace_back(a.x, a.xx, a.y, a.yy, c.zz+1, a.zz);
	if(a.x < c.x)		build.emplace_back(a.x, c.x-1, a.y, a.yy, c.z, c.zz);
	if(c.xx < a.xx) 	build.emplace_back(c.xx+1, a.xx, a.y, a.yy, c.z, c.zz);
	if(a.y < c.y) 		build.emplace_back(c.x, c.xx, a.y, c.y-1, c.z, c.zz);
	if(c.yy < a.yy) 	build.emplace_back(c.x, c.xx, c.yy+1, a.yy, c.z, c.zz);
}

int main(){
	auto start = chrono::high_resolution_clock::now();
	fstream inp("input.txt");
	string line;
	regex r("(-?\\d+)");
	vector<Cube> cubes;
	while(getline(inp,line)){
		int idx = line.find(' ');
		bool on = (line.substr(0,idx)=="on");
		sregex_iterator iter(line.begin(),line.end(),r);
		sregex_iterator end;
		vector<int> coords;
		while(iter != end)
		{
			coords.push_back(stoi((*iter)[0]));
			++iter;
		}
		Cube cur(coords);
		vector<Cube> build;
		for(Cube& c: cubes){
			if(intersects(c,cur))
				subtract(c,cur,build);
			else
				build.push_back(c);
		}
		if(on)
			build.push_back(cur);
		cubes = move(build);
		}

	uint64_t p1=0, p2=0;
	for(auto c: cubes){
		p1 += c.volBounded();
		p2 += c.vol();
	}

	cout << p1 << endl;
	cout << p2 << endl;
	auto stop = chrono::high_resolution_clock::now();
	auto duration = chrono::duration_cast<chrono::microseconds>(stop-start);
	cout << duration.count() << " µs" << endl;
}
