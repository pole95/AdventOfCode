#include <cmath>
#include <iostream>
#include <chrono>

using namespace std;

bool check(int vX,int vY,int bx,int by,int tx,int ty){
	int x =0, y=0;
	while(true){
		x+=vX;
		y+=vY;

		if(x>=bx && x<=tx && y>=by && y<=ty) return true;
		if(x>tx || y<by) return false;

		if(vX>0) vX--;
		vY--;
	}
}

int main(){
	auto start = chrono::high_resolution_clock::now();
	int bx=201, tx=230, by=-99, ty=-65;
	int xVelMin = ceil(-1+sqrt(1+(8*bx))/2);
	int yMax = by*(by+1)/2;

	int p2 = 0;
	for(int x=xVelMin; x<=tx;x++){
		for(int y = by; y<=-by; y++){
			if (check(x,y,bx,by,tx,ty)) p2++;
		}
	}
	cout << yVelMax << endl;
	cout << p2 << endl;
	auto stop = chrono::high_resolution_clock::now();
	auto duration = chrono::duration_cast<chrono::microseconds>(stop-start);
	cout << duration.count() << " µs" << endl;
}
