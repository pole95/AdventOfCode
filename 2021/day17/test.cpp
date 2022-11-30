#include <iostream>

using namespace std;

int main(){
	int p1(0), p2(0);
	int lx=201, ly=-99, hx=230, hy=-65;
	auto simulate = [&](int dx, int dy){
		int x(0), y(0), ans(0);
		while(x<=hx && y>=ly){
			x+=dx;
			y+=dy--;
			ans = max(ans, y);
			if(dx)
				dx--;
			if(lx<=x && x<=hx && ly<=y && y<=hy)
				return ans;
		}
		return -1;
	};
	for(int y = ly;y<=-ly;y++){
		for(int x = 0; x<=hx;x++){
			int res = simulate(x, y);
			if(res != -1){
				p2++;
				p1=max(p1, res);
			}
		}
	}
	
	cout<<"[P1] "<<p1<<"\n[P2] "<<p2<<endl;
}
