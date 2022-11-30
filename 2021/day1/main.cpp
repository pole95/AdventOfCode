#include <iostream>
#include <fstream>
#include <vector>
#include <string>

using namespace std;

int main(){
	ifstream f("input.txt");
	string l;
	vector<int> depths;
	if(f.is_open()){
		while(getline(f,l)){
			depths.push_back(stoi(l));
		}
		f.close();
	}
	int num_increases = 0;
	for (int i = 1; i<depths.size(); i++){
		if (depths[i] > depths[i-1])
			num_increases++;
	}
	cout << num_increases << endl;

	int num_sum_increases = 0;
	vector<int> avg_depths;
	for(int i = 0; i<depths.size()-2; i++)
		avg_depths.push_back(depths[i]+depths[i+1]+depths[i+2]);
	for(int i = 1; i< avg_depths.size(); i++)
		if(avg_depths[i] > avg_depths[i-1])
			num_sum_increases++;
	cout << num_sum_increases << endl;
}
