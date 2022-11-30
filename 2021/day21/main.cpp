#include <iostream>
#include <fstream>
#include <chrono>
#include <vector>
#include <map>

using namespace std;

struct Player{
	int score;
	int position;
	Player(int pos):position(pos),score(0){}
};

struct Die{
	Die():val(0),rolls(0){}
	int val;
	int rolls;
	int roll(){
		val++;
		val %= 100;
		rolls++;
		return val;
	}
};

bool playRound(Player& p, Die& d){
	for(int i=0; i<3; i++)
		p.position += d.roll();
	p.position %= 10;
	p.score += p.position ? p.position : 10;
	return (p.score > 999);
}

void part1(vector<Player> players, Die d){
	while(true){
		for(int i = 0; i < players.size(); i++){
			if(playRound(players[i],d)){
				cout << players[(i+1)%2].score * d.rolls << endl;
				return;
			}
		}
	}
}

map<int,int> FREQS= {
	{3,1},
	{4,3},
	{5,6},
	{6,7},
	{7,6},
	{8,3},
	{9,1}
};

uint64_t numWins(Player& p1, Player& p2, int turn){
	uint64_t res = 0;
	for(auto [k,v]:FREQS){
		if (!turn){
			p1.position = p1.position + k;
			p1.score += (p1.position % 10 ? p1.position % 10 : 10);
			if(p1.score >=21)
				res += v;
				else
				res += v * numWins(p1,p2,1);
		} else {
			p2.position = p2.position + k;
			p2.score += (p2.position % 10 ? p2.position % 10 : 10);
			if(p2.score>=21)
				continue;
				else
				res += v * numWins(p1,p2,0);
		}
	}
	return res;
}
uint64_t numWinExpl(int start1, int startscore1, int start2, int startscore2, int turn){
	uint64_t res = 0;

	for (auto [k,v]:FREQS){
		int pos1 = start1;
		int score1 = startscore1;
		int pos2 = start2;
		int score2 = startscore2;
		if(turn == 0){
			pos1 += v;
			int r = pos1 %10;
			if (r == 0) score1 += 10;
			else score1 += r;
			if (score1 >= 21) res += v;
			else res += v * numWinExpl(pos1,score1,pos2,score2,1);
		} else {
			pos2 += v;
			int r = pos2 %10;
			if (r == 0) score2 += 10;
			else score2 += r;
			if (score2 >= 21) continue;
			else res += v * numWinExpl(pos1,score1,pos2,score2,0);
		}
	}
	return res;

}


void part2(vector<Player> players){
	cout << numWinExpl(4,0,5,0,0) << endl;
}

int main(){
	auto start = chrono::high_resolution_clock::now();
	fstream inp("input.txt");
	string line;
	getline(inp,line);
	vector<Player> players;
	players.emplace_back(line[28] - '0');
	getline(inp,line);
	players.emplace_back(line[28] - '0');
	Die d = Die();
	vector<Player> players2 = players;
	part1(players, d);
	part2(players2);
	auto stop = chrono::high_resolution_clock::now();
	auto duration = chrono::duration_cast<chrono::microseconds>(stop-start);
	cout << duration.count() << " µs" << endl;
}
