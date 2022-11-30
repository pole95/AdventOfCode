#include <iostream>
#include <fstream>
#include <chrono>
#include <vector>
#include <algorithm>
#include <set>
#include <map>

using namespace std;
using namespace std::chrono;

struct Node {
	enum TYPE {START, END, BIG, SMALL};
	TYPE type;
	vector<Node *> neighbours;
	Node(string name){
		if(name == "start") type = START;
		else if(name == "end") type= END;
		else type = islower(name[0])? SMALL : BIG;
	}
};

void dfs(Node* cave, int& counter, set<Node*>& visited, Node* visitedTwice, bool part2){
	if(cave->type == Node::END){
		counter++;
		return;
	}

	if(cave->type == Node::SMALL){
		if (visited.find(cave) == visited.end())
			visited.insert(cave);
		else if (part2 && !visitedTwice)
			visitedTwice = cave;
		else
			return;
	}

	for(auto c: cave->neighbours)
		if(c->type != Node::START)
			dfs(c, counter, visited, visitedTwice, part2);

	if (cave->type == Node::SMALL)
		if(visitedTwice == cave)
			visitedTwice == nullptr;
		else
            visited.erase(cave);
}


int main(){
	auto start = high_resolution_clock::now();
	ifstream inp("input.txt");
	string item, item2;
	map<string, Node*> caves;
	while(getline(inp,item, '-') && getline(inp,item2)){
		if(caves.find(item) == caves.end())
			caves[item] = new Node(item);
		if(caves.find(item2) == caves.end())
			caves[item2] = new Node(item2);
		
		caves[item]->neighbours.push_back(caves[item2]);
		caves[item2]->neighbours.push_back(caves[item]);
	}
	
	set<Node*> visited;
	int counter = 0;
	Node* visitedTwice;

	dfs(caves["start"],counter, visited, visitedTwice, false);
	cout << counter << endl;

	counter = 0;
	visited.clear();
	visitedTwice == nullptr;

	dfs(caves["start"],counter, visited, visitedTwice, true);
	cout << counter << endl;
	auto stop = high_resolution_clock::now();
	auto duration = duration_cast<microseconds>(stop - start);
	cout << duration.count() << " µs" << endl;
}
