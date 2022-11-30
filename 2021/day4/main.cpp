#include <vector>
#include <array>
#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <algorithm>

using namespace std;

// https://stackoverflow.com/questions/216823/how-to-trim-a-stdstring
// trim from start (in place)
static inline void ltrim(std::string &s) {
    s.erase(s.begin(), std::find_if(s.begin(), s.end(), [](unsigned char ch) {
        return !std::isspace(ch);
    }));
}

// trim from end (in place)
static inline void rtrim(std::string &s) {
    s.erase(std::find_if(s.rbegin(), s.rend(), [](unsigned char ch) {
        return !std::isspace(ch);
    }).base(), s.end());
}

// trim from both ends (in place)
static inline void trim(std::string &s) {
    ltrim(s);
    rtrim(s);
}


class Board {

	public:
		Board(vector<string> boardRows){
			for(int r = 0; r < 5; r++){
				stringstream ss(boardRows[r]);
				string item;
				int i = 0;
				while(getline(ss, item, ' ')){
					trim(item);
					if(item.size() > 0)
						boardNums[r][i++] = stoi(item);
				}
			}
		}

		bool mark(int call){
			for(int r = 0; r < 5; r++){
				for(int c = 0; c < 5; c++){
					if (boardNums[r][c] == call)
						boardState[r][c] = true;
				}
			}
			return wins();
		}

		int result(int call){
			int sum = 0;
			for(int r = 0; r < 5; r++){
				for(int c = 0; c < 5; c++){
					if(!boardState[r][c])
						sum += boardNums[r][c];
				}
			}
			return sum * call;
		}

		bool wins(){
			bool result = false;
			for(int r = 0; r < 5; r++)
				result |= checkRow(r);
			for(int c = 0; c < 5; c++)
				result |= checkCol(c);
			return result;
		}

	private:
		array<array<int,5>,5> boardNums{};
		array<array<bool,5>,5> boardState{}; 
		bool checkRow(int r){
			bool result = true;
			for(int c = 0; c < 5; c++){
				result &= boardState[r][c];
			}
			return result;
		}
		bool checkCol(int c){
			bool result = true;
			for(int r = 0; r < 5; r++){
				result &= boardState[r][c];
			}
			return result;
		}
};


int part1(vector<int> calls, vector<Board> boards){
	for (int call : calls){
		for (auto& board : boards){
			if (board.mark(call))
				return board.result(call);
		}
	}
	return -1;
}

int part2(vector<int> calls, vector<Board> boards){
	int count = 0;
	for (int call : calls){
		for (auto& board : boards){
			if (board.wins())
				continue;
			if (board.mark(call)){
				if(board.mark(call))
					count++;
				if (count == boards.size())
					return board.result(call);
			}
		}	
	}
	return -1;
}

int main(){
	ifstream inp("input.txt");
	
	vector<string> lines;

	while(!inp.eof()){
		string line;
		getline(inp,line);
		if(line.size() > 0)
			lines.push_back(line);
	}

	vector<int> calls;
	
	stringstream ss(lines[0]);
	string item;
	while(getline(ss, item, ',')){
		calls.push_back(stoi(item));
	}

	int i = 1;
	vector<Board> boards;
	while(i<lines.size()){
		Board board({lines.begin()+i,lines.begin()+i+5});
		boards.push_back(board);
		i += 5;
	}
	cout << part1(calls, boards) << endl;
	cout << part2(calls, boards) << endl;

	return 0;
}
