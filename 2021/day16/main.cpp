#include <iostream>
#include <fstream>
#include <sstream>
#include <chrono>
#include <unordered_map>
#include <vector>
#include <algorithm>
#include <bitset>

using namespace std;
using namespace std::chrono;

struct Packet{
	uint8_t version; 
	uint8_t type;
	uint8_t id;
	uint32_t len;
	int64_t val;
	vector<Packet> subPackets;

	Packet():
		version(0), type(0), id(0), len(0), val(0), subPackets() {};
	uint64_t versionTotal() {
		uint64_t res = version;
		for(auto p : subPackets)
			res += p.versionTotal();
		return res;
	}

	uint64_t evaluate(){
		uint64_t result;
		switch(type){
			case 0:
				result = 0;
				for(auto p : subPackets)
					result += p.evaluate();
				break;
			case 1:
				result = 1;
				for(auto p : subPackets)
					result *= p.evaluate();
				break;
			case 2:
				result = min_element(subPackets.begin(), subPackets.end(), 
						[](Packet p1, Packet p2){return p1.evaluate() < p2.evaluate();})->evaluate();
				break;
			case 3:
				result = max_element(subPackets.begin(), subPackets.end(), 
						[](Packet p1, Packet p2){return p1.evaluate() < p2.evaluate();})->evaluate();
				break;
			case 4:
				return val;
			case 5:
				return subPackets[0].evaluate() > subPackets[1].evaluate();
			case 6:
				return subPackets[0].evaluate() < subPackets[1].evaluate();
			case 7:
				return subPackets[0].evaluate() == subPackets[1].evaluate();
		}
		return result;
	}
};

struct BinaryInput{
	//vector<int> bits;
	bitset<5400> bits;
	size_t pos;
	uint32_t take(uint8_t n){
		uint32_t res = 0;
		for(int i = 0; i < n; i++)
			res = (res<<1) | bits[pos++];
		return res;
	}
};

string HexStrToBinStr(const string& s) {
  unordered_map<char, string> m {
    {'0', "0000"},
    {'1', "0001"},
    {'2', "0010"},
    {'3', "0011"},
    {'4', "0100"},
    {'5', "0101"},
    {'6', "0110"},
    {'7', "0111"},
    {'8', "1000"},
    {'9', "1001"},
    {'A', "1010"},
    {'B', "1011"},
    {'C', "1100"},
    {'D', "1101"},
    {'E', "1110"},
    {'F', "1111"}
  };

  string r;
  for (const char c : s) {
    r += m[c];
  }
  return r;
}

Packet convertToPacket(BinaryInput& input){
	Packet p {};
	auto ver = input.take(3);
	p.version = ver; 
	p.type = input.take(3);
	if(p.type == 0b100){
		uint8_t extNibble = input.take(5);
		int64_t val = extNibble & 0b1111;
		while(extNibble & 0b10000){
			extNibble = input.take(5);
			val = (val << 4) | (extNibble & 0b1111);
		}
		p.val = val;
	} else {
		p.id = input.take(1);
		if(p.id == 0){
			p.len = input.take(15);
			auto endPos = input.pos + p.len;
			while(input.pos < endPos){
				Packet subP = convertToPacket(input);
				p.subPackets.push_back(subP);
			}
		} else {
			p.len = input.take(11);
			for(int i = 0; i < p.len; i++){
				Packet subP = convertToPacket(input);
				p.subPackets.push_back(subP);
			}
		}
	}

	return p;
}

int main(){
	auto t1 = high_resolution_clock::now();
	ifstream inp("input.txt");
	string hexLine;
	getline(inp,hexLine);
	string binStr = HexStrToBinStr(hexLine);
	//vector<int> bits;
	//for (auto c:binStr) bits.push_back(c-'0');
	reverse(binStr.begin(), binStr.end());
	BinaryInput input{bitset<5400>(binStr),0};
	Packet packets = convertToPacket(input);
	cout << packets.versionTotal() << endl;
	cout << packets.evaluate() << endl;
	auto t2 = high_resolution_clock::now();
	auto duration = duration_cast<microseconds>(t2 - t1);
	cout << duration.count() << " µs" << endl;
}
