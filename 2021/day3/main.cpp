#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <algorithm>

using namespace std;

void splitHighLow(vector<uint32_t>numbers, int bitPos, vector<uint32_t> *high, vector<uint32_t> *low){
	for(auto number: numbers){
		if((number >> bitPos)%2)
			high->push_back(number);
		else
			low->push_back(number);
	}
}

int getMostCommonBitAtPos(vector<uint32_t> numbers, int bitPos){
	vector<uint32_t> hi,lo;
	splitHighLow(numbers,bitPos, &hi, &lo);
	if (hi.size() > lo.size())
		return 1;
	return 0;
}

vector<uint32_t> reverseBinary(vector<uint32_t>numbers){
	vector<uint32_t> rev;
	for (auto num:numbers){
		int bits = 32;
		uint32_t reversed = 0;
		while (num>0){
			reversed <<=1;
			if (num&1 == 1)
				reversed ^= 1;
			num >>= 1;
			bits--;
		}
		reversed <<= bits; //pad to uint32_t if high bits are low
		rev.push_back(reversed);
	}
	return rev;

}

void selectNums(vector<uint32_t> inNums, int bitPos, bool selectHi, vector<uint32_t> *outNums){
	vector<uint32_t> hi,lo;
	splitHighLow(move(inNums), bitPos, &hi, &lo);
	if(hi.size() >= lo.size())
		*outNums = ((selectHi) ? move(hi) : move(lo));
	else
		*outNums = ((selectHi) ? move(lo) : move(hi));
}
	

int main(){
	ifstream f("input.txt");
	string l;
	vector<uint32_t> nums;
	if(f.is_open()){
		while(getline(f,l)){
			nums.push_back(stoi(l,0,2));
		}
		f.close();
	}
	int gamma = 0;
	int epsilon = 0;
	for (int i = 0; i<12;i++){
		int mostCommonBit = getMostCommonBitAtPos(nums, i);
		gamma += mostCommonBit << i;
		if (!mostCommonBit)
			epsilon += 1 << i;
	}
	cout << gamma * epsilon << endl;

	vector<uint32_t>numsRev = reverseBinary(nums);

	for_each(numsRev.begin(),numsRev.end(),[](uint32_t &n){n = n >> 20;});
	vector<uint32_t>o2 = numsRev, co2 = numsRev;

	int bitPos = 0;
	while (o2.size() != 1){
		selectNums(o2, bitPos++, true, &o2);
	}

	bitPos = 0;
	while (co2.size() != 1){
		selectNums(co2, bitPos++, false, &co2);
	}

	cout << (reverseBinary(o2)[0] >> 20) * (reverseBinary(co2)[0] >> 20) << endl;
}
