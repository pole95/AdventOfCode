#include <iostream>
#include <fstream>
#include <chrono>
#include <nlohmann/json.hpp>

using namespace std;
using namespace std::chrono;

//NOT WORKING SEE PYTHON

struct Node{
	int64_t value = -1;
	Node* left = nullptr;
	Node* right = nullptr;
	Node* parent =  nullptr;
	bool isLeaf = false;


	Node(int64_t value): value(value), isLeaf(true){};
	Node(Node* left, Node* right): left(left), right(right){
		left->parent = this;
		right->parent = this;
	};

	Node* getNeighbourRight(){
		Node* p = parent;
		Node* n = this;
		while (p != nullptr){
			if(p->left == n){
				Node* childL = p->right;
				while(childL != nullptr){
					if(childL->isLeaf)
						return childL;
					else
						childL = childL->left;
				}
			} else {
				n = p;
				p = p->parent;
			}
		}
		return nullptr;
	}

	Node* getNeighbourLeft(){
		Node* p = parent;
		Node* n = this;
		while (p != nullptr){
			if(p->right == n){
				Node* childR = p->left;
				while(childR != nullptr){
					if(childR->isLeaf)
						return childR;
					else
						childR = childR->right;
				}
			} else {
				n = p;
				p = p->parent;
			}
		}
		return nullptr;
	}

	int64_t magnitude(){
		if(isLeaf)
			return value;
		else
			return 3 * left->magnitude() + 2 * right->magnitude();
	}

};
void prettyPrint(Node *n){
	if(n->isLeaf){
		printf("%ld",n->value);
	} else {
		printf("[");
		prettyPrint(n->left);
		printf(",");
		prettyPrint(n->right);
		printf("]");
	}
}
ostream& operator<<(ostream& os, Node& n){
	if(n.isLeaf){
		os << n.value;
	} else {
		os << "[" << *n.left << ","  << *n.right <<"]";
	}
	return os;
}

void split(Node* n){
	Node* left = new Node(n->value/2);
	Node* right = new Node(n->value - left->value);
	Node* newNode = new Node(left, right);
	newNode->parent = n->parent;
	if(n->parent->right == n){
		n->parent->right == newNode;
	} else if (n->parent->left == n){
		n->parent->left == newNode;
	}
	delete n;
}

void explode(Node* n){
	Node* left = n->getNeighbourLeft();
	Node* right = n->getNeighbourRight();

	if (left)
		left->value = left->value + n->left->value;
	if (right)
		right->value = right->value + n->right->value;

	Node* newLeaf = new Node(0);
	newLeaf->parent = n->parent;
	if (n->parent->left == n)
		n->parent->left == newLeaf;
	if (n->parent->right == n)
		n->parent->right == newLeaf;

	delete n;
}

Node* getExploding(Node* n, int depth=0){
	if (depth >= 4 && n->left->isLeaf && n->right->isLeaf){
		return n;
	}
	Node* explLeft = getExploding(n->left, depth+1);
	if(explLeft){
		return explLeft;
	}
	Node* explRight = getExploding(n->right, depth+1);
	if(explRight){
		return explRight;
	}
	
	return nullptr;
}

Node* getSplitting(Node* n){
	if (n->isLeaf){
		if (n->value > 9)
			return n;
		else
			return nullptr;
	} else{
		Node* left = getSplitting(n->left);
		if(left)
			return left;
		Node* right = getSplitting(n->right);
		if(right)
			return right;
	}
	return nullptr;
}

Node* reduce(Node* n){
	bool o = true;
	while(o){
		o = false;
		Node* explodingNode = getExploding(n);
		if(explodingNode){
			explode(explodingNode);
			o = true;
			continue;
		}
		Node* splittingNode = getSplitting(n);
		if(splittingNode){
			split(splittingNode);
			o = true;
			continue;
		}
	}
	return n;
}

Node* treeFromJson(nlohmann::json line){
	if(line.is_array()){
		Node* left = treeFromJson(line[0]);
		Node* right = treeFromJson(line[1]);
		return new Node(left, right);
	} else {
		return new Node(line.get<int64_t>());
	}
}

Node* add(Node* n1, Node* n2){
	Node* n = new Node(n1, n2);
	return n;
}

int main(){
	auto t1 = high_resolution_clock::now();
	ifstream inp("input.txt");
	string line;
	getline(inp,line);
	nlohmann::json jline = nlohmann::json::parse(line);
	Node* num = treeFromJson(jline);
	while(getline(inp,line)){
		jline = nlohmann::json::parse(line);
		Node* newNum = treeFromJson(jline);
		num = add(num,newNum);
		cout << *num << endl;
		reduce(num);

	}
	cout << *num << endl;
	cout << num->magnitude() << endl;
	auto t2 = high_resolution_clock::now();
	auto duration = duration_cast<microseconds>(t2 - t1);
	cout << duration.count() << " µs" << endl;
}
