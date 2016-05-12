#include <string>
#include <math.h>
#include <iostream>

using namespace::std;

//protypes
int truthTableToInt(int**);

const int universeSize=3;

int main(){
	string comb1[int(pow(universeSize,universeSize*universeSize))];
	string comb2[int(pow(universeSize,universeSize*universeSize))];
	
	int table[3][3]={{0,1,2},{1,2,0},{2,0,1}};
	cout << truthTableToInt(table);
}

int truthTableToInt(int** tTable){
	int num=0;
	for(int i1=0;i1<universeSize;i1++){
		for(int i2=0;i2<universeSize;i2++){
			num += tTable[i1][i2]*pow(universeSize,i1*universeSize+i2);
		}
	}
	return num;
}

