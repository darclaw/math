mod=require("mathjs").mod;
const universeSize=2;

var firstCombs=[];
//set the size
firstCombs[Math.pow(universeSize,(universeSize*universeSize))-1]=-1;

var secondCombs=[];
//set the size
secondCombs[Math.pow(universeSize,(universeSize*universeSize))-1]=-1;

//var tA=[[0,1,2],[0,1,2],[0,1,2]]; //for uSize=3
var tA=[[0,1],[0,1]];
//tA.toString=function(){return "[[0,1],[0,1]]"};
tA.toString=function(){return "tA"};
var tB=[[0,0],[1,1]];
//tB.toString=function(){return "[[0,0],[1,1]]"};
tB.toString=function(){return "tB"};
//var tB=[[0,0,0],[1,1,1],[2,2,2]];   //for uSize=3

//how many times a unique combination is found
var uniqueIterations=0;

//(function funcname(){})() executes the function imediatly (iife)
(function main(){
	//table=[[0,1,2],[0,0,0],[0,0,0]];
	
	//console.log(truthTableToInt(table));

	//console.log(op(tA,tB));
	
	testTables(tA,tB,"op("+tA+","+tB+")");
	console.log(firstCombs);
	console.log(secondCombs);
})();

//number is made in reverse of reading order,
//ex: [[1,0],[1,1]] is 1*2^0+1*2^2+1*2^3=13
function truthTableToInt(tTable){
	num=0;
	for(i1=0;i1<universeSize;i1++){
		for(i2=0;i2<universeSize;i2++){
			num += tTable[i1][i2]*(Math.pow(universeSize,(i1*universeSize+i2)));
		}
	}
	return num;
}

//tests the table created with the tables t1,t2 and then recurses
function testTables(t1,t2,tableName){
	var newTable=op(t1,t2);
	console.log(newTable);
	//pseudo
	//check the newly created "operator" with default inputs, call it newTable
	//if newTable does not exist in firstCombs then put it in there
	//if newTable exists in firstCombs, put it in secondCombs and return
	var tableNum=truthTableToInt(newTable);
	console.log(tableNum);
	if(firstCombs[tableNum]){
		console.log("if firstCombs");
		if(secondCombs[tableNum]){
			console.log("if firstCombs if secomdCombs");
			return;
		}else{
			console.log("if firstCombs if !secondCombs");
			secondCombs[tableNum]=tableName;
			return;
		}
	}else{
		firstCombs[tableNum]=tableName;
		uniqueIterations++;
		console.log("if !firstCombs");
	}

	console.log(uniqueIterations);
	if(uniqueIterations<=Math.pow(universeSize,universeSize*universeSize)){
		console.log("doing recursion");
		//should be enough tests because it is symetric
		testTables(tA,newTable,"op("+tA+","+tableName+")");
		testTables(tB,newTable,"op("+tB+","+tableName+")");
		testTables(newTable,newTable,"op("+tableName+","+tableName+")");
	}else{
		return;
	}
}

//if t1[a][b]==t2[a][b]
//	table[a][b]=(t1[a][b]-1 )% uSize
//else 
//	table[a][b]=0
function op(t1,t2){
	table=[];
	for(i1=0;i1<universeSize;i1++){
		table[i1]=[];
		for(i2=0;i2<universeSize;i2++){
			if(t1[i1][i2]==t2[i1][i2]){
				table[i1][i2]=mod((t1[i1][i2]-1),universeSize);
			}else{
				table[i2][i2]=0;
			}
		}
	}
	return table;
}

function intToTruthTable(num){
}


