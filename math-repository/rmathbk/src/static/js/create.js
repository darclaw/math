var elems = 
   [ // list of graph elements to start with
      { // node a
         data: { id: 'a', name:'a' }
      },
      { // node b
         data: { id: 'b', name:'b' }
      },
      { // edge ab
         data: { id: 'ab', source: 'a', target: 'b', name:'ab' }
      }
   ];
var cy = cytoscape({

   container: document.getElementById('cy'), // container to render in

   elements: elems,

   style: [ // the stylesheet for the graph
      {
         selector: 'node',
         style: {
            'background-color': '#666',
            'label': 'data(id)',
            'content': 'data(name)'
         }
      },

      {
         selector: 'edge',
         style: {
            'width': 3,
            'line-color': '#ccc',
            'target-arrow-color': '#ccc',
            'target-arrow-shape': 'triangle',
            'curve-style': 'bezier'
         }
      }
   ],

   layout: {
      name: 'cose',
      directed: true
   }

});

var selectAllOfTheSameType = function(ele) {
   cy.elements().unselect();
   if(ele.isNode()) {
      cy.nodes().select();
   }
   else if(ele.isEdge()) {
      cy.edges().select();
   }
};

cy.contextMenus({
   menuItems: [
      {
         id: 'remove',
         title: 'remove',
         selector: 'node, edge',
         onClickFunction: function (event) {
            var target = event.target || event.cyTarget;
            target.remove();
         },
         hasTrailingDivider: true
      },
      {
         id: 'hide',
         title: 'hide',
         selector: '*',
         onClickFunction: function (event) {
            var target = event.target || event.cyTarget;
            target.hide();
         },
         disabled: false
      },
      {
         id: 'rename',
         title: 'rename',
         selector: 'node, edge',
         onClickFunction: function (event) {
            var target = event.target || event.cyTarget;
            target.data({name:""});
            var xTriggered = 0;
            $("body").on("keypress.rename", function( event ) {
               if ( event.which == 13 ) {
                  event.preventDefault();
               }
               xTriggered++;
               var msg = "Handler for .keypress() called " + xTriggered + " time(s).";
               if(event.key == "Enter"){
                  $("body").off("keypress.rename");
                  $("body").off("keydown.rename");
               } else {
                  target.data({name:(target.data().name+event.key)});
               }
               //console.log( msg, "html" );
               console.log( event );
            });
            $("body").on("keydown.rename", function(event){
               if ( event.which == 13 ) {
                  event.preventDefault();
               }
               if(event.key == "Enter"){
                  $("body").off("keypress.rename");
                  $("body").off("keydown.rename");
               } else if(event.key == "Backspace"){
                  target.data({name: target.data().name.slice(0, -1)});
               }
               console.log( event );
            });
         }
      },
      {
         id: 'add-node',
         title: 'add node',
         coreAsWell: true,
         onClickFunction: function (event) {
            var data = {
               group: 'nodes'
            };

            var pos = event.position || event.cyPosition;

            cy.add({
               data: data,
               position: {
                  x: pos.x,
                  y: pos.y
               }
            });
         }
      },
      {
         id: 'remove-selected',
         title: 'remove selected',
         coreAsWell: true,
         onClickFunction: function (event) {
            cy.$(':selected').remove();
         }
      },
      {
         id: 'select-all-nodes',
         title: 'select all nodes',
         selector: 'node',
         onClickFunction: function (event) {
            selectAllOfTheSameType(event.target || event.cyTarget);
         }
      },
      {
         id: 'select-all-edges',
         title: 'select all edges',
         selector: 'edge',
         onClickFunction: function (event) {
            selectAllOfTheSameType(event.target || event.cyTarget);
         }
      }
   ]
});

var addNodeLetter = "b";
var addNodeLetterPos = {x:200,y:200};
$("#addNode").click(function addNode(){
   $("#cy").on("click.placeNode", function placeNode(event){
      clickPos = {x:event.clientX, y: event.clientY-100};
      let oldNodeLetter = addNodeLetter;
      //let oldNodeLetter = addNodeLetterPos;
      addNodeLetter = String.fromCharCode(addNodeLetter.charCodeAt(0) + 1);
      //addNodeLetterPos = {x: addNodeLetterPos.x , y: addNodeLetterPos.y};
      cy.add({
         data:{
            id:addNodeLetter, 
            name:addNodeLetter,
            position:clickPos,
            weight: 50
         },
         renderedPosition: clickPos
      });
      //addNodeLetterPos = {x:addNodeLetter.x+10,y:addNodeLetterPos.y};
      //cy.add({data:{id:addNodeLetter+oldNodeLetter, source:oldNodeLetter, target:addNodeLetter, weight: 1}});
      $("#cy").off("click.placeNode");
   });
});

function createEdgeCurried(node1){
   return function(node2){
      cy.add({data:{id:node1.id()+node2.id(), source:node1.id(), target:node2.id()}});
   }
}

cy.on('tap', 'node', function tap1(evt){
   var node = evt.target;
   console.log( 'tapped ' + node.id() );
   selSecNode = createEdgeCurried(node);
   cy.off('tap', 'node', tap1);
   cy.on('tap', 'node', function tap2(event2){
      selSecNode(event2.target);
      cy.off('tap','node', tap2);
      cy.on('tap', 'node', tap1);
   });
});

$("#data_add").click(function(evt){
   $("#data_add").after("<div><input type='text' placeholder='key' name='key'><input type='text' name='value_type' placeholder='value type' class='data_type_value'></input><input type='text' placeholder='value' name='value' class='data_add_value'><button class='remove_data_add'>&#10006</button></div>");
   bind_removing();
   bind_type_change();
   function bind_removing(){
      $(".remove_data_add").click(function(evt){
         evt.target.parentNode.remove();
      });
   }
   var level = 0;
   function bind_type_change(){
      level++;
      $(".data_type_value").focusout(function(evt){
         if(evt.target.value != "multi"){
            $(evt.target).siblings(".data_add_value").prop('type', evt.target.value);
         } else {
            //add making multiple entries
            $(evt.target).siblings(".data_add_value").replaceWith("<div><input type='text' placeholder='"+level+"key' name='key'><input type='text' name='value_type' placeholder='"+level+" value type' class='data_type_value'></input><input type='text' placeholder='"+level+" value' name='value' class='data_add_value'><button class='remove_data_add'>&#10006</button></div>");
            bind_type_change();
            bind_removing();
         }
      });
   }
});

$("#submit").click(function submit(evt){
   let data = [];
   let vals = [];
   let val_ts = [];
   let keys = [];
   $("#info > div").each(function getInfo(index, elm){
      let inf = {};
      inf.key        = $(elm).children("[name = 'key']").val();
      inf.value_type = $(elm).children("[name = 'value_type']").val();
      inf.value      = $(elm).children("[name = 'value']").val();
      data.push(inf);

      vals.push(inf.value);
      val_ts.push(inf.value_type);
      keys.push(inf.key);
   });
   console.log(JSON.stringify(data));
   
   let adjMat = [];
   let nameToNum = {};
   for(let i=0;i<cy.nodes().length;i++){
      adjMat[i]=[];
      for(let i2=0;i2<cy.nodes().length;i2++){
         adjMat[i][i2]=0;
      }
   }
   cy.nodes().forEach(function popNameToNum(node,i){
      nameToNum[node.data().id]=i;
   });
   console.log("create/submit "+JSON.stringify(nameToNum));
   cy.nodes().forEach(function getAdjecent(node,i){
      //adjMat[nameToNum[node.data().id]]=[]; //[node.data().id]={};
      node.outgoers().forEach(function (anode){
         console.log("create/submit/getAdjecent "+JSON.stringify(anode.data().target));
         console.log("create/submit/getAdjecent "+JSON.stringify(adjMat[nameToNum[i]]));
         //adjMatLet[node.data().id][anode.data().target]=1;
         if(anode.data().target){
            adjMat[nameToNum[node.data().id]][nameToNum[anode.data().target]]=1;
         }
      });
   });
   console.log(JSON.stringify(adjMat));

   $.post("/add/graph", {graph:cy.json(), data:data, ndata:data.length, vals:vals, val_ts:val_ts, keys:keys, adjMat:adjMat, adjMatSize:adjMat.length});
});

