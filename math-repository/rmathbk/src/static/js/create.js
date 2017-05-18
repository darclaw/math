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
