window.onload = function() {
    canvasApp();
}

function canvasApp() {


    function drawScreen() {
	var theCanvas = document.getElementById("canvasOne");
	if (!canvasSupport) {
	    alert("Could not get the canvas!");
	    return;
	}

	var context = theCanvas.getContext("2d");
	context.fillStyle = 'black';
	context.fillRect(0,0,200,200);

	context.fillStyle='red';
	context.fillRect(1,1,50,50);

	var x=actionList[0];
	x("hello");
//	setInterval(animator,1000);
	

	
    }
    var ctr=0;
    function animator(){
	actionList[ctr](ctr);
	ctr++;
	if(ctr>1)ctr=0;
//	setInterval(animator, 1000);
    }
    var actionList = [
	function (message) {
	    alert(message);
	},
	function (message) {
	    alert(message);
	}
    ];

    function canvasSupport() {
	return !!document.createElement("testCanvas").getContext;
    }

    drawScreen();
}


