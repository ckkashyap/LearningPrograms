var theCanvas;
var theContext;
window.onload = function() {
    canvasApp();
}

function canvasApp() {
    function drawScreen() {
	theCanvas = document.getElementById("canvasOne");
	theContext = theCanvas.getContext("2d");
	if (!canvasSupport) {
	    alert("Could not get the canvas!");
	    return;
	}

	setInterval(animator, 100);
    }

    var ctr=0;
    function animator(){
	theContext.clearRect(0,0,500,500);
	theCanvas.width=theCanvas.width;
	actionList[ctr](theContext);
	theContext.stroke();
	ctr++;
	if(ctr==actionList.length)ctr=0;
	
    }

    function canvasSupport() {
	return !!document.createElement("testCanvas").getContext;
    }

    drawScreen();
}



