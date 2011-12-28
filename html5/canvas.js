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
	
	context.fillStyle = '#ffffaa';
	context.fillRect(0,0,500,300);
	context.fillStyle = '#000000';
	context.font="20px _sans";
	context.textBaseline="top";
	context.fillText("Hello World", 195, 80);

	context.strokeStyle='#000000';
	context.strokeRect(5,5,490,290);

	
    }

    function canvasSupport() {
	return !!document.createElement("testCanvas").getContext;
    }

    drawScreen();
}


