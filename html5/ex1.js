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
	context.fillStyle = '#000000';
	context.strokeStyle = '#ff00ff';
	context.lineWidth = 2;
	context.fillRect(10,10,40,40);
	context.strokeRect(0,0,60,60);
	context.clearRect(20,20,20,20);

	context.strokeStyle='black';
	context.lineWidth=10;
	context.lineCap='round';
	context.lineJoin='miter';
	context.beginPath();
	context.moveTo(100,100);
	context.lineTo(60,60);
	//context.moveTo(150,200);
	context.lineTo(150,200);
	context.stroke();
//	context.closePath();
	

	
    }

    function canvasSupport() {
	return !!document.createElement("testCanvas").getContext;
    }

    drawScreen();
}


