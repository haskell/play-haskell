"use strict";

function sendRun(source, cb) {
	var xhr = new XMLHttpRequest();

	xhr.onreadystatechange = function(){
		if (xhr.readyState == 4) {
			if (xhr.status == 200) {
				var obj;
				try {
					obj = JSON.parse(xhr.responseText);
				} catch(e) {
					alert("Invalid data received from server");
					return;
				}
				cb(obj)
			} else {
				alert("Failed to submit run job (status " + xhr.status + "): " + xhr.responseText);
			}
		}
	};

	xhr.open("POST", "/play/run");
	xhr.responseType = "text";
	xhr.setRequestHeader("Content-Type", "text/plain");
	xhr.send(source);
}

function doRun() {
	var source = document.getElementById("code-ta").value;
	sendRun(source, function(response) {
		alert(response);
	});
}
