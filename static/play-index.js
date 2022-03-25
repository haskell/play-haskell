"use strict";

function performXHR(method, path, responseType, mcontentType, mdata, successcb, failcb) {
	var haveData;
	if (arguments.length == 5) {
		successcb = mcontentType;
		failcb = mdata;
		haveData = false;
	} else if (arguments.length == 7) {
		haveData = true;
	} else {
		throw new Error("Invalid usage of performXHR");
	}

	var xhr = new XMLHttpRequest();

	xhr.onreadystatechange = function(){
		if (xhr.readyState == 4) {
			if (xhr.status == 200) {
				if (responseType == "text") successcb(xhr.responseText);
				else if (responseType == "json") {
					var obj;
					try {
						obj = JSON.parse(xhr.responseText);
					} catch (e) {
						failcb(xhr);
					}
					successcb(obj);
				} else {
					throw new Error("Invalid responseType in performXHR");
				}
			} else failcb(xhr);
		}
	};

	xhr.open(method, path);
	xhr.responseType = "text";
	if (haveData) xhr.setRequestHeader("Content-Type", mcontentType);
	if (haveData) xhr.send(mdata); else xhr.send();
}

function setWorking(yes) {
	var elt = document.getElementById("btn-run");
	if (yes) elt.setAttribute("disabled", ""); else elt.removeAttribute("disabled");

	elt = document.getElementById("rightcol");
	if (yes) elt.classList.add("greytext");
	else elt.classList.remove("greytext");
}

function getVersions(cb) {
	performXHR("GET", "/play/versions", "json", cb, function(xhr) {
		alert("Error getting available compiler versions (status " + xhr.status + "): " + xhr.responseText);
	});
}

function sendRun(source, version, cb) {
	var payload = JSON.stringify({source, version});
	setWorking(true);
	performXHR("POST", "/play/run", "json", "text/plain", payload,
		function(res) {
			setWorking(false);
			cb(res);
		}, function(xhr) {
			setWorking(false);
			alert("Failed to submit run job (status " + xhr.status + "): " + xhr.responseText);
		}
	);
}

function doRun() {
	var source = document.getElementById("code-ta").value;
	var version = document.getElementById("ghcversionselect").value;
	if (typeof version != "string" || version == "") version = "8.10.7";

	sendRun(source, version, function(response) {
		var ecNote = document.getElementById("exitcode-note");
		if (response.ec != 0) {
			ecNote.classList.remove("invisible");
			ecNote.textContent = "Command exited with code " + response.ec + ".";
		} else {
			ecNote.classList.add("invisible");
		}

		document.getElementById("out-stdout").textContent = response.out;
		document.getElementById("out-stderr").textContent = response.err;
	});
}

window.addEventListener("load", function() {
	document.getElementById("code-ta").addEventListener("keypress", function(ev) {
		if ((ev.key == "Enter" || ev.keyCode == 13) && ev.ctrlKey) {
			doRun();
		}
	});

	getVersions(function(versions) {
		var sel = document.getElementById("ghcversionselect");
		for (var i = 0; i < versions.length; i++) {
			var opt = document.createElement("option");
			opt.value = versions[i];
			opt.textContent = "GHC " + versions[i];
			if (versions[i] == "8.10.7") opt.setAttribute("selected", "");
			sel.appendChild(opt);
		}
	});
});
