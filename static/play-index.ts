import {EditorState, EditorView, basicSetup} from "@codemirror/basic-setup";
import {javascript} from "@codemirror/lang-javascript";

let state = EditorState.create({doc: `main :: IO ()
main = do
  let unsorted = [10,9..1]
  putStrLn $ show $ quicksort unsorted

quicksort :: Ord a => [a] -> [a]
quicksort []     = []
quicksort (x:xs) = let lesser  = filter (< x) xs
                       greater = filter (> x) xs
                   in quicksort lesser ++ [x] ++ quicksort greater

`, extensions: [
  basicSetup,
  javascript(),
]});
(window as any).view = new EditorView({state, parent: document.querySelector("#editor")!});

type json =
| string
| number
| boolean
| null
| json[]
| {[key: string]: json}


function performXHR(method: string, path: string, responseType: string, successcb: (response: json) => void, failcb: (xhr: XMLHttpRequest) => void, mcontentType?: string, mdata?: string) {
	const xhr: XMLHttpRequest = new XMLHttpRequest();

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
	if (mcontentType) xhr.setRequestHeader("Content-Type", mcontentType);
	if (mdata) xhr.send(mdata); else xhr.send();
}

function setWorking(yes: boolean) {
	let elt: HTMLElement = document.getElementById("btn-run");
	if (yes) elt.setAttribute("disabled", ""); else elt.removeAttribute("disabled");

	elt = document.getElementById("rightcol");
	if (yes) elt.classList.add("greytext");
	else elt.classList.remove("greytext");
}

function getVersions(cb: (response: string) => void) {
	performXHR("GET", "/play/versions", "json", cb, function(xhr) {
		alert("Error getting available compiler versions (status " + xhr.status + "): " + xhr.responseText);
	});
}

function sendRun(source: string, version: string, cb: (response: json) => void) {
	const payload: string = JSON.stringify({source, version});
	setWorking(true);
	performXHR("POST", "/play/run", "json",
		function(res: json) {
			setWorking(false);
			cb(res);
		}, function(xhr) {
			setWorking(false);
			alert("Failed to submit run job (status " + xhr.status + "): " + xhr.responseText);
		}, "text/plain", payload
	);
}

function doRun() {
	const source: string = (window as any).view.state.doc.toString();
	let version = (document.getElementById("ghcversionselect") as any).value;
	if (typeof version != "string" || version == "") version = "8.10.7";

	sendRun(source, version, function(response: {[key: string]: json}) {
		const ecNote: HTMLElement = document.getElementById("exitcode-note");
		if (response.ec != 0) {
			ecNote.classList.remove("invisible");
			ecNote.textContent = "Command exited with code " + response.ec + ".";
		} else {
			ecNote.classList.add("invisible");
		}

		document.getElementById("out-stdout").textContent = response.out as string;
		document.getElementById("out-stderr").textContent = response.err as string;
	});
}

window.addEventListener("load", function() {
	document.getElementById("editor").addEventListener("keypress", function(ev) {
		if ((ev.key == "Enter" || ev.keyCode == 13) && ev.ctrlKey) {
			doRun();
		}
	});

	getVersions(function(versions) {
		const sel: HTMLElement = document.getElementById("ghcversionselect");
		for (let i = 0; i < versions.length; i++) {
			const opt: HTMLOptionElement = document.createElement("option");
			opt.value = versions[i];
			opt.textContent = "GHC " + versions[i];
			if (versions[i] == "8.10.7") opt.setAttribute("selected", "");
			sel.appendChild(opt);
		}
	});
});

document.getElementById("btn-run").addEventListener('click', doRun);
