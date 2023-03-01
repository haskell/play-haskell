function addMediaListener(querystr: string, fallbackevent: string | null, cb: (q: MediaQueryList | MediaQueryListEvent | null) => void) {
	if ("matchMedia" in window) {
		const queryList = window.matchMedia(querystr);
		// Invoke the callback once to get the current media setting
		cb(queryList);
		// Apparently older Safari doesn't have addEventListener here yet
		// (Note that we need to cast to 'any' here to convince Typescript that
		// no, not every browser has addEventListener on this object)
		if ("addEventListener" in queryList) queryList.addEventListener("change", cb);
		else if ("addListener" in (queryList as any)) (queryList as any).addListener(function() { cb(window.matchMedia(querystr)); });
	} else if (fallbackevent != null) {
		document.addEventListener(fallbackevent, function() { cb(null); });
	}
}

const example_snippets: string[] = [
	`import Data.List (partition)

main :: IO ()
main = do
  let unsorted = [10,9..1]
  putStrLn $ show $ quicksort unsorted

quicksort :: Ord a => [a] -> [a]
quicksort []     = []
quicksort (x:xs) = let (lesser, greater) = partition (<= x) xs
                   in quicksort lesser ++ [x] ++ quicksort greater`,
	`import Data.Bifunctor (first, second)

main :: IO ()
main = do
  let unsorted = [10,9..1]
  putStrLn $ show $ mergesort unsorted

mergesort :: Ord a => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort l =
  let distribute l1 l2 []     = (l1, l2)
      distribute l1 l2 (x:xs) = distribute l2 (x:l1) xs
      (l1, l2) = distribute [] [] l
  in mergesort l1 \`merge\` mergesort l2

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys         = ys
merge xs []         = xs
merge (x:xs) (y:ys)
  | x <= y          = x : merge xs (y:ys)
  | otherwise       = y : merge (x:xs) ys`,
	`main :: IO ()
main = do
  let unsorted = [10,9..1]
  putStrLn $ show $ mergesort unsorted

mergesort :: Ord a => [a] -> [a]
mergesort = mergeUp . map pure
  where
    mergeUp :: Ord a => [[a]] -> [a]
    mergeUp []      = []
    mergeUp [chunk] = chunk
    mergeUp chunks  = mergeUp (map (uncurry merge) (pairs chunks))

    pairs :: Monoid a => [a] -> [(a, a)]
    pairs []        = []
    pairs [x]       = [(x, mempty)]
    pairs (x:y:xs)  = (x, y) : pairs xs

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys         = ys
merge xs []         = xs
merge (x:xs) (y:ys)
  | x <= y          = x : merge xs (y:ys)
  | otherwise       = y : merge (x:xs) ys`
];

// If a version is not present in this dictionary, just display it as-is
const ghcReadableVersion: Record<string, string> = {
	"9.6.0.20230111": "9.6.1-alpha1",
	"9.6.0.20230128": "9.6.1-alpha2",
	"9.6.0.20230210": "9.6.1-alpha3",
};

// defined in a <script> block in play.mustache
declare var preload_script: string | null;
declare var preload_ghc_version: string | null;
const snippet = preload_script != null ? preload_script : example_snippets[Math.floor(Math.random() * example_snippets.length)];


// defined in ace-files/ace.js with a <script src> block in play.mustache
declare var ace: any;

ace.config.set("basePath", location.origin + "/ace-files");
const editor = ace.edit("leftcol");
editor.setOptions({
	printMargin: false,
	fontSize: "large",
	useSoftTabs: true,
	mode: "ace/mode/haskell",
});
editor.session.setValue(snippet);
editor.session.setOptions({
	tabSize: 2,
})

addMediaListener("(prefers-color-scheme: dark)", null, function(ql) {
	if (ql.matches) editor.setTheme("ace/theme/monokai");
	else editor.setTheme("ace/theme/github");
});

type json =
	| string
	| number
	| boolean
	| null
	| json[]
	| {[key: string]: json};

type Runner = "run" | "core" | "asm";

let lastRunKind: Runner = "run";

const defaultGHCversion: string = "9.2.7";


function performXHR(
	method: string,
	path: string,
	responseType: string,
	successcb: (response: json) => void,
	failcb: (xhr: XMLHttpRequest) => void,
	mcontentType?: string,
	mdata?: string)
{
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
	const btns: Element[] = Array.from(document.getElementsByClassName("button"));
	const rightcol: HTMLElement = document.getElementById("rightcol");
	if (yes) {
		btns.forEach(btn => {
			btn.setAttribute("disabled", "");
		});
		document.getElementById("main").classList.add("loading");
	} else {
		btns.forEach(btn => {
			btn.removeAttribute("disabled");
		});
		document.getElementById("main").classList.remove("loading");
	}
}

function getVersions(cb: (response: string) => void) {
	performXHR("GET", "/versions", "json", cb, function(xhr) {
		alert("Error getting available compiler versions (status " + xhr.status + "): " + xhr.responseText);
	});
}

function sendRun(source: string, version: string, opt: string, run: Runner, cb: (response: json) => void) {
	const payload: string = JSON.stringify({code: source, version, opt, output: run});
	setWorking(true);
	performXHR("POST", "/submit", "json",
		function(res: json) {
			setWorking(false);
			cb(res);
		}, function(xhr) {
			setWorking(false);
			alert("Failed to submit run job (status " + xhr.status + "): " + xhr.responseText);
		}, "text/plain", payload
	);
}

function doRun(run: Runner) {
	lastRunKind = run;

	const source: string = editor.getValue();
	let version = (document.getElementById("ghcversionselect") as any).value;
	let opt = (document.getElementById("optselect") as any).value;
	if (typeof version != "string" || version == "") version = defaultGHCversion;
	if (typeof opt != "string" || opt == "") opt = "O1";

	sendRun(source, version, opt, run, function(response: {[key: string]: json}) {
		function setInvisible(elem, yes) {
			if (yes) elem.classList.add("invisible");
			else elem.classList.remove("invisible");
		}

		if (!response.ghcout) response.ghcout = "";
		if (!response.sout) response.sout = "";
		if (!response.serr) response.serr = "";

		const ecNote: HTMLElement = document.getElementById("exitcode-note");
		ecNote.classList.remove("initial");
		if (response.err != null) {
			setInvisible(ecNote, false);
			switch (response.err) {
				case "timeout": ecNote.textContent = "Command timed out"; break;
				case "backend": ecNote.textContent = "The server encountered an error processing your request; please try again later"; break;
				default: ecNote.textContent = "Unknown error? (" + response.err + ")"; break;
			}
		} else if (response.ec != 0) {
			setInvisible(ecNote, false);
			ecNote.textContent = "Command exited with code " + response.ec + ".";
		} else if (response.ghcout == "" && response.sout == "" && response.serr == "") {
			setInvisible(ecNote, false);
			ecNote.textContent = "No output.";
		} else {
			setInvisible(ecNote, true);
		}

		setInvisible(document.getElementById("out-container-ghcout"), (response.ghcout as string).length == 0);
		setInvisible(document.getElementById("out-container-stdout"), (response.sout as string).length == 0);
		setInvisible(document.getElementById("out-container-stderr"), (response.serr as string).length == 0);

		if (response.ghcout) document.getElementById("out-ghcout").textContent = response.ghcout as string;
		if (response.sout) document.getElementById("out-stdout").textContent = response.sout as string;
		if (response.serr) document.getElementById("out-stderr").textContent = response.serr as string;
	});
}

function doSave() {
	const source: string = editor.getValue();
	let version = (document.getElementById("ghcversionselect") as any).value;
	const payload: string = JSON.stringify({code: source, version});

	performXHR(
		"POST", "/save", "json",
		response => {
			if (typeof response != "string") {
				alert("Invalid response returned by server: " + response);
				return;
			}

			history.pushState(null, "", location.origin + "/saved/" + response);
			alert("Saved! You can share the URL of this page.");
		},
		xhr => {
			alert("Could not save your code!\nServer returned status code " + xhr.status + ": " + xhr.responseText);
		},
		"application/json", payload
	);
}

function setSeparatorToWidth(wid: number | null) {
	const containerelem = document.getElementById("main");
	if (wid == null) containerelem.style.gridTemplateColumns = "";
	else containerelem.style.gridTemplateColumns =
			"[left-start] " + wid + "px [left-end] 4px [right-start] 1fr [right-end]";
	editor.resize();
}

function handleSeparatorDragEvents() {
	const sepelem = document.getElementById("colseparator");
	const containerelem = document.getElementById("main");
	const leftchild = document.getElementById("leftcol");

	function currentWidth() {
		return leftchild.getBoundingClientRect().width;
	}

	let initmousex: number | null = null;
	let initwidth: number | null = null;

	function movehandler(ev) {
		if (initmousex == null) {cancelhandler(ev); return;}
		const margin = 50;
		let newWidth = initwidth + ev.clientX - initmousex;
		newWidth = Math.max(newWidth, margin);
		newWidth = Math.min(newWidth, containerelem.getBoundingClientRect().width - margin);
		setSeparatorToWidth(newWidth);
	}

	function cancelhandler(ev) {
		initmousex = initwidth = null;
		document.body.style.userSelect = "auto";
		document.body.removeEventListener("mousemove", movehandler);
		document.body.removeEventListener("mouseup", cancelhandler);
		document.body.removeEventListener("mouseleave", cancelhandler);
	}

	sepelem.addEventListener("mousedown", function(ev) {
		initmousex = ev.clientX;
		initwidth = currentWidth();
		setSeparatorToWidth(initwidth);  // should be unnecessary, but eh
		document.body.style.userSelect = "none";

		document.body.addEventListener("mousemove", movehandler);
		document.body.addEventListener("mouseup", cancelhandler);
		document.body.addEventListener("mouseleave", cancelhandler);
	});

	sepelem.addEventListener("dblclick", function(ev) {
		setSeparatorToWidth(null);  // reset to 50%
	});
}

window.addEventListener("load", function() {
	editor.commands.addCommand({
		name: "Run",
		bindKey: {win: "Ctrl-Enter", mac: "Command-Enter"},
		exec: function() {
			doRun(lastRunKind);
		},
		readOnly: true,
	});

	let runTooltip =
			editor.commands.platform == "win"
				? "Press Ctrl-Enter to run again"
				: "Press Cmd-Enter to run again";
	document.getElementById("btn-run").setAttribute("title", runTooltip);
	document.getElementById("btn-core").setAttribute("title", runTooltip);
	document.getElementById("btn-asm").setAttribute("title", runTooltip);

	getVersions(function(versions) {
		const sel: HTMLElement = document.getElementById("ghcversionselect");
		for (let i = 0; i < versions.length; i++) {
			const opt: HTMLOptionElement = document.createElement("option");
			opt.value = versions[i];
			const readable = versions[i] in ghcReadableVersion ? ghcReadableVersion[versions[i]] : versions[i];
			opt.textContent = "GHC " + readable;
			if (versions[i] == defaultGHCversion) opt.setAttribute("selected", "");
			sel.appendChild(opt);
		}
	});

	const sel: HTMLElement = document.getElementById("optselect");
	["O0", "O1", "O2"].forEach(o => {
		const opt: HTMLOptionElement = document.createElement("option");
		opt.value = o;
		opt.textContent = "-" + o;
		if (o == "O1") opt.setAttribute("selected", "");
		sel.appendChild(opt);
	});
});

document.getElementById("btn-run").addEventListener('click', () => { doRun("run") });
document.getElementById("btn-core").addEventListener('click', () => { doRun("core") });
document.getElementById("btn-asm").addEventListener('click', () => { doRun("asm") });
document.getElementById("btn-save").addEventListener('click', () => { doSave() });
handleSeparatorDragEvents();
addMediaListener("screen and (max-width: 800px)", "resize", function(ql) {
	if (ql && ql.matches) setSeparatorToWidth(null);
});

// vim: set noet sw=4 ts=4:
