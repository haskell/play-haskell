import {EditorState, EditorView, basicSetup} from "@codemirror/basic-setup";
import {javascript} from "@codemirror/lang-javascript";

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
  let toleft []      = ([], [])
      toleft (x:xs)  = first (x :) (toright xs)
      toright []     = ([], [])
      toright (x:xs) = second (x :) (toright xs)
      (l1, l2) = toleft l
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

// defined in a <script> block in play.mustache
declare var preload_script: string | null;
const snippet = preload_script != null ? preload_script : example_snippets[Math.floor(Math.random() * 3)];

const state = EditorState.create({doc: snippet, extensions: [
  basicSetup,
  javascript(),
]});
(window as any).view = new EditorView({state, parent: document.getElementById("leftcol")!});

let currentChallenge: string | null = null;

type json =
| string
| number
| boolean
| null
| json[]
| {[key: string]: json}

enum Runner {
	Run = 0,
	Core = 1,
	Asm = 2
}


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
		rightcol.classList.add("loading");
	} else {
		btns.forEach(btn => {
			btn.removeAttribute("disabled");
		});
		rightcol.classList.remove("loading");
	}
}

function getVersions(cb: (response: string) => void) {
	performXHR("GET", "/play/versions", "json", cb, function(xhr) {
		alert("Error getting available compiler versions (status " + xhr.status + "): " + xhr.responseText);
	});
}

function refreshChallenge(cb?: () => void) {
	performXHR("GET", "/play/challenge", "text",
		function(challenge) {
			if (typeof challenge == "string") {
				currentChallenge = challenge;
				if (cb) cb();
			} else {
				alert("Error getting challenge for submitting run requests (not a string: " + challenge + ")");
			}
		}, function(xhr) {
			alert("Error getting challenge for submitting run requests (status " + xhr.status + "): " + xhr.responseText);
		}
	);
}

function sendRun(source: string, version: string, opt: string, run: Runner, cb: (response: json) => void, retryChallenge: boolean = true) {
	const payload: string = JSON.stringify({challenge: currentChallenge, source, version, opt});
	setWorking(true);
	let ep: string = null;
	switch (run) {
		case Runner.Run:
			ep = "/play/run"
			break;
		case Runner.Core:
			ep = "/play/core"
			break;
		case Runner.Asm:
			ep = "/play/asm"
			break;
	}
	performXHR("POST", ep, "json",
		function(res: json) {
			setWorking(false);
			cb(res);
		}, function(xhr) {
			if (retryChallenge) {
				refreshChallenge(function() {
					sendRun(source, version, opt, run, cb, false);
				});
			} else {
				setWorking(false);
				alert("Failed to submit run job (status " + xhr.status + "): " + xhr.responseText);
			}
		}, "text/plain", payload
	);
}

function doRun(run: Runner) {
	const source: string = (window as any).view.state.doc.toString();
	let version = (document.getElementById("ghcversionselect") as any).value;
	let opt = (document.getElementById("optselect") as any).value;
	if (typeof version != "string" || version == "") version = "8.10.7";
	if (typeof opt != "string" || opt == "") opt = "O1";

	sendRun(source, version, opt, run, function(response: {[key: string]: json}) {
		const ecNote: HTMLElement = document.getElementById("exitcode-note");
		if (response.ec === -1) {
			ecNote.classList.remove("invisible");
			ecNote.textContent = "Command timed out :(";
		} else if (response.ec != 0) {
			ecNote.classList.remove("invisible");
			ecNote.textContent = "Command exited with code " + response.ec + ".";
		} else {
			ecNote.classList.add("invisible");
		}

		document.getElementById("out-stdout").textContent = response.out as string;
		document.getElementById("out-stderr").textContent = response.err as string;
	});
}

function doOpenAsPaste() {
	const form = document.createElement("form");
	form.action = "/";
	form.method = "POST";
	form.target = "_blank";
	form.setAttribute("style", "display: none");

	const input = document.createElement("input");
	input.type = "text";
	input.name = "name1";
	input.value = "Main.hs";
	form.appendChild(input);

	const textarea = document.createElement("textarea");
	textarea.name = "code1";
	textarea.value = (window as any).view.state.doc.toString();
	form.appendChild(textarea);

	document.body.appendChild(form);
	form.submit();
	document.body.removeChild(form);
}

window.addEventListener("load", function() {
	// // This is broken with the codemirror editor, of course, which rebinds
	// // ctrl-enter to "insert blank line".
	// // Note, if this is reinstated, please add a title= tooltip to the
	// // relevant buttons in the html
	// document.getElementById("editor").addEventListener("keypress", function(ev) {
	//     if ((ev.key == "Enter" || ev.keyCode == 13) && ev.ctrlKey) {
	//         doRun();
	//     }
	// });

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

	refreshChallenge();
	setInterval(refreshChallenge, 12 * 3600 * 1000);  // once per half a day (i.e. half the server refresh interval)

	const sel: HTMLElement = document.getElementById("optselect");
	["O0", "O1", "O2"].forEach(o => {
		const opt: HTMLOptionElement = document.createElement("option");
		opt.value = o;
		opt.textContent = "-" + o;
		if (o == "O1") opt.setAttribute("selected", "");
		sel.appendChild(opt);
	});
});

document.getElementById("btn-run").addEventListener('click', () => { doRun(Runner.Run) });
document.getElementById("btn-core").addEventListener('click', () => { doRun(Runner.Core) });
document.getElementById("btn-asm").addEventListener('click', () => { doRun(Runner.Asm) });
document.getElementById("btn-openaspaste").addEventListener('click', () => { doOpenAsPaste() });
