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
                   in quicksort lesser ++ [x] ++ quicksort greater`
,
`{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Time as Time

data Visitor
  = Member Profile
  | NonMember (Maybe T.Text)
  deriving Show

data Profile =
  Profile
    { name :: T.Text
    , birthday :: Time.Day
    } deriving Show

main :: IO ()
main = do
  let haskell = Member Profile
        { name = "Haskell Curry"
        , birthday = read "1900-09-12"
        }
  greeting <- makeGreeting haskell
  putStrLn $ T.unpack greeting

makeGreeting :: Visitor -> IO T.Text
makeGreeting visitor =
  case visitor of
    NonMember maybeName ->
      pure $ case maybeName of
        Just name -> "Hello, " <> name <> "!"
        Nothing   -> "Hello, mysterious visitor!"
    Member profile -> do
      today <- Time.utctDay <$> Time.getCurrentTime
      let monthAndDay = (\\(_y, m, d) -> (m, d)) . Time.toGregorian
      if monthAndDay today == monthAndDay (birthday profile)
      then pure $ "Happy birthday, " <> name profile <> "!"
      else pure $ "Welcome back, " <> name profile <> "!"`
,
`import Control.Monad (replicateM)
import Data.Foldable (foldl')
import qualified System.Random.Stateful as Rand

data Drone = Drone
  { xPos :: Int
  , yPos :: Int
  , zPos :: Int
  } deriving Show

data Movement
  = Forward | Back | ToLeft | ToRight | Up | Down
  deriving (Show, Enum, Bounded)

main :: IO ()
main = do
  let initDrone = Drone { xPos = 0, yPos = 100, zPos = 0 }
  -- Generate 15 moves randomly
  randomMoves <- replicateM 15 $ Rand.uniformEnumM Rand.globalStdGen
  let resultDrone = foldl' moveDrone initDrone randomMoves
  print resultDrone

moveDrone :: Drone -> Movement -> Drone
moveDrone drone move =
  case move of
    Forward -> drone { zPos = zPos drone + 1 }
    Back    -> drone { zPos = zPos drone - 1 }
    ToLeft  -> drone { xPos = xPos drone - 1 }
    ToRight -> drone { xPos = xPos drone + 1 }
    Up      -> drone { yPos = yPos drone + 1 }
    Down    -> drone { yPos = yPos drone - 1 }`
,
// adapted from @liamzee's https://github.com/haskript/big-book-of-small-haskell-projects/blob/51fd3ac4db30e9df2f14924d66d1469638aed009/35.HexGrid/HexGrid.hs
`main :: IO ()
main = putStr $ unlines $ hexagons 12 17

hexagons :: Int -> Int -> [String]
hexagons xRepeat yRepeat =
  yRepeat \`times\` [xRepeat \`times\` "/ \\\\_"
                  ,xRepeat \`times\` "\\\\_/ "]
  where
    n \`times\` l = concat (replicate n l)`
];

// If a version is not present in this dictionary, just display it as-is
const ghcReadableVersion: Record<string, string> = {
	"9.6.0.20230111": "9.6.1-alpha1",
	"9.6.0.20230128": "9.6.1-alpha2",
	"9.6.0.20230210": "9.6.1-alpha3",
	"9.6.0.20230302": "9.6.1-rc1",
};

// defined in a <script> block in play.mustache
declare var preload_script: string | null;
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

const defaultGHCversion: string = "9.4.8";


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

function showSaveDialog(saveUrl) {
	const dialog = document.getElementById("save-alert") as HTMLDialogElement;
	dialog.showModal();
	document.getElementById("save-link-slot").innerText = saveUrl;

	if (!("clipboard" in navigator && navigator.clipboard != undefined)) {
		dialog.classList.add("no-clipboard-support");
	} else {
		dialog.classList.remove("no-clipboard-support");

		const copyLinkButton = document.getElementById("btn-copy-link");
		copyLinkButton.classList.remove("success");
		copyLinkButton.onclick = ev => {
			navigator.clipboard.writeText(saveUrl)
				.then(() => {
					copyLinkButton.classList.add("success");
					// This needs to be in a timeout to work?
					setTimeout(() => document.getElementById("btn-close-save-alert").focus(), 0);
				}, () => {
					alert("Clipboard set failed");
				});
		};
	}
}

function doSave() {
	const source: string = editor.getValue();

	performXHR(
		"POST", "/save", "text",
		response => {
			if (typeof response !== "string") {
				alert("Invalid response returned by server: " + response);
				return;
			}

			const saveUrl = `${location.origin}/saved/${response}`;
			history.pushState(null, "", saveUrl);

			let el;
			if (el = document.getElementById("paste-save-time")) el.innerText = new Date().toLocaleString();
			if (el = document.getElementById("paste-raw-link") as HTMLAnchorElement) el.href = saveUrl + "/raw";

			try {
				showSaveDialog(saveUrl);
			} catch (e) {
				alert(`Saved here:\n${saveUrl}\nCopy this link to share your snippet. Your browser's URL bar was also updated.`);
			}
		},
		xhr => {
			alert("Could not save your code!\nServer returned status code " + xhr.status + ": " + xhr.responseText);
		},
		"text/plain", source
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

// Assumes that the element has 'transition: opacity <fadetime>ms'. Assumes
// .fadeout results in 'opacity: 0' and that .hidden results in
// 'display: hidden'.
// Returns a function that hides the element immediately, cancelling the active fadeout.
function setupButtonFadeout(btn, delayms, fadetimems): () => void {
	function fullhide() {
		btn.classList.add("hidden");
		btn.classList.remove("fadeout");
	}

	let timeout = setTimeout(() => {
		btn.classList.add("fadeout");
		timeout = setTimeout(() => {fullhide(); timeout = null;}, fadetimems + 50);
	}, delayms);

	return () => {
		if (timeout != null) clearTimeout(timeout);
		fullhide();
	};
}

// Upon the next user action (mouse move, key press), run the function once.
function uponUserAction(fun: () => void) {
	let mouseh = false, keyh = false;

	function run() {
		fun();
		if (mouseh) { window.removeEventListener("mousemove", fun); mouseh = false; }
		if (keyh) { window.removeEventListener("keydown", fun); keyh = false; }
	}

	window.addEventListener("mousemove", run); mouseh = true;
	window.addEventListener("keydown", run); keyh = true;
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
		for (let i = versions.length - 1; i >= 0; i--) {
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

	let currentlyChangingAutomatically = false;
	let unloadHandlerSetup = false;
	editor.getSession().on("change", e => {
		if (currentlyChangingAutomatically) return;  // only user changes are important
		if (unloadHandlerSetup) return;
		window.addEventListener("beforeunload", unloadE => {
			unloadE.preventDefault();  // this triggers the dialog
			unloadE.returnValue = true;  // same, but for some old browsers
		});
		unloadHandlerSetup = true;
	});

	const btnBasicTemplate = document.getElementById("btn-basic-template");
	let completeFadeout = null;
	uponUserAction(() => {
		completeFadeout = setupButtonFadeout(btnBasicTemplate, 2000, 1500);
	});
	document.getElementById("btn-basic-template").addEventListener('click', () => {
		// Ensure that this change does not get picked up as a user change to be saved
		currentlyChangingAutomatically = true;
		editor.session.setValue("main :: IO ()\nmain = _");
		currentlyChangingAutomatically = false;

		if (completeFadeout != null) completeFadeout();
		else btnBasicTemplate.classList.add("hidden");
		editor.focus();
		editor.gotoLine(2, 8);
	});

	editor.focus();
});

document.getElementById("btn-run").addEventListener('click', () => { doRun("run") });
document.getElementById("btn-core").addEventListener('click', () => { doRun("core") });
document.getElementById("btn-asm").addEventListener('click', () => { doRun("asm") });
document.getElementById("btn-save").addEventListener('click', () => { doSave() });
document.getElementById("btn-close-save-alert").addEventListener('click', () => {
	(document.getElementById("save-alert") as HTMLDialogElement).close();
});
if ("getSelection" in window) {
	document.getElementById("save-link-slot").addEventListener('click', ev => {
		// Clear any current selection
		const selection = window.getSelection();
		selection.removeAllRanges();

		// Select the URL
		const range = document.createRange();
		range.selectNodeContents(document.getElementById("save-link-slot"));
		selection.addRange(range);
	});
}
handleSeparatorDragEvents();
addMediaListener("screen and (max-width: 800px)", "resize", function(ql) {
	if (ql && ql.matches) setSeparatorToWidth(null);
});

// vim: set noet sw=4 ts=4:
