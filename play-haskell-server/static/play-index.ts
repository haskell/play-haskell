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

// If a version is not present in this dictionary, just display it as-is
const ghcReadableVersion: Record<string, string> = {
	"9.6.0.20230111": "9.6.1-alpha1",
	"9.6.0.20230128": "9.6.1-alpha2",
	"9.6.0.20230210": "9.6.1-alpha3",
	"9.6.0.20230302": "9.6.1-rc1",
};

// defined in a <script> block in play.mustache
declare var preload_script: string;

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
editor.session.setValue(preload_script);
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


class UnloadHandler {
	ignoreChanges;
	bufferSaved;
	_handlerSetup;

	constructor() {
		this.ignoreChanges = false;  // set to true while making changes that do not dirty the buffer
		this.bufferSaved = false;  // set to true when saving the buffer so that a dialog is only shown after dirtying the buffer again

		this._handlerSetup = false;

		// We only attach the beforeunload handler once a change is actually
		// made to ensure that back/forward caching still works if the user
		// didn't change anythingin the buffer (see MDN on beforeunload).
		editor.getSession().on("change", e => {
			if (this.ignoreChanges) return;
			if (this.bufferSaved) this.bufferSaved = false;
			if (this._handlerSetup) return;
			window.addEventListener("beforeunload", unloadE => {
				if (this.bufferSaved) return;  // if saved, buffer is not dirty
				unloadE.preventDefault();  // this triggers the dialog
				unloadE.returnValue = true;  // same, but for some old browsers
			});
			this._handlerSetup = true;
		});
	}
}

let gUnloadHandler = null;  // initialised in "load" handler


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

function renderGHCout(elem: Node, out: string) {
	elem.textContent = "";

	const re = /^(.*: [^ ]*: \[)(GHC-[0-9]+)\]/m;
	let cursor = 0;
	let m;
	while ((m = out.substr(cursor).match(re)) != null) {
		const prelen = m.index + m[1].length;
		elem.appendChild(document.createTextNode(out.substr(cursor, prelen)));

		const anchor = document.createElement("a");
		anchor.href = "https://errors.haskell.org/messages/" + m[2];
		anchor.target = "_blank";
		anchor.appendChild(document.createTextNode(m[2]));
		elem.appendChild(anchor);

		cursor += prelen + m[2].length;
		// Note that we haven't added the closing ']' yet. That's fine; the
		// next iteration (or the final part below the loop) will do that.
	}

	if (cursor < out.length) {
		elem.appendChild(document.createTextNode(out.substr(cursor)));
	}
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

		if (response.ghcout) renderGHCout(document.getElementById("out-ghcout"), response.ghcout as string);
		if (response.sout) document.getElementById("out-stdout").textContent = response.sout as string;
		if (response.serr) document.getElementById("out-stderr").textContent = response.serr as string;
	});

	// Immediately refocus the editor (after synchronous JS has finished).
	// One might think it's best to only focus the editor here if it was
	// previously focused before clicking the Run button. However, that doesn't
	// help for Tridactyl users, because they need to unfocus the editor before
	// being able to click the Run button. Thus let's just focus the editor
	// unconditionally. I can't think of a downside.
	setTimeout(() => editor.focus(), 0);
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

			// Mark the buffer as saved so the back button doesn't warn until
			// the buffer is changed again
			gUnloadHandler.bufferSaved = true;

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

function doShowHelpDialog() {
	try {
		const dialog = document.getElementById("help-alert") as HTMLDialogElement;
		dialog.showModal();
		document.getElementById("btn-close-help-alert").focus();
	} catch (e) {
		alert("https://github.com/haskell/play-haskell/blob/9b2355fe740a7cfb87bfad013800a7b5a0c912ea/play-haskell-server/play.mustache#L434-L458");
	}
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
			editor.commands.platform == "mac"
				? "Press Cmd-Enter to run again"
				: "Press Ctrl-Enter to run again";
	// document.getElementById("btn-run").setAttribute("title", runTooltip);
	// document.getElementById("btn-core").setAttribute("title", runTooltip);
	// document.getElementById("btn-asm").setAttribute("title", runTooltip);

	if (editor.commands.platform == "mac") {
		const l = document.getElementsByClassName("ui-ctrl-cmd");
		for (let i = 0; i < l.length; i++) l[i].innerHTML = "Cmd";
	}
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

	gUnloadHandler = new UnloadHandler();

	const btnBasicTemplate = document.getElementById("btn-basic-template");
	let completeFadeout = null;
	uponUserAction(() => {
		completeFadeout = setupButtonFadeout(btnBasicTemplate, 2000, 1500);
	});
	document.getElementById("btn-basic-template").addEventListener('click', () => {
		// Ensure that this change does not get picked up as a user change to be saved
		gUnloadHandler.ignoreChanges = true;
		editor.session.setValue("main :: IO ()\nmain = _");
		gUnloadHandler.ignoreChanges = false;

		if (completeFadeout != null) completeFadeout();
		else btnBasicTemplate.classList.add("hidden");
		editor.focus();
		editor.gotoLine(2, 8);
	});

	editor.focus();
});

document.getElementById("btn-yolc").addEventListener('click', () => { doRun("run") });
// document.getElementById("btn-run").addEventListener('click', () => { doRun("run") });
// document.getElementById("btn-core").addEventListener('click', () => { doRun("core") });
// document.getElementById("btn-asm").addEventListener('click', () => { doRun("asm") });
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
document.getElementById("btn-close-help-alert").addEventListener('click', () => {
	(document.getElementById("help-alert") as HTMLDialogElement).close();
});
handleSeparatorDragEvents();
addMediaListener("screen and (max-width: 800px)", "resize", function(ql) {
	if (ql && ql.matches) setSeparatorToWidth(null);
});

// vim: set noet sw=4 ts=4:
