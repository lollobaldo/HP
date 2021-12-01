"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.injectFileName = exports.replaceInFile = exports.readFile = exports.showProgress = void 0;
const vscode = require("vscode");
const fs = require("fs");
const path = require("path");
const showProgress = () => {
    let ret;
    const promise = new Promise(resolve => {
        ret = resolve;
    });
    vscode.window.withProgress({
        location: vscode.ProgressLocation.Notification,
        title: "Working on it! May take some time...",
        cancellable: false
    }, (progress, token) => promise);
    return { end: ret };
};
exports.showProgress = showProgress;
const readFile = async (filePath) => {
    try {
        return fs.promises.readFile(filePath, 'utf8');
    }
    catch (err) {
        console.error('Error occured while reading file!', err);
    }
    return "";
};
exports.readFile = readFile;
const replaceInFile = async (templatePath, filePath, reps) => {
    let data = await (0, exports.readFile)(templatePath);
    for (const [s1, s2] of reps) {
        data = data.replace(s1, s2);
    }
    console.log(data);
    try {
        return fs.promises.writeFile(filePath, data, 'utf8');
    }
    catch (err) {
        console.error('Error occured while writing back file!', err);
    }
};
exports.replaceInFile = replaceInFile;
const injectFileName = async (context) => {
    if (!vscode.window.activeTextEditor) {
        throw "No editor is active.";
    }
    const filename = vscode.window.activeTextEditor.document.fileName;
    let { name } = path.parse(filename);
    const tempDispPath = path.join(context.extensionPath, 'interactive-map', 'templates', 'Main.template.hs');
    const injeDispPath = path.join(context.extensionPath, 'interactive-map', 'Main.hs');
    await (0, exports.replaceInFile)(tempDispPath, injeDispPath, [
        ["###REPLACE WITH NAME OF MODULE###", name],
    ]);
};
exports.injectFileName = injectFileName;
const generateHtml = async (ghciInstancePromise, identifier) => {
    const ghciInstance = await ghciInstancePromise;
    const load = await ghciInstance.call(':l Main');
    console.log("load:", load);
    const dat = await ghciInstance.call(`graph File.${identifier}`);
    console.log(dat, JSON.parse(dat));
    try {
        console.log(JSON.parse(dat));
    }
    catch (e) {
        console.log(e);
    }
    ;
    return JSON.parse(dat);
};
const loadingPage = '\
<html><head>\
<style>\
.loader {\
  border: 16px solid #f3f3f3; /* Light grey */\
  border-top: 16px solid #3498db; /* Blue */\
  border-radius: 50%;\
  width: 120px;\
  height: 120px;\
  animation: spin 2s linear infinite;\
}\
\
@keyframes spin {\
  0% { transform: rotate(0deg); }\
  100% { transform: rotate(360deg); }\
}\
</style>\
<body><div class="loader"></div>\
';
//# sourceMappingURL=utils.js.map