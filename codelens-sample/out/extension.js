"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.activate = void 0;
const vscode = require("vscode");
const path = require("path");
const repljs_1 = require("./repljs");
const utils_1 = require("./utils");
const Visual_1 = require("./Visual");
function activate(context) {
    let _ghciInstance;
    let _activeCwd = '';
    const visuals = {};
    const getGhci = async () => {
        const editor = vscode.window.activeTextEditor;
        if (!editor)
            throw "No editor is active.";
        const filename = editor.document.fileName;
        let dir = path.parse(filename).dir;
        // Fix for Windows uppercase requirement for drive letters
        if (dir[1] === ':')
            dir = dir.replace(dir[0], dir[0].toUpperCase());
        const cwd = path.join(context.extensionPath, 'interactive-map').replace(/\\/g, "\/");
        console.log(_ghciInstance, cwd, _activeCwd);
        if (!_ghciInstance || cwd !== _activeCwd) {
            const cmd = `cabal repl Main --ghc-options=-i${dir}`.replace(/\\/g, "\/");
            console.log(cmd, cwd);
            _ghciInstance = new repljs_1.InteractiveProcessHandle(cmd, [], { cwd });
            await _ghciInstance.call('');
            _activeCwd = cwd;
        }
        return _ghciInstance;
    };
    context.subscriptions.push(vscode.workspace.onDidSaveTextDocument((document) => {
        console.log("refreshing");
        console.log(visuals);
        const documentId = document.uri.toString(true);
        console.log(documentId);
        if (documentId in visuals) {
            for (const identifier in visuals[documentId]) {
                console.info("Refreshing: ", identifier);
                visuals[documentId][identifier].refreshHtml();
            }
        }
    }), vscode.commands.registerCommand('visualise.identifier', async () => {
        if (!vscode.window.activeTextEditor) {
            return;
        }
        const ghciInstancePromise = getGhci();
        const editor = vscode.window.activeTextEditor;
        const document = editor.document;
        const documentId = document.uri.toString(true);
        if (!(documentId in visuals))
            visuals[documentId] = {};
        const progressNotification = (0, utils_1.showProgress)();
        const filename = editor.document.fileName;
        let { dir } = path.parse(filename);
        if (dir[1] === ':')
            dir = dir.replace(dir[0], dir[0].toUpperCase());
        (0, utils_1.injectFileName)(context);
        const wordRange = editor.document.getWordRangeAtPosition(editor.selection.start);
        const identifier = editor.document.getText(wordRange);
        // console.log("Identifier: ", identifier);
        const line = editor.selection.active.line;
        const visual = await Visual_1.Visual.newVisual(context, ghciInstancePromise, identifier, line);
        visuals[documentId][identifier] = visual;
        progressNotification.end();
    }));
}
exports.activate = activate;
//# sourceMappingURL=extension.js.map