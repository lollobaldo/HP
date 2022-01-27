"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.Visual = void 0;
const vscode = require("vscode");
const utils_1 = require("./utils");
class Visual {
    constructor(context, ghciPromise, identifier, line) {
        if (!vscode.window.activeTextEditor)
            throw "No editor is active.";
        this.ghciPromise = ghciPromise;
        this.identifier = identifier;
        this.line = line;
        this.inset = vscode.window.createWebviewTextEditorInset(vscode.window.activeTextEditor, line - 1, 12, { localResourceRoots: [vscode.Uri.file(context.extensionPath)], enableScripts: true, });
        this.inset.webview.html = loadingPage;
        this.inset.webview.onDidReceiveMessage(async (message) => {
            console.log(message);
            if (!vscode.window.activeTextEditor) {
                throw "No editor is active.";
            }
            const progressNotification = (0, utils_1.showProgress)();
            if (message.refresh) {
                await this.refreshHtml();
                progressNotification.end();
                return;
            }
            ;
            await this.crudAction(message);
            progressNotification.end();
            return;
        }, undefined, context.subscriptions);
        this.inset.onDidDispose(() => {
            console.log('WEBVIEW disposed...:(');
        });
    }
    static async newVisual(context, ghciPromise, identifier, line) {
        const a = new Visual(context, ghciPromise, identifier, line);
        await a.refreshHtml();
        return a;
    }
    async refreshHtml() {
        const ghciInstance = await this.ghciPromise;
        const load = await ghciInstance.call(':l Main');
        console.log("load:", load);
        const response = await ghciInstance.call(`graph File.${this.identifier}`);
        // console.log("WWWWW" + response);
        try {
            const parsed = JSON.parse(response);
            // console.debug(parsed);
            this.inset.webview.html = parsed.html;
        }
        catch (e) {
            console.error(e);
            console.error(response);
        }
        ;
    }
    async crudAction(message) {
        if (!vscode.window.activeTextEditor)
            throw "No editor is active.";
        const { key, value, opType } = message;
        const exp = opType == 'Delete' ? 'Nothing' : `Just ${value}`;
        const ghciInstance = await this.ghciPromise;
        await ghciInstance.call(':l Main');
        const response = await ghciInstance.call(`edit (${opType}) [(${key})] (${exp}) (File.${this.identifier}) `);
        const parsed = JSON.parse(response);
        // console.info(parsed);
        var startposition = new vscode.Position(this.line, 0);
        var endingposition = new vscode.Position(this.line + 1, 0);
        var range = new vscode.Range(startposition, endingposition);
        vscode.window.activeTextEditor.edit(editBuilder => {
            editBuilder.replace(range, `${this.identifier} = ${parsed.code}\n`);
        });
        await vscode.window.activeTextEditor.document.save();
        this.refreshHtml();
    }
}
exports.Visual = Visual;
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
//# sourceMappingURL=Visual.js.map