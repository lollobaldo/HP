"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.activate = void 0;
const vscode = require("vscode");
const fs = require("fs");
const path = require("path");
const cp = require("child_process");
;
const readFile = async (filePath) => {
    try {
        return fs.promises.readFile(filePath, 'utf8');
    }
    catch (err) {
        console.error('Error occured while reading file!', err);
    }
    return "";
};
const replaceInFile = async (templatePath, filePath, reps) => {
    let data = await readFile(templatePath);
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
function activate(context) {
    // vscode.commands.registerCommand('myExtension.sayHello', () => {
    //   if (!vscode.window.activeTextEditor) {
    //       return;
    //   }
    //   const inset = vscode.window.createWebviewTextEditorInset(vscode.window.activeTextEditor, 2, 10);
    //   inset.onDidDispose(() => {
    //       console.log('WEBVIEW disposed...');
    //   });
    //   fs.readFile(path.join(context.extensionPath,'out1.html'),(err,data) => {
    //     if(err) { console.error(err); }
    //     console.log(data.toString());
    //     inset.webview.html = data.toString();
    //   });
    //   // inset.webview.html = `<head><meta></head><body><img src="https://imgs.xkcd.com/comics/plutonium.png"/><body>`;
    // });
    context.subscriptions.push(vscode.commands.registerCommand('catCoding.start', async () => {
        if (!vscode.window.activeTextEditor) {
            return;
        }
        const editor = vscode.window.activeTextEditor;
        const filename = vscode.window.activeTextEditor.document.fileName;
        const { name, dir } = path.parse(filename);
        console.log(filename, dir, name);
        const tempSettingPath = path.join(context.extensionPath, 'interactive-map', '.ghci.template');
        const injeSettingPath = path.join(context.extensionPath, 'interactive-map', '.ghci');
        await replaceInFile(tempSettingPath, injeSettingPath, [["###REPLACE WITH DIRECTORY OF PROJECT###", dir]]);
        const tempMainPath = path.join(context.extensionPath, 'interactive-map', 'Main.hs.template');
        const injeMainPath = path.join(context.extensionPath, 'interactive-map', 'Main.hs');
        const wordRange = editor.document.getWordRangeAtPosition(editor.selection.start);
        const highlight = editor.document.getText(wordRange);
        await replaceInFile(tempMainPath, injeMainPath, [
            ["###REPLACE WITH NAME OF MODULE###", name],
            ["###REPLACE WITH IDENTIFIER OF EXPRESSION###", highlight]
        ]);
        console.log(highlight);
        // Create and show panel
        const panel = vscode.window.createWebviewPanel('catCoding', 'Cat Coding', vscode.ViewColumn.One, {
            // Enable scripts in the webview
            enableScripts: true
        });
        cp.execSync('cabal run --ghc-options=-i/afs/inf.ed.ac.uk/user/s18/s1853050/UNI2/HP/demo', { cwd: path.join(context.extensionPath, 'interactive-map') });
        const data = await readFile(path.join(context.extensionPath, 'interactive-map', 'out1.html'));
        panel.webview.html = data.toString();
        // panel.webview.html = getWebviewContent();
        // Handle messages from the webview
        panel.webview.onDidReceiveMessage(message => {
            vscode.window.showErrorMessage(message.id);
            return;
        }, undefined, context.subscriptions);
    }));
}
exports.activate = activate;
function getWebviewContent() {
    // return(fs.readFile('out1.html',(err,data) => {
    //   if(err) { console.error(err); }
    //     resultPanel.webview.html = data;
    // }));
    //   return `<!DOCTYPE html>
    // <html lang="en">
    // <head>
    //     <meta charset="UTF-8">
    //     <meta name="viewport" content="width=device-width, initial-scale=1.0">
    //     <title>Cat Coding</title>
    // </head>
    // <body>
    //     <img src="https://media.giphy.com/media/JIX9t2j0ZTN9S/giphy.gif" width="300" />
    // </body>
    // </html>`;
}
// // The module 'vscode' contains the VS Code extensibility API
// // Import the module and reference it with the alias vscode in your code below
// // import { ExtensionContext, commands, window } from 'vscode';
// import * as vscode from 'vscode';
// // this method is called when your extension is activated
// // your extension is activated the very first time the command is executed
// export function activate(context: vscode.ExtensionContext) {
//   vscode.commands.registerCommand('myExtension.sayHello', () => {
//     if (!vscode.window.activeTextEditor) {
//         return;
//     }
//     const inset = vscode.window.createWebviewTextEditorInset(vscode.window.activeTextEditor, 2, 10);
//     inset.onDidDispose(() => {
//         console.log('WEBVIEW disposed...');
//     });
//     inset.webview.html = `<head><meta></head><body><img src="https://imgs.xkcd.com/comics/plutonium.png"/><body>`;
//   });
// }
// function getWebviewContent() {
//     return `<!DOCTYPE html>
//   <html lang="en">
//   <head>
//       <meta charset="UTF-8">
//       <meta name="viewport" content="width=device-width, initial-scale=1.0">
//       <title>Cat Coding</title>
//   </head>
//   <body>
//       <img src="https://media.giphy.com/media/JIX9t2j0ZTN9S/giphy.gif" width="300" />
//   </body>
//   </html>`;
//   }
//# sourceMappingURL=extension.js.map