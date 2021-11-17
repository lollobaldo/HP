"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.activate = void 0;
const vscode = require("vscode");
const fs = require("fs");
const path = require("path");
const cp = require("child_process");
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
const invalidateCache = async (filePath) => {
    let data = await readFile(filePath);
    console.log(data[0]);
    if (data[0] === '\n')
        data = data.substring(1);
    else
        data = '\n' + data;
    console.log(data[0]);
    try {
        return fs.promises.writeFile(filePath, data, 'utf8');
    }
    catch (err) {
        console.error('Error occured while writing back file!', err);
    }
};
function activate(context) {
    const generateHtml = async (dir, cwd, forceRefreshPath = '') => {
        if (forceRefreshPath) {
            console.log(`invalidating ${forceRefreshPath}`);
            await invalidateCache(forceRefreshPath);
        }
        const cmd = `cabal run MainDisplay --ghc-options=-i${dir}`.replace(/\\/g, "\/");
        console.log(cmd);
        try {
            console.log(cp.execSync(cmd, { cwd }).toString());
        }
        catch (err) {
            console.log(err);
        }
        const data = await readFile(path.join(context.extensionPath, 'interactive-map', 'out1.html'));
        return data;
    };
    context.subscriptions.push(vscode.commands.registerCommand('catCoding.start', async () => {
        if (!vscode.window.activeTextEditor) {
            return;
        }
        const editor = vscode.window.activeTextEditor;
        const document = editor.document;
        const filename = editor.document.fileName;
        let { name, dir } = path.parse(filename);
        if (dir[1] === ':')
            dir = dir.replace(dir[0], dir[0].toUpperCase());
        const cwd = path.join(context.extensionPath, 'interactive-map').replace(/\\/g, "\/");
        const tempSettingPath = path.join(context.extensionPath, 'interactive-map', '.ghci.template');
        const injeSettingPath = path.join(context.extensionPath, 'interactive-map', '.ghci');
        await replaceInFile(tempSettingPath, injeSettingPath, [["###REPLACE WITH DIRECTORY OF PROJECT###", dir]]);
        const tempDispPath = path.join(context.extensionPath, 'interactive-map', 'MainDisplay.hs.template');
        const injeDispPath = path.join(context.extensionPath, 'interactive-map', 'MainDisplay.hs');
        const wordRange = editor.document.getWordRangeAtPosition(editor.selection.start);
        const highlight = editor.document.getText(wordRange);
        await replaceInFile(tempDispPath, injeDispPath, [
            ["###REPLACE WITH NAME OF MODULE###", name],
            ["###REPLACE WITH IDENTIFIER OF EXPRESSION###", highlight]
        ]);
        console.log(highlight);
        const line = editor.selection.active.line;
        const inset = vscode.window.createWebviewTextEditorInset(vscode.window.activeTextEditor, line - 1, 12, { localResourceRoots: [vscode.Uri.file(context.extensionPath)], enableScripts: true, });
        inset.webview.onDidReceiveMessage(async (message) => {
            console.log(message);
            vscode.window.showErrorMessage(message.id);
            const { key, value, isRemove } = message;
            const exp = isRemove ? 'Nothing' : `Just ${value}`;
            console.log(isRemove, exp);
            const tempEditPath = path.join(context.extensionPath, 'interactive-map', 'MainEdit.hs.template');
            const injeEditPath = path.join(context.extensionPath, 'interactive-map', 'MainEdit.hs');
            await replaceInFile(tempEditPath, injeEditPath, [
                ["###REPLACE WITH NAME OF MODULE###", name],
                ["###REPLACE WITH IDENTIFIER OF EXPRESSION###", highlight],
                ["###REPLACE WITH KEY###", key],
                ["###REPLACE WITH VALUE###", exp]
            ]);
            const cmd = `cabal run MainEdit --ghc-options=-i${dir}`.replace(/\\/g, "\/");
            console.log(cmd);
            let result = "";
            try {
                result = cp.execSync(cmd, { cwd }).toString();
            }
            catch (err) {
                console.log(err);
                return;
            }
            console.log(result);
            const mid = result.split('\n');
            const newValue = mid[mid.length - 2];
            var startposition = new vscode.Position(line, 0);
            var endingposition = new vscode.Position(line + 1, 0);
            var range = new vscode.Range(startposition, endingposition);
            editor.edit(editBuilder => {
                editBuilder.replace(range, `${highlight} = ${newValue}\n`);
            });
            await document.save();
            inset.webview.html = await generateHtml(dir, cwd, injeDispPath);
            return;
        }, undefined, context.subscriptions);
        inset.onDidDispose(() => {
            console.log('WEBVIEW disposed...:(');
        });
        inset.webview.html = await generateHtml(dir, cwd, injeDispPath);
    }));
}
exports.activate = activate;
//# sourceMappingURL=extension.js.map