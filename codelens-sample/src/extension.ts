import * as vscode from 'vscode';
import * as fs from 'fs';
import * as path from 'path';

import { InteractiveProcessHandle } from './repljs';

type Repl = InteractiveProcessHandle;

const showProgress = () => {
  let ret: (value: void | PromiseLike<void>) => void;
  const promise = new Promise<void>(resolve => {
      ret = resolve;
  });
  vscode.window.withProgress({
    location: vscode.ProgressLocation.Notification,
    title: "Working on it! May take some time...",
    cancellable: false
  }, (progress, token) => promise);
  return { end: ret };
};

const readFile = async (filePath: fs.PathLike): Promise<string> => {
  try {
    return fs.promises.readFile(filePath, 'utf8');
  } catch (err) {
    console.error('Error occured while reading file!', err);
  }
  return "";
};

const replaceInFile = async (templatePath: fs.PathLike, filePath: fs.PathLike, reps: [RegExp | string, string][]) => {
  let data = await readFile(templatePath);
  for (const [s1, s2] of reps) {
    data = data.replace(s1, s2);
  }  
  console.log(data);
  try {
    return fs.promises.writeFile(filePath, data, 'utf8');
  } catch (err) {
    console.error('Error occured while writing back file!', err);
  }
};

type Underlier = {
  identifier: string;
  span: number;
  inset: vscode.WebviewEditorInset;
};

export function activate(context: vscode.ExtensionContext) {
  let _ghciInstance: InteractiveProcessHandle;
  let _activeCwd = '';

  const underliers: { [document: string]: { [identifier: string]: Underlier } } = {};

  const getGhci = async () => {
    const editor = vscode.window.activeTextEditor;
    if (!editor) {
      throw "No active editor to use as cwd";
    }

    const filename = editor.document.fileName;
    let dir = path.parse(filename).dir;
    // Fix for Windows uppercase requirement for drive letters
    if (dir[1] === ':') dir = dir.replace(dir[0], dir[0].toUpperCase());
    const cwd = path.join(context.extensionPath, 'interactive-map').replace(/\\/g, "\/");

    console.log(_ghciInstance, cwd, _activeCwd);
    if (!_ghciInstance || cwd !== _activeCwd) {
      const cmd = `cabal repl Main --ghc-options=-i${dir}`.replace(/\\/g, "\/");
      console.log(cmd, cwd);
      _ghciInstance = new InteractiveProcessHandle(cmd, [], { cwd });
      await _ghciInstance.call('');
      _activeCwd = cwd;
    }
    return _ghciInstance;
  };

  const injectFileName = async (editor: vscode.TextEditor) => {
    const filename = editor.document.fileName;
    let { name } = path.parse(filename);
    const tempDispPath = path.join(context.extensionPath, 'interactive-map', 'templates', 'Main.template.hs');
    const injeDispPath = path.join(context.extensionPath, 'interactive-map', 'Main.hs');
    
    await replaceInFile(tempDispPath, injeDispPath, [
      ["###REPLACE WITH NAME OF MODULE###", name],
    ]);
  };

  const generateHtml = async (ghciInstancePromise: Promise<InteractiveProcessHandle>, identifier: string) => {
    const ghciInstance = await ghciInstancePromise;
    const load = await ghciInstance.call(':l Main');
    console.log("load:", load);
    const dat = await ghciInstance.call(`graph File.${identifier}`);
    console.log(dat, JSON.parse(dat));
    try{
      console.log(JSON.parse(dat));
    }catch(e){
      console.log(e)
    };
    return JSON.parse(dat);
  };

  context.subscriptions.push(
    vscode.workspace.onDidSaveTextDocument((document: vscode.TextDocument) => {
      console.log("refreshing");
      const documentId = document.uri.toString(true);
      if (!(documentId in underliers)) {
        const ghciInstancePromise = getGhci();
        for (const identifier in underliers[documentId]) {
          underliers[documentId][identifier].inset.webview.html = loadingPage;
        }
        for (const identifier in underliers[documentId]) {
          underliers[documentId][identifier].inset.webview.html = loadingPage;
        }
      }
    }),
    vscode.commands.registerCommand('visualise.identifier', async () => {
      if (!vscode.window.activeTextEditor) {
        return;
      }
      const ghciInstancePromise = getGhci();
      const editor = vscode.window.activeTextEditor;
      const document = editor.document;
      
      const documentId = document.uri.toString(true);
      if (!(documentId in underliers)) underliers[documentId] = {};

      const progressNotification = showProgress();

      const filename = editor.document.fileName;
      let { dir } = path.parse(filename);
      if (dir[1] === ':') dir = dir.replace(dir[0], dir[0].toUpperCase());

      injectFileName(editor);

      const wordRange = editor.document.getWordRangeAtPosition(editor.selection.start);
      const identifier = editor.document.getText(wordRange);
      console.log("Identifier: ", identifier);

      const line = editor.selection.active.line;
      const inset = vscode.window.createWebviewTextEditorInset(
          vscode.window.activeTextEditor, line-1, 12,
          { localResourceRoots: [ vscode.Uri.file(context.extensionPath) ], enableScripts: true, }
      );
      inset.webview.html = loadingPage;

      underliers[documentId][identifier] = { identifier, span: line, inset}

      inset.webview.onDidReceiveMessage(
        async message => {
          console.log(message);
          const progressNotification = showProgress();
          
          if (message.refresh) {
            inset.webview.html = (await generateHtml(ghciInstancePromise, identifier)).html;
            progressNotification.end();
            return;
          };

          const { key, value, isRemove } = message;
          
          const exp = isRemove ? 'Nothing' : `Just ${value}`;
          console.log(isRemove, exp);

          const ghciInstance = await ghciInstancePromise;
          await ghciInstance.call(':l Main');
          const result = JSON.parse(await ghciInstance.call(`edit (File.${identifier}) (${key}) (${exp})`));
          console.log("RESULT:");
          console.log(result);
          var startposition = new vscode.Position(line,0);
          var endingposition = new vscode.Position(line+1,0);
          var range = new vscode.Range(startposition,endingposition);
          editor.edit(editBuilder => {
            editBuilder.replace(range, `${identifier} = ${result.code}\n`);
          });
          await document.save();

          inset.webview.html = (await generateHtml(ghciInstancePromise, identifier)).html;
          progressNotification.end();
          return;
        },
        undefined,
        context.subscriptions
      );
      inset.onDidDispose(() => {
        console.log('WEBVIEW disposed...:(');
      });
      const response = await generateHtml(ghciInstancePromise, identifier);
      console.log(response.info);
      inset.webview.html = response.html;
      progressNotification.end();
    })
  );
}

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
