import * as vscode from 'vscode';
import * as fs from 'fs';
import * as path from 'path';
import * as cp from 'child_process';

import { InteractiveProcessHandle } from './repljs';

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

export function activate(context: vscode.ExtensionContext) {
  let _ghciInstance: InteractiveProcessHandle;
  let _activeCwd = '';
  
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
    vscode.commands.registerCommand('catCoding.waffle', async () => {
      const ghciInstance = await getGhci();
      const d = await ghciInstance.call(':r');
      console.log(d);
      return;
    }),
    vscode.commands.registerCommand('catCoding.start', async () => {
      if (!vscode.window.activeTextEditor) {
        return;
      }
      const ghciInstancePromise = getGhci();
      const editor = vscode.window.activeTextEditor;
      const document = editor.document;
      
      const progressNotification = showProgress();

      const filename = editor.document.fileName;
      let { name, dir } = path.parse(filename);
      if (dir[1] === ':') dir = dir.replace(dir[0], dir[0].toUpperCase());

      const tempDispPath = path.join(context.extensionPath, 'interactive-map', 'templates', 'Main.template.hs');
      const injeDispPath = path.join(context.extensionPath, 'interactive-map', 'Main.hs');
      
      const wordRange = editor.document.getWordRangeAtPosition(editor.selection.start);
      const highlight = editor.document.getText(wordRange);
      await replaceInFile(tempDispPath, injeDispPath, [
        ["###REPLACE WITH NAME OF MODULE###", name],
      ]);

      console.log(highlight);

      const line = editor.selection.active.line;
      const inset = vscode.window.createWebviewTextEditorInset(
          vscode.window.activeTextEditor, line-1, 12,
          { localResourceRoots: [ vscode.Uri.file(context.extensionPath) ], enableScripts: true, }
      );
      inset.webview.onDidReceiveMessage(
        async message => {
          console.log(message);
          const progressNotification = showProgress();
          
          if (!message.refresh) {
            inset.webview.html = (await generateHtml(ghciInstancePromise, highlight)).html;
            progressNotification.end();
            return;
          };

          const { key, value, isRemove } = message;
          
          const exp = isRemove ? 'Nothing' : `Just ${value}`;
          console.log(isRemove, exp);

          const ghciInstance = await ghciInstancePromise;
          await ghciInstance.call(':l Main');
          const result = JSON.parse(await ghciInstance.call(`edit (File.${highlight}) (${key}) (${exp})`));
          console.log("RESULT:");
          console.log(result);
          var startposition = new vscode.Position(line,0);
          var endingposition = new vscode.Position(line+1,0);
          var range = new vscode.Range(startposition,endingposition);
          editor.edit(editBuilder => {
            editBuilder.replace(range, `${highlight} = ${result.code}\n`);
          });
          await document.save();

          inset.webview.html = (await generateHtml(ghciInstancePromise, highlight)).html;
          progressNotification.end();
          return;
        },
        undefined,
        context.subscriptions
      );
      inset.onDidDispose(() => {
        console.log('WEBVIEW disposed...:(');
      });
      const response = await generateHtml(ghciInstancePromise, highlight);
      console.log(response.info);
      inset.webview.html = response.html;
      progressNotification.end();
    })
  );
}
